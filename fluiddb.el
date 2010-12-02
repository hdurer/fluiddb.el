;;; fluiddb.el --- Code to work with FluidDB
;;
;; Copyright (C) 2009, 2010 Holger Durer
;;
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth floor,
;; Boston, MA 02110-1301, USA.


(require 'url)
(require 'json)
(require 'cl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *fluiddb-credentials* nil
  "Nil (use anonymous access) or a cons of user-name and password strings")

(defvar *fluiddb-server* "fluiddb.fluidinfo.com"
  "The server to use for calls -- either the main instance or sandbox")


(defvar *fluiddb-within-call* nil
  "Helper variable to indicate if we are withing a fluiddb call and thus won't want the authentication mechanisms to kick in")


;; The following disables the interactive request for user name and
;; password should an API call encounter a permission-denied response.
;; This API is meant to be usable without constant asking for username
;; and password.
(defadvice url-http-handle-authentication (around fluiddb-fix)
  (unless *fluiddb-within-call*
      ad-do-it))
(ad-activate 'url-http-handle-authentication)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-escape-string-for-uri (string &optional verbatim-chars)
  "Escape the string to be usable in an URI (doing percent escaping of certain characters.

Newer versions of url.el have the function
browse-url-url-encode-chars for this but if we bring our own,
this will work with Emacs 22 as well.

We escape everything except letters, digits and those in verbatim-chars 
(or - . _ ~ if that second parameter isn't passed) (c.f. section 2.3 of RFC 3986)."
  (let ((verbatim-chars (or verbatim-chars (list ?- ?. ?_ ?~))))
    (with-output-to-string
      (loop for char across string
            do (princ (if (or (and (>= char ?a) (<= char ?z))
                              (and (>= char ?A) (<= char ?Z))
                              (and (>= char ?0) (<= char ?9))
                              (memq char verbatim-chars))
                          (format "%c" char)
                        (format "%%%02X" char)))))))


(defun fluiddb-url-format-namespace-or-tag (tag-or-namespace-name)
  "Helper function to convert the tar or namespace name into a (url-escaped) string ready to be used as part of the path in the request URL.
This is similar to fluiddb-escape-string-for-uri but treats the slash as the path separator which should not be esacped."
  (fluiddb-escape-string-for-uri tag-or-namespace-name (list ?/ ?- ?. ?_ ?~)))

(defun  fluiddb-send-request (method url-extra query-args body accept-value extra-headers)
  "The general purpose helper function to do the actual call to
the FluidDB server"
  (let ((extra-headers extra-headers))
    (save-excursion
      (if *fluiddb-credentials*
          (push (cons "Authorization"
                      (concat "Basic "
                              (base64-encode-string
                               (concat (car *fluiddb-credentials*) ":" (cdr *fluiddb-credentials*)))))
                extra-headers))
      (when query-args
        (loop 
         for first = t then nil
         for (param . value) in query-args
         do (setq url-extra
                  (concat url-extra
                          (if first "?" "&")
                          param
                          "="
                          (fluiddb-escape-string-for-uri value)))))
      (let* ((*fluiddb-within-call* t)
             (url-request-method method)
             (url-http-attempt-keepalives nil)
             (url-mime-accept-string accept-value)
             (url-mime-charset-string nil)
             (url-extensions-header nil)
             (url-request-data body)
             (url-request-extra-headers extra-headers)
             (url (concat "http://" *fluiddb-server* "/" url-extra))
             (buffer (url-retrieve-synchronously url))
             result)
        (switch-to-buffer buffer)
        (setq result (url-http-parse-headers))
        (goto-char (if (re-search-forward "^\r?$" nil 1)
                       (match-beginning 0)
                     (point-max)))
        (move-end-of-line nil)
        (forward-char)
        (let* ((status url-http-response-status)
               (status-ok (and (<= 200 status)
                               (<= status 299)))
               (content-type url-http-content-type)
               (content (buffer-substring (point) (point-max)))
               (error-class (unless status-ok
                              (mail-fetch-field "X-FluidDB-Error-Class")))
               (request-id (unless status-ok
                             (mail-fetch-field "X-FluidDB-Request-Id"))))
          (kill-buffer buffer)
          (if (or (string-equal content-type "application/json")
                  (string-equal content-type "application/vnd.fluiddb.value+json6"))
              (setq content (json-read-from-string content)))
          (if status-ok
              (list status-ok content status content-type)
            (list status-ok content status content-type error-class request-id)))))))


(defun fluiddb-bool-to-json-bool (flag)
  (if flag
      t
    :json-false))


(defun fluiddb-something-to-string (something)
  "Do sensible conversion to a string"
  (typecase something
    (symbol (if (keywordp something)
                (substring (symbol-name something) 1)
              (symbol-name something)))
    (string something)
    (t (format "%s" something))))


(defun fluiddb-make-permission-object (policy exceptions)
  "Return a JSON encoded string representing the passed permission.
Policy should be either open or closed."
  (json-encode-alist
   `(("policy" . ,(fluiddb-something-to-string policy))
     ("exceptions" . ,(mapcar 'fluiddb-something-to-string exceptions)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-user (user-name)
  "Retrieve information about the user with the given name"
  (fluiddb-send-request "GET"
                        (concat "users/" (fluiddb-escape-string-for-uri user-name))
                        nil
                        nil
                        "application/json"
                        nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-object (guid)
  "Retrieve an object by its id"
  (fluiddb-send-request "GET"
                        (concat "objects/" guid)
                        '(("showAbout" . "True"))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-get-object-about (about)
  "Retrieve an object by its about tag"
  (fluiddb-send-request "GET"
                        (concat "about/" (fluiddb-escape-string-for-uri about))
                        nil
                        nil
                        "application/json"
                        nil))


(defun fluiddb-query-objects (query)
  "Perform a query for all object matching"
  (fluiddb-send-request "GET"
                        "objects"
                        `(("query" . ,query))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-create-object (&optional about)
  "Create a new object (anonymous if no about is specified)"
  (fluiddb-send-request "POST"
                        "objects"
                        nil
                        (when about
                          (json-encode-alist `(("about" . ,about))))
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-get-object-tag-value (id tag &optional accept)
  "Retrieve the object's tag value for the object with the given id"
  (fluiddb-send-request "GET"
                        (concat "objects/" id "/" (fluiddb-url-format-namespace-or-tag tag))
                        nil
                        nil
                        (or accept "application/vnd.fluiddb.value+json")                          
                        nil))


(defun fluiddb-get-object-about-tag-value (about tag &optional accept)
  "Retrieve the object's tag value for the object with the given about tag"
  (fluiddb-send-request "GET"
                        (concat "about/" (fluiddb-escape-string-for-uri about)
                                "/" 
                                (fluiddb-url-format-namespace-or-tag tag))
                        nil
                        nil
                        (or accept "application/vnd.fluiddb.value+json")                          
                        nil))


(defun fluiddb-object-tag-has-value-p (id tag)
    "Check if the object with the given id has the specified tag value set.
Returns nil or the mime type of the value."
    (let ((response (fluiddb-send-request "HEAD"
                                          (concat "objects/" id "/" (fluiddb-url-format-namespace-or-tag tag))
                                          nil
                                          nil
                                          "*/*"
                                          nil)))
      (when (first response)
        ;; response is (status-ok-p content status content-type)
        (or (fourth response) t))))


(defun fluiddb-set-object-tag-value (id tag contents &optional content-type)
  "Set the specified tag value on the object with the given id.
Content is either presumed to be pre-formatted (if content-type is given)
or will be JSON encoded and passed as a fluiddb primitive type."
  (fluiddb-send-request "PUT"
                        (concat "objects/" id "/" (fluiddb-url-format-namespace-or-tag tag))
                        nil
                        (if content-type
                            contents
                          (json-encode contents))
                        "*/*"
                        `(("Content-type" . ,(or content-type "application/vnd.fluiddb.value+json")))))


(defun fluiddb-delete-object-tag-value (id tag)
  "Delete the specified tag value on the object with the given id"
  (fluiddb-send-request "DELETE"
                        (concat "object/" id "/" (fluiddb-url-format-namespace-or-tag tag))
                        nil
                        nil
                        "*/*"
                        nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-namespace (namespace)
  "Retrieve information about the given namespace"
  (fluiddb-send-request "GET"
                        (concat "namespaces/" (fluiddb-url-format-namespace-or-tag namespace))
                        '(("returnDescription" . "True")
                          ("returnNamespaces" . "True")
                          ("returnTags" . "True"))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-create-namespace (namespace name description)
  "Create a new sub-namespace within the passed namespace"
  (fluiddb-send-request "POST"
                        (concat "namespaces/" (fluiddb-url-format-namespace-or-tag namespace))
                        nil
                        (json-encode-alist `(("description" . ,description)
                                             ("name" . ,name)))

                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-change-namespace (namespace new-description)
  "Change the description of the given namespace"
  (fluiddb-send-request "PUT"
                        (concat "namespaces/" (fluiddb-url-format-namespace-or-tag namespace))
                        nil
                        (json-encode-alist `(("description" . ,new-description)))
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-delete-namespace (namespace)
  "Delete the given namespace"
  (fluiddb-send-request "DELETE"
                        (concat "namespaces/" (fluiddb-url-format-namespace-or-tag namespace))
                        nil
                        nil
                        "*/*"
                        nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-namespace-permissions (namespace action)
  "Retrieve the permission information for the given namespace and action.
Action can be one of create, update, delete, list, control."
  (fluiddb-send-request "GET"
                        (concat "permissions/namespaces/" (fluiddb-url-format-namespace-or-tag namespace))
                        `(("action" . ,(fluiddb-something-to-string action)))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-set-namespace-permissions (namespace action policy exceptions)
  "Set the permission information for the given namespace and action.
Action can be one of create, update, delete, list, control.
Policy can be either open or closed."
  (fluiddb-send-request "PUT"
                        (concat "permissions/namespaces/" (fluiddb-url-format-namespace-or-tag namespace))
                        `(("action" . ,(fluiddb-something-to-string action)))
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-get-tag-permissions (tag action)
  "Retrieve the permission information for the given tag and action.
Action can be one of update, delete, control."
  (fluiddb-send-request "GET"
                        (concat "permissions/tags/" (fluiddb-url-format-namespace-or-tag tag))
                        `(("action" . ,(fluiddb-something-to-string action)))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-set-tag-permissions (tag action policy exceptions)
  "Set the permission information for the given tag and action.
Action can be one of update, delete, control.
Policy can be either open or closed."
  (fluiddb-send-request "PUT"
                        (concat "permissions/tags/" (fluiddb-url-format-namespace-or-tag tag))
                        `(("action" . ,(fluiddb-something-to-string action)))
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-get-tag-value-permissions (tag action)
  "Retrieve the permission information for the given tag value and action.
Action can be one of create, read, delete, control."
  (fluiddb-send-request "GET"
                        (concat "permissions/tag-values/" (fluiddb-url-format-namespace-or-tag tag))
                        `(("action" . ,(fluiddb-something-to-string action)))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-set-tag-value-permissions (tag action policy exceptions)
  "Set the permission information for the given tag value and action.
Action can be one of create, read, delete, control.
Policy can be either open or closed."
  (fluiddb-send-request "PUT"
                        (concat "permissions/tag-values/" (fluiddb-url-format-namespace-or-tag tag))
                        `(("action" . ,(fluiddb-something-to-string action)))
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Policies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-policy (user-name category action)
  "Retrieve the user's default policy for the given category and action.
Possible values for categorie/actions are:
- namespaces: create, update, delete, and list.
- tags: update and delete.
- tag-values: create, read, and delete."
  (fluiddb-send-request "GET"
                        (concat "policies/" 
                                (fluiddb-escape-string-for-uri user-name) "/" 
                                (fluiddb-escape-string-for-uri (fluiddb-something-to-string category)) "/"
                                (fluiddb-escape-string-for-uri (fluiddb-something-to-string action)))
                        nil
                        nil
                        "application/json"
                        nil))


(defun fluiddb-set-policy (user-name category action policy exceptions)
  "Set the user's default policy for the given category and action.
Possible values for categorie/actions are:
- namespaces: create, update, delete, and list.
- tags: update and delete.
- tag-values: create, read, and delete.
Policy can be either open or closed."
  (fluiddb-send-request "PUT"
                        (concat "policies/" 
                                (fluiddb-escape-string-for-uri user-name) "/" 
                                (fluiddb-escape-string-for-uri (fluiddb-something-to-string category)) "/"
                                (fluiddb-escape-string-for-uri (fluiddb-something-to-string action)))
                        nil
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-create-tag (namespace name description indexed)
  "Create a new tag withing the given namespace"
  (fluiddb-send-request "POST"
                        (concat "tags/" (fluiddb-url-format-namespace-or-tag namespace))
                        nil
                        (json-encode-alist `(("description" . ,description)
                                             ("name" . ,name)
                                             ("indexed" . ,(fluiddb-bool-to-json-bool indexed))))
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-get-tag (tag)
  "Retrieve information about the given tag"
  (fluiddb-send-request "GET"
                        (concat "tags/" (fluiddb-url-format-namespace-or-tag tag))
                        '(("returnDescription" . "True"))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-change-tag (tag new-description)
  "Change the description of the given tag"
  (fluiddb-send-request "PUT"
                        (concat "tags/" (fluiddb-url-format-namespace-or-tag tag))
                        nil
                        (json-encode-alist `(("description" . ,new-description)))
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-delete-tag (tag)
  "Delete the given tag"
  (fluiddb-send-request "DELETE"
                        (concat "tags/" (fluiddb-url-format-namespace-or-tag tag))
                        nil
                        nil
                        "*/*"
                        nil))


(provide 'fluiddb)
