;;; fluiddb.el --- Code to work with FluidDB
;;
;; Copyright (C) 2009 Holger Durer
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
  "Nil (use anonymous access) or a cons of user-name and password")

(defvar *fluiddb-server* "sandbox.fluidinfo.com"
  "The server to use for calls -- either the main instance or sandbox")


(defvar *fluiddb-within-call* nil
  "Helper variable to indicate if we are withing a fluiddb call and thus won't want the authentication mechanisms to kick in")

(defadvice url-http-handle-authentication (around fluiddb-fix)
  (unless *fluiddb-within-call*
      ad-do-it))
(ad-activate 'url-http-handle-authentication)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-escape-string-for-uri (string)
  "Escape the string to be usable in an URI (doing percent escaping of certain characters.

Newer versions of url.el have the function
browse-url-url-encode-chars for this but if we bring our own,
this will work with Emacs 22 as well.

We escape everything except letters, digits and - . _ ~
(c.f. section 2.3 of RFC 3986)."
  (with-output-to-string
    (loop for char across string
          do (princ (if (or (and (>= char ?a) (<= char ?z))
                            (and (>= char ?A) (<= char ?Z))
                            (and (>= char ?0) (<= char ?9))
                            (memq char (list ?- ?. ?_ ?~)))
                        (format "%c" char)
                      (format "%%%02X" char))))))

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
  (json-encode-alist
   `(("policy" . ,(fluiddb-something-to-string policy))
     ("exceptions" . ,(mapcar 'fluiddb-something-to-string exceptions)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-user (user-name)
  (fluiddb-send-request "GET"
                        (concat "users/" user-name)
                        nil
                        nil
                        "application/json"
                        nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-object (guid)
  (fluiddb-send-request "GET"
                        (concat "objects/" guid)
                        '(("showAbout" . "True"))
                        nil
                        "application/json"
                        nil))

(defun fluiddb-query-objects (query)
  (fluiddb-send-request "GET"
                        "objects"
                        `(("query" . ,query))
                        nil
                        "application/json"
                        nil))

(defun fluiddb-create-object (&optional about)
  (fluiddb-send-request "POST"
                        "objects"
                        nil
                        (when about
                          (json-encode-alist `(("about" . ,about))))
                        "application/json"
                        '(("Content-Type" . "application/json"))))

(defun fluiddb-get-object-tag-value (id tag &optional accept)
  (fluiddb-send-request "GET"
                        (concat "objects/" id "/" tag)
                        nil
                        nil
                        (or accept "application/vnd.fluiddb.value+json")                          
                        nil))

(defun fluiddb-set-object-tag-value (id tag contents &optional content-type)
  (fluiddb-send-request "PUT"
                        (concat "objects/" id "/" tag)
                        nil
                        (if content-type
                            contents
                          (json-encode contents))
                        "*/*"
                        `(("Content-type" . ,(or content-type "application/vnd.fluiddb.value+json")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-namespace (ns)
  (fluiddb-send-request "GET"
                        (concat "namespaces/" ns)
                        '(("returnDescription" . "True")
                          ("returnNamespaces" . "True")
                          ("returnTags" . "True"))
                        nil
                        "application/json"
                        nil))

(defun fluiddb-create-namespace (ns name description)
  (fluiddb-send-request "POST"
                        (concat "namespaces/" ns)
                        nil
                        (json-encode-alist `(("description" . ,description)
                                             ("name" . ,name)))

                        "application/json"
                        '(("Content-Type" . "application/json"))))

(defun fluiddb-change-namespace (ns new-description)
  (fluiddb-send-request "PUT"
                        (concat "namespaces/" ns)
                        nil
                        (json-encode-alist `(("description" . ,new-description)))
                        "application/json"
                        '(("Content-Type" . "application/json"))))

(defun fluiddb-delete-namespace (ns)
  (fluiddb-send-request "DELETE"
                        (concat "namespaces/" ns)
                        nil
                        nil
                        "*/*"
                        nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-namespace-permissions (namespace action)
  (fluiddb-send-request "GET"
                        (concat "permissions/namespaces/" namespace)
                        `(("action" . ,(fluiddb-something-to-string action)))
                        nil
                        "application/json"
                        nil))

(defun fluiddb-set-namespace-permissions (namespace action policy exceptions)
  (fluiddb-send-request "PUT"
                        (concat "permissions/namespaces/" namespace)
                        `(("action" . ,(fluiddb-something-to-string action)))
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-get-tag-permissions (tag action)
  (fluiddb-send-request "GET"
                        (concat "permissions/tags/" tag)
                        `(("action" . ,(fluiddb-something-to-string action)))
                        nil
                        "application/json"
                        nil))

(defun fluiddb-set-tag-permissions (tag action policy exceptions)
  (fluiddb-send-request "PUT"
                        (concat "permissions/tags/" tag)
                        `(("action" . ,(fluiddb-something-to-string action)))
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))

(defun fluiddb-get-tag-value-permissions (tag action)
  (fluiddb-send-request "GET"
                        (concat "permissions/tag-values/" tag-value)
                        `(("action" . ,(fluiddb-something-to-string action)))
                        nil
                        "application/json"
                        nil))

(defun fluiddb-set-tag-value-permissions (tag action policy exceptions)
  (fluiddb-send-request "PUT"
                        (concat "permissions/tag-values/" tag-value)
                        `(("action" . ,(fluiddb-something-to-string action)))
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Policies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-get-policy (user-name category action)
  (fluiddb-send-request "GET"
                        (concat "policies/" 
                                user-name "/" 
                                (fluiddb-something-to-string category) "/"
                                (fluiddb-something-to-string action))
                        nil
                        nil
                        "application/json"
                        nil))

(defun fluiddb-set-policy (user-name category action policy exceptions)
  (fluiddb-send-request "PUT"
                        (concat "policies/" 
                                user-name "/" 
                                (fluiddb-something-to-string category) "/"
                                (fluiddb-something-to-string action))
                        nil
                        (fluiddb-make-permission-object policy exceptions)
                        "application/json"
                        '(("Content-Type" . "application/json"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-create-tag (ns name description indexed)
  (fluiddb-send-request "POST"
                        (concat "tags/" ns)
                        nil
                        (json-encode-alist `(("description" . ,description)
                                             ("name" . ,name)
                                             ("indexed" . ,(fluiddb-bool-to-json-bool indexed))))
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-get-tag (ns tag)
  (fluiddb-send-request "GET"
                        (concat "tags/" ns "/" tag)
                        '(("returnDescription" . "True"))
                        nil
                        "application/json"
                        nil))


(defun fluiddb-change-tag (ns tag new-description)
  (fluiddb-send-request "PUT"
                        (concat "tags/" ns "/" tag)
                        nil
                        (json-encode-alist `(("description" . ,new-description)))
                        "application/json"
                        '(("Content-Type" . "application/json"))))


(defun fluiddb-delete-tag (ns tag-description)
  (fluiddb-send-request "DELETE"
                        (concat "tags/" ns "/" tag)
                        nil
                        nil
                        "*/*"
                        nil))


(provide 'fluiddb)
