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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                          (browse-url-url-encode-chars value "[+ &?]")))))
      (let* ((url-request-method method)
             (url-mime-accept-string accept-value)
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
        (let ((status url-http-response-status)
              (content-type url-http-content-type)
              (content (buffer-substring (point) (point-max))))
          (kill-buffer buffer)
          (if (string-equal content-type "application/json")
              (setq content (json-read-from-string content)))
          (list status content))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Policies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

