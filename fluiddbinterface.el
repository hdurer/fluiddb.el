;;; fluiddbinterface.el --- Code to create an Emacs interface to FluidDB
;;
;; Copyright (C) 2009 Holger Durer
;;
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth floor,
;; Boston, MA 02110-1301, USA.

;;; Installation:
;; Place the fluiddb.el and this fluiddbinterface.el files somewhere
;; in your load-path.
;;
;;; Requirements
;; You will need json.el -- e.g. from
;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs
;;
;; This file makes use of url.el which is now shipped with Emacs.  For
;; older Emacs versions you will need to install that yourself -- very
;; old versions of url.el don't seem to work according to user
;; reports.
;;
;;
;; Then either:
;; (require 'fluiddbinterface)
;;
;; or more lazily:
;; (autoload 'fluiddb-browse-user "fluiddbinterface.el" "Browse a specific user in the FluidDB" t)
;; (autoload 'fluiddb-browse-namespace "fluiddbinterface.el" "Browse a specific namespace in the FluidDB" t) 
;; (autoload 'fluiddb-browse-tag "fluiddbinterface.el" "Browse a specific tag in the FluidDB" t)
;; (autoload 'fluiddb-browse-query "fluiddbinterface.el" "Do a query against FluidDB and show the results" t)
;; (autoload 'fluiddb-browse-object "fluiddbinterface.el" "Browse a specific object in the FluidDB" t)

;;; Usage:
;; Invoke any of these five commands mentioned above to enter at
;; FluidDB buffer.  This buffer tries to behave a bit like a web
;; browser with ability to walk the browsing history ('b' and 'f') and
;; to refresh the view ('g').
;;
;; Various bit of text (names of tags, user, namespaces, ...) are
;; active (use 'TAB' and 'S-TAB' to navigate between them) and 'RET'
;; will usually do the sensible action on them (e.g. browse to that
;; object)
;; 
;; Queries are a bit more complicated: Perform a query and then within
;; that buffer press 't' to specify a tag name and to the fetch the
;; object tag values for all objects found in that query.


(require 'fluiddb)                      ;; we need to underlying API functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fluiddb-mode-hook nil
  "Fluiddb-mode hook.")

(defvar fluiddb-mode-map (make-sparse-keymap "FluidDB")
  "The keymap for the Fluiddb-mode buffers")


(defun fluiddb-mode-init-variables ()
  (font-lock-mode -1))

;;(defface fluiddb-title-face
;;  `((((type tty) (class color))
;;     (:background "black" :foreground "white"))
;;    (((type tty) (class mono))
;;     (:bold t))
;;    (((class color) (background dark))
;;     (:background "blue"))
;;    (((class color) (background light))
;;     (:background "lightblue"))
;;    (t (:background "gray")))
;;  "Basic face for highlighting the region."
;;  :group 'basic-faces)


(defun fluiddb-mode ()
  "Major mode for browsing the Fluiddb data.

\\{fluiddb-mode-map}"
  (unless fluiddb-browse-objects
    (kill-all-local-variables))
  (fluiddb-mode-init-variables)
  (use-local-map fluiddb-mode-map)
  (setq major-mode 'fluiddb-mode
        mode-name "FluidDB"
        fluiddb-active-regions nil)
  ;;(set-syntax-table fluiddb-mode-syntax-table)
  (run-mode-hooks 'fluiddb-mode-hook)
  (font-lock-mode -1)
  (buffer-disable-undo (current-buffer)))


(if fluiddb-mode-map
    (let ((km fluiddb-mode-map)
          (bm (make-sparse-keymap)))

      (define-key km "v" 'fluiddb-browser-view-at-point)
      (define-key km (kbd "RET") 'fluiddb-browser-action-at-point)
      
      (define-key km "g" 'fluiddb-browser-reload)
      (define-key km "t" 'fluiddb-browser-add-tag-values)

      ;; navigation
      (define-key km "f" 'fluiddb-browser-forward-in-history)
      (define-key km "b" 'fluiddb-browser-backward-in-history)

      (define-key km (kbd "TAB") 'fluiddb-browser-next-region)
      (define-key km (kbd "S-TAB") 'fluiddb-browser-previous-region)
      (define-key km (kbd "<backtab>") 'fluiddb-browser-previous-region)

      ;; the Browse sub commands
      (define-key km "B" bm)
      (define-key bm "t" 'fluiddb-browse-tag)
      (define-key bm "u" 'fluiddb-browse-user)
      (define-key bm "q" 'fluiddb-browse-query)
      (define-key bm "n" 'fluiddb-browse-namespace)
      (define-key bm "o" 'fluiddb-browse-object)

      nil))

(make-variable-buffer-local 'fluiddb-browse-objects)
(make-variable-buffer-local 'fluiddb-browse-current-object)
(make-variable-buffer-local 'fluiddb-active-regions)
(make-variable-buffer-local 'fluiddb-buffer-id-markers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions

(defun fluiddb-split-name (name)
  "Split a name (tag or namespace) into the containing namespace part and the basename part.
Returns two values - namespace and basename.
The namespace part can be emtpy if there is only one component."
  (let* ((parts (split-string name "/"))
         (n-parts (length parts))
         (ns (with-output-to-string
               (loop for first = t then nil
                     for count from 1 below n-parts
                     for part in parts
                     do (progn
                          (unless first
                            (princ "/"))
                          (princ part)))))
         (last (nth (1- n-parts) parts)))
    (values ns last)))


(defun fluiddb-make-presentable-string (object mime-type)
  "Convert object of mime-type into something short we can show to the user."
  (let ((string (format "%s" object)))
    (if (> (length string) 100)
        (concat (substring string 0 97) "...")
      string)))


(defun fluiddb-get-a-buffer ()
  "Get a FluidDB buffer.  
If the current buffer is a FluidDB buffer use that, otherwise switch to *fluiddb*."
  (unless (eq major-mode 'fluiddb-mode)
    (let ((confirm-nonexistent-file-or-buffer nil))
      (switch-to-buffer "*fluiddb*")))
  (fluiddb-mode)
  (toggle-read-only 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay/active regions operations
(defun fluiddb-resort-buffer-markers ()
  "Re-sort the internal list of active regions after it has changed."
  (setf fluiddb-active-regions (sort fluiddb-active-regions
                                     (lambda (a b)
                                       (< (car a) (car b))))))


(defmacro fluiddb-with-new-active-region (setup-function setup-args &rest body)
  "Set up an overlay for region produced by body.
Call setup-function on the overlay and push admin info about it
into buffer-local list for traversal."
  (let ((start (gensym "start")))
    `(let ((,start (point-marker)))
       ,@body
       (let* ((end (point-marker))
              (overlay (make-overlay ,start end)))
         (apply ,setup-function overlay ,setup-args)
         (push (cons ,start end) fluiddb-active-regions)))))
(put 'fluiddb-with-new-active-region 'lisp-indent-function 3)



(defun fluiddb-make-title-markup (overlay)
  "Add markup for some title text to the given overlay"
  (overlay-put overlay 'face 'bold-italic)
  (overlay-put overlay 'help-echo "Title field describing this page"))


(defun fluiddb-make-id-markup (overlay id)
  "Add markup and actions for some id text to the given overlay"
  (overlay-put overlay 'help-echo "An id. Press 'RET' to browse to this id.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'fluiddb-id id)
  (overlay-put overlay 'action 
               (list (lambda (id) 
                       (fluiddb-browse-object id))
                     id)))


(defun fluiddb-make-tag-markup (overlay tag)
  "Add markup and actions for some tag text to the given overlay"
  (overlay-put overlay 'help-echo "A tag. Press 'RET' to browse to this tag.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'fluiddb-tag tag)
  (overlay-put overlay 'action 
               (list (lambda (tag)
                       (fluiddb-browse-tag tag))
                     tag)))


(defun fluiddb-make-user-markup (overlay user-name)
  (overlay-put overlay 'help-echo "A user. Press 'RET' to browse to this user.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action 
               (list (lambda (name)
                       (fluiddb-browse-user name))
                     user-name)))

(defun fluiddb-make-ns-markup (overlay ns)
  (overlay-put overlay 'help-echo "A namespace. Press 'RET' to browse to this namespace.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action
               (list (lambda (ns)
                       (fluiddb-browse-namespace ns))
                     ns)))

(defun fluiddb-make-query-markup (overlay query)
  (overlay-put overlay 'help-echo "A query. Press 'RET' to browse to this query.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action
               (list (lambda (query)
                       (fluiddb-browse-query query))
                     query)))

(defun fluiddb-make-about-markup (overlay text)
  (overlay-put overlay 'help-echo "'v' or 'RET' to view")
  (overlay-put overlay 'face 'italic)
  (overlay-put overlay 'text text)
  (let ((view-action (lambda (text)
                       (with-output-to-temp-buffer "*FluidDB about text*"
                         (princ text)))))
    (overlay-put overlay 'view-action (list view-action text))
    (overlay-put overlay 'action (list view-action text))))


(defun fluiddb-make-tag-value-markup (overlay guid tag)
  (let ((view-tag-value (lambda (guid tag-name)
                          (with-output-to-temp-buffer "*FluidDB Tag Value*"
                            (save-excursion
                              (switch-to-buffer "*FluidDB Tag Value*")
                              (message "Fetching object's tag value...")
                              (sit-for 0)
                              (let* ((res (fluiddb-get-object-tag-value guid tag-name "*/*"))
                                     (is-ok (first res))
                                     (value (second res))
                                     (mime-type (fourth res)))
                                (if is-ok
                                    (progn
                                      (insert-string (format "Type:   %s\n" mime-type))
                                      (insert-string (format "Length: %s\n" (length value)))
                                      (cond 
                                       ((or (string-equal "image/png" mime-type)
                                            (string-equal "image/gif" mime-type))
                                        (insert-string "Image:  ")
                                        (fluiddb-with-new-active-region
                                            (lambda (overlay value mime-type)
                                              (overlay-put overlay 'display 
                                                           (create-image value 
                                                                         (intern (second 
                                                                                  (split-string mime-type "/")))
                                                                         t)))
                                            (list value mime-type)
                                            (insert-string value)))
                                        (t
                                        (insert-string (format "Value:  %s" value)))))
                                  (insert-string (format "Failed to get tag value %s for %s: %s %s %s"
                                                         tag-name guid (third res)
                                                         (fifth res) (sixth res))))))))))
    (overlay-put overlay 'face 'bold)
    (overlay-put overlay 'help-echo "Tag value.  'v' to view; 'ret' to browse to the tag")
    (overlay-put overlay 'action  
                 (list (function fluiddb-browse-tag)
                       tag))
    (overlay-put overlay 'view-action  
                 (list view-tag-value
                       guid tag))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation


(defun fluiddb-browser-next-region ()
  "Action to perform to move point to the beginning of the next active region (if there is one)"
  (interactive)
  (let* ((current (point))
         (next (loop for pos in fluiddb-active-regions
                     for start = (marker-position (car pos))
                     when (> start current)
                     do (return start))))
    (if next
        (goto-char next)
      (message "No next region"))))


(defun fluiddb-browser-previous-region ()
  "Action to perform to move point to the beginning of the preceeding active region (if there is one)"
  (interactive)
  (let* ((current (point))
         (next (loop for pos in (reverse fluiddb-active-regions)
                     for start = (marker-position (car pos))
                     when (< start current)
                     do (return start))))
    (if next
        (goto-char next)
      (message "No previous region"))))


(defun fluiddb-browser-find-in-history (item)
  (loop for this on fluiddb-browse-objects
        and prev = nil then this
        when (eq (car this) item)
        do (return (values this (car prev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer presentation

(defun fluiddb-setup-buffer (action)
  "Initialise a new or old buffer to be ready for the actual filling command to add contents.
This step *does* already add the common code of user authentication."
  (fluiddb-get-a-buffer)
  (erase-buffer)
  (goto-char (point-min))
  (insert-string "Signed in as: ")
  (fluiddb-with-new-active-region
      (lambda (overlay)
        (overlay-put overlay 'face 'underlined)
        (overlay-put overlay 'action (list (function fluiddb-do-change-authentication) (current-buffer))))
      ()
      (if *fluiddb-credentials*
          (insert-string (car *fluiddb-credentials*))
        (insert-string "*None* (anonymous)")))
  (newline)
  (insert-string "____________________________________________________")
  (newline))


(defun fluiddb-do-change-authentication (&optional buffer)
  "Action to perform to change the user credentials to use when accessing the FluidDB."
  (interactive)
  (let ((name (read-string "User name (empty for anon access): " (car *fluiddb-credentials*) )))
    (if (string-equal "" name)
        (setf *fluiddb-credentials* nil)
      (let ((password (read-passwd "Password: ")))
        (setf *fluiddb-credentials* (cons name password))))))


(defun fluiddb-browser-view-at-point ()
  "Action to perform when user pressed 'v' on an active region.
This looks up the actual action in the overlay."
  (interactive)
  (let ((action (get-char-property (point) 'view-action)))
    (if action
        (apply (car action) (cdr action))
      (message "Nothing to view at point"))))


(defun fluiddb-browser-action-at-point ()
  "Action to perform when user pressed 'RET' on an active region.
This looks up the actual action in the overlay."
  (interactive)
  (let ((action (get-char-property (point) 'action)))
    (if action
        (apply (car action) (cdr action))
      (message "Nothing to do at point"))))



(defun fluiddb-browser-reload ()
  (interactive)
  (fluiddb-show-this fluiddb-browse-current-object nil))


(defun fluiddb-browser-add-tag-values (tag)
  (interactive "sTag to show: ")
  (if fluiddb-buffer-id-markers
      (progn
        (toggle-read-only 0)
        (loop for (id marker) in fluiddb-buffer-id-markers
              do (progn
                   (goto-char marker)
                   (newline)
                   (insert-string "        - ")
                   (fluiddb-with-new-active-region (function fluiddb-make-tag-markup)
                       (list tag)
                       (insert-string tag))
                   (insert-string ": ")
                   (let ((res (fluiddb-get-object-tag-value id tag "*/*")))
                     (if (first res)
                         ;; ok
                         (let ((text (fluiddb-make-presentable-string (second res) (fourth res))))
                           (fluiddb-with-new-active-region (function fluiddb-make-about-markup)
                               (list (second res))
                               (insert-string text))
                           (insert-string (format " (%s)" (fourth res))))
                       (insert-string (format "Failed: %s %s %s" (third res) (fifth res) (sixth res)))))
                   (fluiddb-resort-buffer-markers)
                   (sit-for 0)))
        (toggle-read-only 1))
    (message "Nothing here to show tags on!")))

(defun fluiddb-browser-backward-in-history ()
  (interactive)
  (multiple-value-bind (this prev) (fluiddb-browser-find-in-history fluiddb-browse-current-object)
    (if (cadr this)
        (progn
          (setq fluiddb-browse-current-object (cadr this))
          (fluiddb-show-this fluiddb-browse-current-object nil))
      (message "Nothing to go back to"))))

(defun fluiddb-browser-forward-in-history ()
  (interactive)
  (multiple-value-bind (this prev) (fluiddb-browser-find-in-history fluiddb-browse-current-object)
    (if prev
        (progn
          (setq fluiddb-browse-current-object prev)
          (fluiddb-show-this fluiddb-browse-current-object nil))
      (message "Nothing to go forward to"))))



(defun fluiddb-show-object (guid)
  (let ((res (fluiddb-get-object guid)))
    (if (first res)
        (progn
          ;; ok result
          (fluiddb-with-new-active-region (function fluiddb-make-title-markup) 
              ()
              (insert-string (format "Object %s: " guid)))
          (newline)
          (let ((about (assoc 'about (second res)))
                (tags (cdr (assoc 'tagPaths (second res)))))
            (if about
                (progn
                  (insert-string "  About: ")
                  (fluiddb-with-new-active-region (function fluiddb-make-about-markup)
                      (list (cdr about))
                      (insert-string (format "%s" (cdr about)))))
              (insert-string "  (Anonymous -- no 'about' string)"))
            (newline)
            (insert-string (format "  %s tags:" (length tags)))
            (newline)
            (loop 
             for index from 1
             for tag-name across tags
             do (progn
                  (insert-string (format "  %3d: " index))
                  (fluiddb-with-new-active-region (function fluiddb-make-tag-value-markup)
                      (list guid tag-name)
                      (insert-string tag-name))
                  (newline)))))
      (insert-string
       (format "Error getting object %s -- %s %s %s %s" 
               guid
               (third res) (fifth res) (sixth res))))))


 
(defun fluiddb-show-tag (full-tag)
  (multiple-value-bind (ns tag) (fluiddb-split-name full-tag)
    (let ((res (fluiddb-get-tag ns tag)))
      (if (first res)
        (progn
          ;; ok result
          (fluiddb-with-new-active-region (function fluiddb-make-title-markup) 
              ()
              (insert-string "Tag '")
            (fluiddb-with-new-active-region (function fluiddb-make-tag-markup)
                (list full-tag)
                (insert-string tag))
            (insert-string "' in namespace '")
            (fluiddb-with-new-active-region (function fluiddb-make-ns-markup)
                (list ns)
                (insert-string ns))
            (insert-string "'"))
          (newline)
          (let ((user (first (split-string full-tag "/")))
                (description (cdr (assoc 'description (second res))))
                (id (cdr (assoc 'id (second res))))
                (indexed (cdr (assoc 'indexed (second res)))))
            (insert-string (format "  Id:          " ))
            (fluiddb-with-new-active-region (function fluiddb-make-id-markup) 
                (list id)
                (insert-string id))
            (newline)
            (insert-string "  Description: ")
            (fluiddb-with-new-active-region (function fluiddb-make-about-markup)
                (list description)
                (insert-string (format "%s" description)))
            (newline)
            (insert-string (format "  Is indexed:  %s" (if indexed "Yes" "No")))
            (newline)))
            (insert-string "  Belongs to:  ")
            (fluiddb-with-new-active-region (function fluiddb-make-user-markup)
                (list user)
                (insert-string (format "%s" user)))
            (newline)
        (insert-string
         (format "Error getting tag %s -- %s %s %s %s" 
                 full-tag
                 (third res) (fifth res) (sixth res)))))))


(defun fluiddb-show-user (name)
  (let ((res (fluiddb-get-user name)))
      (if (first res)
        (progn
          ;; ok result
          (fluiddb-with-new-active-region (function fluiddb-make-title-markup) 
              ()
              (insert-string (format "User '%s'\n" name)))
          (let ((id (cdr (assoc 'id (second res)))))
            (insert-string (format "  Id:          " ))
            (fluiddb-with-new-active-region (function fluiddb-make-id-markup) 
                (list id)
                (insert-string id))
            (newline)
            (insert-string "  User's namespace: ")
            (fluiddb-with-new-active-region (function fluiddb-make-ns-markup)
                (list name)
                (insert-string (format "%s" name)))
            (newline)))
        (insert-string
         (format "Error getting user %s -- %s %s %s"
                 name
                 (third res) (fifth res) (sixth res))))))


(defun fluiddb-show-ns (ns)
  (multiple-value-bind (parent-ns ns-name) (fluiddb-split-name ns)
    (let ((res (fluiddb-get-namespace ns)))
      (if (first res)
        (progn
          ;; ok result
          (fluiddb-with-new-active-region (function fluiddb-make-title-markup) 
              ()
              (insert-string "Namespace '")
            (fluiddb-with-new-active-region (function fluiddb-make-ns-markup)
                (list ns)
                (insert-string ns-name))
            (unless (string-equal "" parent-ns)
              (insert-string "' within namespace '")
              (fluiddb-with-new-active-region (function fluiddb-make-ns-markup)
                  (list parent-ns)
                  (insert-string parent-ns)))
            (insert-string "'"))
          (newline)
          (let ((user (first (split-string ns "/")))
                (description (cdr (assoc 'description (second res))))
                (id (cdr (assoc 'id (second res))))
                (tags (cdr (assoc 'tagNames (second res))))
                (sub-namespaces (cdr (assoc 'namespaceNames (second res)))))
            (insert-string (format "  Id:          "))
            (fluiddb-with-new-active-region (function fluiddb-make-id-markup) 
                (list id)
                (insert-string id))
            (newline)
            (insert-string "  Description: ")
            (fluiddb-with-new-active-region (function fluiddb-make-about-markup)
                (list description)
                (insert-string (format "%s" description)))
            (newline)
            (insert-string "  Belongs to:  ")
            (fluiddb-with-new-active-region (function fluiddb-make-user-markup)
                (list user)
                (insert-string (format "%s" user)))
            (newline)
            (newline)
            ;; format the sub-namespaces
            (insert-string (format "  %s sub-namespaces:" (length sub-namespaces)))
            (newline)
            (loop for index from 1
                  for sub-ns across sub-namespaces
                  do (progn
                       (insert-string (format "  %3d: " index))
                       (fluiddb-with-new-active-region (function fluiddb-make-ns-markup)
                           (list (concat ns "/" sub-ns))
                           (insert-string sub-ns))
                       (newline)))
            ;; format the tags within this namespace
            (newline)
            (insert-string (format "  %s tags:" (length tags)))
            (newline)
            (loop for index from 1
                  for tag across tags
                  do (progn
                       (insert-string (format "  %3d: " index))
                       (fluiddb-with-new-active-region (function fluiddb-make-tag-markup)
                           (list (concat ns "/" tag))
                           (insert-string tag))
                       (newline)))))
        (insert-string
         (format "Error getting namespace %s -- %s %s %s %s" 
                 ns
                 (third res) (fifth res) (sixth res)))))))


(defun fluiddb-show-query (query)
  (let ((res (fluiddb-query-objects query)))
    (if (first res)
        (let ((ids (cdr (assoc 'ids (second res)))))
          ;; ok result
          (fluiddb-with-new-active-region (function fluiddb-make-title-markup) 
              ()
              (insert-string (format "%d results for query '" (length ids)))
            (fluiddb-with-new-active-region (function fluiddb-make-query-markup)
                (list query)
                (insert-string query))
            (insert-string "'"))
          (newline)
          (newline)
          (loop for index from 1
                for id across ids
                do (progn
                     (insert-string (format "  %3d: " index))
                     (fluiddb-with-new-active-region (function fluiddb-make-id-markup) 
                         (list id)
                         (insert-string id))
                     (push (list id (point-marker)) fluiddb-buffer-id-markers)
                     (newline)))
          (newline)
          (newline)
          (insert-string "(press 't' to add object-tag values)")
          (setq fluiddb-buffer-id-markers (nreverse fluiddb-buffer-id-markers)))
      (insert-string
       (format "Error performing query %s -- %s %s %s %s" 
               query
               (third res) (fifth res) (sixth res))))))



(defun fluiddb-show-this (item add-to-history)
  (fluiddb-setup-buffer item)
  (setq fluiddb-browse-current-object item)
  (when add-to-history
    (setq fluiddb-browse-objects (cons item fluiddb-browse-objects)))
  
  (setq fluiddb-buffer-id-markers nil) ;; reset anything in there already

  (ecase (first item)
    (:query (fluiddb-show-query (second item)))
    (:object (fluiddb-show-object (second item)))
    (:tag (fluiddb-show-tag (second item)))
    (:user (fluiddb-show-user (second item)))
    (:namespace (fluiddb-show-ns (second item))))

  (fluiddb-resort-buffer-markers)
  (goto-char (if (cdr fluiddb-active-regions)
                 (caadr fluiddb-active-regions)
               (point-min)))
  (toggle-read-only 1)
  (message "done"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-browse-object (guid)
  "Browse a specific object in the FluidDB"
  (interactive "sGUID: ")
  (if (= (length  guid) 36)
      (let ((action (list :object guid)))
        (fluiddb-show-this action t))
    (message "Not a valid GUID")))

(defun fluiddb-browse-namespace (ns)
  "Browse a specific namespace in the FluidDB"
  (interactive "sNamespace: ")
  (let ((action (list :namespace ns)))
    (fluiddb-show-this action t)))

(defun fluiddb-browse-tag (tag)
  "Browse a specific tag in the FluidDB"
  (interactive "sTag: ")
  (let ((action (list :tag tag)))
    (fluiddb-show-this action t)))

(defun fluiddb-browse-user (user-name)
  "Browse a specific user in the FluidDB"
  (interactive "sUser name: ")
  (let ((action (list :user user-name)))
    (fluiddb-show-this action t)))


(defun fluiddb-browse-query (query)
  "Do a query against FluidDB and show the results"
  (interactive "sQuery: ")
  (if (string-equal "" query)
      (message "No query given")
    (let ((action (list :query query)))
      (fluiddb-show-this action t))))



(provide 'fluiddbinterface)
