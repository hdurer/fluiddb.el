;;; fluidinfointerface.el --- Code to create an Emacs interface to Fluidinfo
;;
;; Copyright (C) 2009, 2010 Holger Durer
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
;; Place the fluidinfo.el and this fluidinfointerface.el files somewhere
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
;; (require 'fluidinfointerface)
;;
;; or more lazily:
;; (autoload 'fluidinfo-browse-user "fluidinfointerface.el" "Browse a specific user in the Fluidinfo" t)
;; (autoload 'fluidinfo-browse-namespace "fluidinfointerface.el" "Browse a specific namespace in the Fluidinfo" t) 
;; (autoload 'fluidinfo-browse-tag "fluidinfointerface.el" "Browse a specific tag in the Fluidinfo" t)
;; (autoload 'fluidinfo-browse-query "fluidinfointerface.el" "Do a query against Fluidinfo and show the results" t)
;; (autoload 'fluidinfo-browse-object "fluidinfointerface.el" "Browse a specific object in the Fluidinfo" t)

;;; Usage:
;; Invoke any of these five commands mentioned above to enter at
;; Fluidinfo buffer.  This buffer tries to behave a bit like a web
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


(require 'fluidinfo)                      ;; we need to underlying API functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fluidinfo-mode-hook nil
  "Fluidinfo-mode hook.")

(defvar fluidinfo-mode-map (make-sparse-keymap "Fluidinfo")
  "The keymap for the Fluidinfo-mode buffers")


(defun fluidinfo-mode-init-variables ()
  (font-lock-mode -1))

;;(defface fluidinfo-title-face
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


(defun fluidinfo-mode ()
  "Major mode for browsing the Fluidinfo data.

\\{fluidinfo-mode-map}"
  (unless fluidinfo-browse-objects
    (kill-all-local-variables))
  (fluidinfo-mode-init-variables)
  (use-local-map fluidinfo-mode-map)
  (setq major-mode 'fluidinfo-mode
        mode-name "Fluidinfo"
        fluidinfo-active-regions nil)
  ;;(set-syntax-table fluidinfo-mode-syntax-table)
  (run-mode-hooks 'fluidinfo-mode-hook)
  (font-lock-mode -1)
  (buffer-disable-undo (current-buffer)))


(if fluidinfo-mode-map
    (let ((km fluidinfo-mode-map)
          (bm (make-sparse-keymap)))

      (define-key km "v" 'fluidinfo-browser-view-at-point)
      (define-key km (kbd "RET") 'fluidinfo-browser-action-at-point)
      
      (define-key km "g" 'fluidinfo-browser-reload)
      (define-key km "t" 'fluidinfo-browser-add-tag-values)

      ;; navigation
      (define-key km "f" 'fluidinfo-browser-forward-in-history)
      (define-key km "b" 'fluidinfo-browser-backward-in-history)

      (define-key km (kbd "TAB") 'fluidinfo-browser-next-region)
      (define-key km (kbd "S-TAB") 'fluidinfo-browser-previous-region)
      (define-key km (kbd "<backtab>") 'fluidinfo-browser-previous-region)

      ;; the Browse sub commands
      (define-key km "B" bm)
      (define-key bm "t" 'fluidinfo-browse-tag)
      (define-key bm "u" 'fluidinfo-browse-user)
      (define-key bm "q" 'fluidinfo-browse-query)
      (define-key bm "n" 'fluidinfo-browse-namespace)
      (define-key bm "o" 'fluidinfo-browse-object)

      nil))

(make-variable-buffer-local 'fluidinfo-browse-objects)
(make-variable-buffer-local 'fluidinfo-browse-current-object)
(make-variable-buffer-local 'fluidinfo-active-regions)
(make-variable-buffer-local 'fluidinfo-buffer-id-markers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions

(defun fluidinfo-split-name (name)
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


(defun fluidinfo-make-presentable-string (object mime-type)
  "Convert object of mime-type into something short we can show to the user."
  (let ((string (format "%s" object)))
    (if (> (length string) 100)
        (concat (substring string 0 97) "...")
      string)))


(defun fluidinfo-get-a-buffer ()
  "Get a Fluidinfo buffer.  
If the current buffer is a Fluidinfo buffer use that, otherwise switch to *fluidinfo*."
  (unless (eq major-mode 'fluidinfo-mode)
    (let ((confirm-nonexistent-file-or-buffer nil))
      (switch-to-buffer "*fluidinfo*")))
  (fluidinfo-mode)
  (toggle-read-only 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay/active regions operations
(defun fluidinfo-resort-buffer-markers ()
  "Re-sort the internal list of active regions after it has changed."
  (setf fluidinfo-active-regions (sort fluidinfo-active-regions
                                     (lambda (a b)
                                       (< (car a) (car b))))))


(defmacro fluidinfo-with-new-active-region (setup-function setup-args &rest body)
  "Set up an overlay for region produced by body.
Call setup-function on the overlay and push admin info about it
into buffer-local list for traversal."
  (let ((start (gensym "start")))
    `(let ((,start (point-marker)))
       ,@body
       (let* ((end (point-marker))
              (overlay (make-overlay ,start end)))
         (apply ,setup-function overlay ,setup-args)
         (push (cons ,start end) fluidinfo-active-regions)))))
(put 'fluidinfo-with-new-active-region 'lisp-indent-function 3)



(defun fluidinfo-make-title-markup (overlay)
  "Add markup for some title text to the given overlay"
  (overlay-put overlay 'face 'bold-italic)
  (overlay-put overlay 'help-echo "Title field describing this page"))


(defun fluidinfo-make-id-markup (overlay id)
  "Add markup and actions for some id text to the given overlay"
  (overlay-put overlay 'help-echo "An id. Press 'RET' to browse to this id.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'fluidinfo-id id)
  (overlay-put overlay 'action 
               (list (lambda (id) 
                       (fluidinfo-browse-object id))
                     id)))


(defun fluidinfo-make-tag-markup (overlay tag)
  "Add markup and actions for some tag text to the given overlay"
  (overlay-put overlay 'help-echo "A tag. Press 'RET' to browse to this tag.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'fluidinfo-tag tag)
  (overlay-put overlay 'action 
               (list (lambda (tag)
                       (fluidinfo-browse-tag tag))
                     tag)))


(defun fluidinfo-make-user-markup (overlay user-name)
  (overlay-put overlay 'help-echo "A user. Press 'RET' to browse to this user.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action 
               (list (lambda (name)
                       (fluidinfo-browse-user name))
                     user-name)))

(defun fluidinfo-make-ns-markup (overlay ns)
  (overlay-put overlay 'help-echo "A namespace. Press 'RET' to browse to this namespace.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action
               (list (lambda (ns)
                       (fluidinfo-browse-namespace ns))
                     ns)))

(defun fluidinfo-make-query-markup (overlay query)
  (overlay-put overlay 'help-echo "A query. Press 'RET' to browse to this query.")
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action
               (list (lambda (query)
                       (fluidinfo-browse-query query))
                     query)))

(defun fluidinfo-make-about-markup (overlay text)
  (overlay-put overlay 'help-echo "'v' or 'RET' to view")
  (overlay-put overlay 'face 'italic)
  (overlay-put overlay 'text text)
  (let ((view-action (lambda (text)
                       (with-output-to-temp-buffer "*Fluidinfo about text*"
                         (princ text)))))
    (overlay-put overlay 'view-action (list view-action text))
    (overlay-put overlay 'action (list view-action text))))


(defun fluidinfo-make-tag-value-markup (overlay guid tag)
  (let ((view-tag-value (lambda (guid tag-name)
                          (with-output-to-temp-buffer "*Fluidinfo Tag Value*"
                            (save-excursion
                              (switch-to-buffer "*Fluidinfo Tag Value*")
                              (message "Fetching object's tag value...")
                              (sit-for 0)
                              (let* ((res (fluidinfo-get-object-tag-value guid tag-name "*/*"))
                                     (is-ok (first res))
                                     (value (second res))
                                     (mime-type (fourth res)))
                                (if is-ok
                                    (progn
                                      (insert-string (format "Type:   %s\n" mime-type))
                                      (insert-string (format "Length: %s\n" (length value)))
                                      (cond 
                                       ((or (string-equal "image/png" mime-type)
                                            (string-equal "image/jpg" mime-type)
                                            (string-equal "image/gif" mime-type)
                                            (string-equal "image/tiff" mime-type)
                                            (string-equal "image/xbm" mime-type)
                                            (string-equal "image/xpm" mime-type))
                                        (insert-string "Image:  ")
                                        (fluidinfo-with-new-active-region
                                            (lambda (overlay value mime-type)
                                              (overlay-put overlay 'display 
                                                           (create-image value 
                                                                         nil ;; detect type
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
                 (list (function fluidinfo-browse-tag)
                       tag))
    (overlay-put overlay 'view-action  
                 (list view-tag-value
                       guid tag))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation


(defun fluidinfo-browser-next-region ()
  "Action to perform to move point to the beginning of the next active region (if there is one)"
  (interactive)
  (let* ((current (point))
         (next (loop for pos in fluidinfo-active-regions
                     for start = (marker-position (car pos))
                     when (> start current)
                     do (return start))))
    (if next
        (goto-char next)
      (message "No next region"))))


(defun fluidinfo-browser-previous-region ()
  "Action to perform to move point to the beginning of the preceeding active region (if there is one)"
  (interactive)
  (let* ((current (point))
         (next (loop for pos in (reverse fluidinfo-active-regions)
                     for start = (marker-position (car pos))
                     when (< start current)
                     do (return start))))
    (if next
        (goto-char next)
      (message "No previous region"))))


(defun fluidinfo-browser-find-in-history (item)
  "Helper function to look up things in the browser history."
  (loop for this on fluidinfo-browse-objects
        and prev = nil then this
        when (eq (car this) item)
        do (return (values this (car prev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer presentation and actions in the buffer

(defun fluidinfo-setup-buffer (action)
  "Initialise a new or old buffer to be ready for the actual filling command to add contents.
This step *does* already add the common code of user authentication."
  (fluidinfo-get-a-buffer)
  (erase-buffer)
  (goto-char (point-min))
  (insert-string "Signed in as: ")
  (fluidinfo-with-new-active-region
      (lambda (overlay)
        (overlay-put overlay 'face 'underlined)
        (overlay-put overlay 'action (list (function fluidinfo-do-change-authentication) (current-buffer))))
      ()
      (if *fluidinfo-credentials*
          (insert-string (car *fluidinfo-credentials*))
        (insert-string "*None* (anonymous)")))
  (newline)
  (insert-string "____________________________________________________")
  (newline))


(defun fluidinfo-do-change-authentication (&optional buffer)
  "Action to perform to change the user credentials to use when accessing the Fluidinfo."
  (interactive)
  (let ((name (read-string "User name (empty for anon access): " (car *fluidinfo-credentials*) )))
    (if (string-equal "" name)
        (setf *fluidinfo-credentials* nil)
      (let ((password (read-passwd "Password: ")))
        (setf *fluidinfo-credentials* (cons name password))))))


(defun fluidinfo-browser-view-at-point ()
  "Action to perform when user pressed 'v' on an active region.
This looks up the actual action in the overlay."
  (interactive)
  (let ((action (get-char-property (point) 'view-action)))
    (if action
        (apply (car action) (cdr action))
      (message "Nothing to view at point"))))


(defun fluidinfo-browser-action-at-point ()
  "Action to perform when user pressed 'RET' on an active region.
This looks up the actual action in the overlay."
  (interactive)
  (let ((action (get-char-property (point) 'action)))
    (if action
        (apply (car action) (cdr action))
      (message "Nothing to do at point"))))



(defun fluidinfo-browser-reload ()
  "Reload the currently browsed object."
  (interactive)
  (fluidinfo-show-this fluidinfo-browse-current-object nil))


(defun fluidinfo-browser-add-tag-values (tag)
  (interactive "sTag to show: ")
  (if fluidinfo-buffer-id-markers
      (progn
        (toggle-read-only 0)
        (loop for (id marker) in fluidinfo-buffer-id-markers
              do (progn
                   (goto-char marker)
                   (newline)
                   (insert-string "        - ")
                   (fluidinfo-with-new-active-region (function fluidinfo-make-tag-markup)
                       (list tag)
                       (insert-string tag))
                   (insert-string ": ")
                   (let ((res (fluidinfo-get-object-tag-value id tag "*/*")))
                     (if (first res)
                         ;; ok
                         (let ((text (fluidinfo-make-presentable-string (second res) (fourth res))))
                           (fluidinfo-with-new-active-region (function fluidinfo-make-about-markup)
                               (list (second res))
                               (insert-string text))
                           (insert-string (format " (%s)" (fourth res))))
                       (insert-string (format "Failed: %s %s %s" (third res) (fifth res) (sixth res)))))
                   (fluidinfo-resort-buffer-markers)
                   (sit-for 0)))
        (toggle-read-only 1))
    (message "Nothing here to show tags on!")))

(defun fluidinfo-browser-backward-in-history ()
  "Go backwards in the browsing history"
  (interactive)
  (multiple-value-bind (this prev) (fluidinfo-browser-find-in-history fluidinfo-browse-current-object)
    (if (cadr this)
        (progn
          (setq fluidinfo-browse-current-object (cadr this))
          (fluidinfo-show-this fluidinfo-browse-current-object nil))
      (message "Nothing to go back to"))))

(defun fluidinfo-browser-forward-in-history ()
  "Go forwards in the browsing history"
  (interactive)
  (multiple-value-bind (this prev) (fluidinfo-browser-find-in-history fluidinfo-browse-current-object)
    (if prev
        (progn
          (setq fluidinfo-browse-current-object prev)
          (fluidinfo-show-this fluidinfo-browse-current-object nil))
      (message "Nothing to go forward to"))))



(defun fluidinfo-show-object (guid)
  (let ((res (fluidinfo-get-object guid)))
    (if (first res)
        (progn
          ;; ok result
          (fluidinfo-with-new-active-region (function fluidinfo-make-title-markup) 
              ()
              (insert-string (format "Object %s: " guid)))
          (newline)
          (let ((about (assoc 'about (second res)))
                (tags (cdr (assoc 'tagPaths (second res)))))
            (if about
                (progn
                  (insert-string "  About: ")
                  (fluidinfo-with-new-active-region (function fluidinfo-make-about-markup)
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
                  (fluidinfo-with-new-active-region (function fluidinfo-make-tag-value-markup)
                      (list guid tag-name)
                      (insert-string tag-name))
                  (newline)))))
      (insert-string
       (format "Error getting object %s -- %s %s %s" 
               guid
               (third res) (fifth res) (sixth res))))))


 
(defun fluidinfo-show-tag (full-tag)
  (multiple-value-bind (ns tag) (fluidinfo-split-name full-tag)
    (let ((res (fluidinfo-get-tag full-tag)))
      (if (first res)
        (progn
          ;; ok result
          (fluidinfo-with-new-active-region (function fluidinfo-make-title-markup) 
              ()
              (insert-string "Tag '")
            (fluidinfo-with-new-active-region (function fluidinfo-make-tag-markup)
                (list full-tag)
                (insert-string tag))
            (insert-string "' in namespace '")
            (fluidinfo-with-new-active-region (function fluidinfo-make-ns-markup)
                (list ns)
                (insert-string ns))
            (insert-string "'"))
          (newline)
          (let ((user (first (split-string full-tag "/")))
                (description (cdr (assoc 'description (second res))))
                (id (cdr (assoc 'id (second res))))
                (indexed (cdr (assoc 'indexed (second res)))))
            (insert-string (format "  Id:          " ))
            (fluidinfo-with-new-active-region (function fluidinfo-make-id-markup) 
                (list id)
                (insert-string id))
            (newline)
            (insert-string "  Description: ")
            (fluidinfo-with-new-active-region (function fluidinfo-make-about-markup)
                (list description)
                (insert-string (format "%s" description)))
            (newline)
            (insert-string (format "  Is indexed:  %s" (if indexed "Yes" "No")))
            (newline)
            (insert-string "  Belongs to:  ")
            (fluidinfo-with-new-active-region (function fluidinfo-make-user-markup)
                (list user)
                (insert-string (format "%s" user)))
            (newline)))
        (insert-string
         (format "Error getting tag %s -- %s %s %s" 
                 full-tag
                 (third res) (fifth res) (sixth res)))))))


(defun fluidinfo-show-user (name)
  (let ((res (fluidinfo-get-user name)))
      (if (first res)
        (progn
          ;; ok result
          (fluidinfo-with-new-active-region (function fluidinfo-make-title-markup) 
              ()
              (insert-string (format "User '%s'\n" name)))
          (let ((id (cdr (assoc 'id (second res)))))
            (insert-string (format "  Id:          " ))
            (fluidinfo-with-new-active-region (function fluidinfo-make-id-markup) 
                (list id)
                (insert-string id))
            (newline)
            (insert-string "  User's namespace: ")
            (fluidinfo-with-new-active-region (function fluidinfo-make-ns-markup)
                (list (downcase name))
                (insert-string (format "%s" (downcase name))))
            (newline)))
        (insert-string
         (format "Error getting user %s -- %s %s %s"
                 name
                 (third res) (fifth res) (sixth res))))))


(defun fluidinfo-show-ns (ns)
  (multiple-value-bind (parent-ns ns-name) (fluidinfo-split-name ns)
    (let ((res (fluidinfo-get-namespace ns)))
      (if (first res)
        (progn
          ;; ok result
          (fluidinfo-with-new-active-region (function fluidinfo-make-title-markup) 
              ()
              (insert-string "Namespace '")
            (fluidinfo-with-new-active-region (function fluidinfo-make-ns-markup)
                (list ns)
                (insert-string ns-name))
            (unless (string-equal "" parent-ns)
              (insert-string "' within namespace '")
              (fluidinfo-with-new-active-region (function fluidinfo-make-ns-markup)
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
            (fluidinfo-with-new-active-region (function fluidinfo-make-id-markup) 
                (list id)
                (insert-string id))
            (newline)
            (insert-string "  Description: ")
            (fluidinfo-with-new-active-region (function fluidinfo-make-about-markup)
                (list description)
                (insert-string (format "%s" description)))
            (newline)
            (insert-string "  Belongs to:  ")
            (fluidinfo-with-new-active-region (function fluidinfo-make-user-markup)
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
                       (fluidinfo-with-new-active-region (function fluidinfo-make-ns-markup)
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
                       (fluidinfo-with-new-active-region (function fluidinfo-make-tag-markup)
                           (list (concat ns "/" tag))
                           (insert-string tag))
                       (newline)))))
        (insert-string
         (format "Error getting namespace %s -- %s %s %s" 
                 ns
                 (third res) (fifth res) (sixth res)))))))


(defun fluidinfo-show-query (query)
  (let ((res (fluidinfo-query-objects query)))
    (if (first res)
        (let ((ids (cdr (assoc 'ids (second res)))))
          ;; ok result
          (fluidinfo-with-new-active-region (function fluidinfo-make-title-markup) 
              ()
              (insert-string (format "%d results for query '" (length ids)))
            (fluidinfo-with-new-active-region (function fluidinfo-make-query-markup)
                (list query)
                (insert-string query))
            (insert-string "'"))
          (newline)
          (newline)
          (loop for index from 1
                for id across ids
                do (progn
                     (insert-string (format "  %3d: " index))
                     (fluidinfo-with-new-active-region (function fluidinfo-make-id-markup) 
                         (list id)
                         (insert-string id))
                     (push (list id (point-marker)) fluidinfo-buffer-id-markers)
                     (newline)))
          (newline)
          (newline)
          (insert-string "(press 't' to add object-tag values)")
          (setq fluidinfo-buffer-id-markers (nreverse fluidinfo-buffer-id-markers)))
      (insert-string
       (format "Error performing query %s -- %s %s %s" 
               query
               (third res) (fifth res) (sixth res))))))



(defun fluidinfo-show-this (item add-to-history)
  (fluidinfo-setup-buffer item)
  (setq fluidinfo-browse-current-object item)
  (when add-to-history
    (setq fluidinfo-browse-objects (cons item fluidinfo-browse-objects)))
  
  (setq fluidinfo-buffer-id-markers nil) ;; reset anything in there already

  (ecase (first item)
    (:query (fluidinfo-show-query (second item)))
    (:object (fluidinfo-show-object (second item)))
    (:tag (fluidinfo-show-tag (second item)))
    (:user (fluidinfo-show-user (second item)))
    (:namespace (fluidinfo-show-ns (second item))))

  (fluidinfo-resort-buffer-markers)
  (goto-char (if (cdr fluidinfo-active-regions)
                 (caadr fluidinfo-active-regions)
               (point-min)))
  (toggle-read-only 1)
  (message "done"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluidinfo-browse-object (guid)
  "Browse a specific object in the Fluidinfo"
  (interactive "sGUID: ")
  (if (= (length  guid) 36)
      (let ((action (list :object guid)))
        (fluidinfo-show-this action t))
    (message "Not a valid GUID")))

(defun fluidinfo-browse-namespace (ns)
  "Browse a specific namespace in the Fluidinfo"
  (interactive "sNamespace: ")
  (let ((action (list :namespace ns)))
    (fluidinfo-show-this action t)))

(defun fluidinfo-browse-tag (tag)
  "Browse a specific tag in the Fluidinfo"
  (interactive "sTag: ")
  (let ((action (list :tag tag)))
    (fluidinfo-show-this action t)))

(defun fluidinfo-browse-user (user-name)
  "Browse a specific user in the Fluidinfo"
  (interactive "sUser name: ")
  (let ((action (list :user user-name)))
    (fluidinfo-show-this action t)))


(defun fluidinfo-browse-query (query)
  "Do a query against Fluidinfo and show the results"
  (interactive "sQuery: ")
  (if (string-equal "" query)
      (message "No query given")
    (let ((action (list :query query)))
      (fluidinfo-show-this action t))))



(provide 'fluidinfointerface)
