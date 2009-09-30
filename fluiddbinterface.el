;;; fluiddbinterface.el --- Code to create an Emacs interface to FluidDB
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


(require 'fluiddb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fluiddb-mode-hook nil
  "Fluiddb-mode hook.")
(defvar fluiddb-mode-map (make-sparse-keymap "FluidDB"))

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
  "Major mode for Fluiddb interaction
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
    (let ((km fluiddb-mode-map))
      (define-key km "v" 'fluiddb-browser-view-at-point)
      (define-key km (kbd "RET") 'fluiddb-browser-action-at-point)
      (define-key km "f" 'fluiddb-browser-forward-in-history)
      (define-key km "g" 'fluiddb-browser-reload)
      (define-key km "b" 'fluiddb-browser-backward-in-history)
      (define-key km (kbd "TAB") 'fluiddb-browser-next-region)
      (define-key km (kbd "S-TAB") 'fluiddb-browser-previous-region)
      (define-key km (kbd "<backtab>") 'fluiddb-browser-previous-region)
      ;;(define-key km "\C-c\C-f" 'fluiddb-foo)
      ;;(define-key km "\C-c\C-b" 'fluiddb-bar)
      nil))

(make-variable-buffer-local 'fluiddb-browse-objects)
(make-variable-buffer-local 'fluiddb-browse-current-object)
(make-variable-buffer-local 'fluiddb-active-regions)



(defun fluiddb-split-name (name)
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


(defun fluiddb-get-a-buffer ()
  (let ((confirm-nonexistent-file-or-buffer nil))
    (switch-to-buffer "*fluiddb*"))
  (fluiddb-mode)
  (toggle-read-only 0))

(defmacro fluiddb-with-new-active-region (setup-function setup-args &rest body)
  "Set up an overlay for region produced by body.  Call setup-function on the overlay and push admin info about it into buffer-local list for traversal."
  (let ((start (gensym "start")))
    `(let ((,start (point)))
       ,@body
       (let* ((end (point))
              (overlay (make-overlay ,start end)))
         (apply ,setup-function overlay ,setup-args)
         (push (cons ,start end) fluiddb-active-regions)))))
(put 'fluiddb-with-new-active-region 'lisp-indent-function 3)


(defun fluiddb-setup-buffer (action)
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
  (newline))

(defun fluiddb-do-change-authentication (&optional buffer)
  (interactive)
  (let ((name (read-string "User name (empty for anon access): " (car *fluiddb-credentials*) )))
    (if (string-equal "" name)
        (setf *fluiddb-credentials* nil)
      (let ((password (read-passwd "Password: ")))
        (setf *fluiddb-credentials* (cons name password))))))


(defun fluiddb-browser-next-region ()
  (interactive)
  (let* ((current (point))
         (next (loop for pos in fluiddb-active-regions
                     for start = (car pos)
                     when (> start current)
                     do (return start))))
    (if next
        (goto-char next)
      (message "No next region"))))

(defun fluiddb-browser-previous-region ()
  (interactive)
  (let* ((current (point))
         (next (loop for pos in (reverse fluiddb-active-regions)
                     for start = (car pos)
                     when (< start current)
                     do (return start))))
    (if next
        (goto-char next)
      (message "No previous region"))))



(defun fluiddb-browser-view-at-point ()
  (interactive)
  (let ((action (get-char-property (point) 'view-action)))
    (if action
        (apply (car action) (cdr action))
      (message "Nothing to view at point"))))

(defun fluiddb-browser-action-at-point ()
  (interactive)
  (let ((action (get-char-property (point) 'action)))
    (if action
        (apply (car action) (cdr action))
      (message "Nothing to do at point"))))

(defun fluiddb-make-title-markup (overlay)
  (overlay-put overlay 'face 'bold-italic))

(defun fluiddb-make-id-markup (overlay id)
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action 
               (list (lambda (id) 
                       (fluiddb-browse-object id))
                     id)))

(defun fluiddb-make-tag-markup (overlay tag)
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action 
               (list (lambda (tag)
                       (fluiddb-browse-tag tag))
                     tag)))

(defun fluiddb-make-ns-markup (overlay ns)
  (overlay-put overlay 'face 'bold)
  (overlay-put overlay 'action
               (list (lambda (ns)
                       (fluiddb-browse-namespace ns))
                     ns)))



(defun fluiddb-make-about-markup (overlay text)
  (overlay-put overlay 'face 'italic)
  (overlay-put overlay 'text text)
  (overlay-put overlay 'help-echo "The 'about' text of this object.  'v' to view")
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
    (overlay-put overlay 'help-echo "Tag value.  'v' to view; 'ret' to examine the tag")
    (overlay-put overlay 'action  
                 (list (function fluiddb-browse-tag)
                       tag))
    (overlay-put overlay 'view-action  
                 (list view-tag-value
                       guid tag))))



(defun fluiddb-browser-find-in-history (item)
  (loop for this on fluiddb-browse-objects
        and prev = nil then this
        when (eq (car this) item)
        do (return (values this (car prev)))))

(defun fluiddb-browser-reload ()
  (interactive)
  (fluiddb-show-this fluiddb-browse-current-object nil))


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
          (let ((description (cdr (assoc 'description (second res))))
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
        (insert-string
         (format "Error getting object %s -- %s %s %s %s" 
                 guid
                 (third res) (fifth res) (sixth res)))))))


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
          (let ((description (cdr (assoc 'description (second res))))
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
         (format "Error getting object %s -- %s %s %s %s" 
                 guid
                 (third res) (fifth res) (sixth res)))))))


(defun fluiddb-show-this (item add-to-history)
  (fluiddb-setup-buffer item)
  (setq fluiddb-browse-current-object item)
  (when add-to-history
    (setq fluiddb-browse-objects (cons item fluiddb-browse-objects)))

  (ecase (car item)
    (:object (fluiddb-show-object (cadr item)))
    (:tag (fluiddb-show-tag (cadr item)))
    (:namespace (fluiddb-show-ns (cadr item))))

  (setf fluiddb-active-regions (sort fluiddb-active-regions
                                     (lambda (a b)
                                       (< (car a) (car b)))))
  (goto-char (if fluiddb-active-regions
                 (caar fluiddb-active-regions)
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


(provide 'fluiddbinterface)
