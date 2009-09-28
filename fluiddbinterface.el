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
  (kill-all-local-variables)
  (fluiddb-mode-init-variables)
  (use-local-map fluiddb-mode-map)
  (setq major-mode 'fluiddb-mode)
  (setq mode-name "FluidDB")
  ;;(set-syntax-table fluiddb-mode-syntax-table)
  (run-mode-hooks 'fluiddb-mode-hook)
  (font-lock-mode -1)
  (buffer-disable-undo (current-buffer)))


(if fluiddb-mode-map
    (let ((km fluiddb-mode-map))
      (define-key km "v" 'fluiddb-browser-view-at-point)
      ;;(define-key km "\C-c\C-f" 'fluiddb-foo)
      ;;(define-key km "\C-c\C-b" 'fluiddb-bar)
      nil))

(make-variable-buffer-local 'fluiddb-browse-object)

(defun fluiddb-get-a-buffer ()
  (let ((confirm-nonexistent-file-or-buffer nil))
    (switch-to-buffer "*fluiddb*"))
  (fluiddb-mode)
  (toggle-read-only 0))


(defun fluiddb-browser-view-at-point ()
  (interactive)
  (let ((action (get-char-property (point) 'view-action)))
    (if action
        (apply (car action) (cdr action))
      (message "Nothing to view at point"))))

(defun fluiddb-make-title-markup (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'bold-italic)))


(defun fluiddb-make-about-markup (start end text)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'italic)
    (overlay-put overlay 'text text)
    (overlay-put overlay 'help-echo "The 'about' text of this object.  'v' to view")
    (let ((view-action (lambda (text)
                         (with-output-to-temp-buffer "*FluidDB about text*"
                           (princ text)))))
      (overlay-put overlay 'view-action (list view-action text))
      (overlay-put overlay 'action (list view-action text)))))

(defun fluiddb-make-tag-value-markup (start end guid tag)
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
                                        (let ((start (point)))
                                          (insert-string value)
                                          (let ((overlay (make-overlay start (point))))
                                            (overlay-put overlay 'display 
                                                         (create-image value 
                                                                       (intern (second 
                                                                                (split-string mime-type "/")))
                                                                       t)))))
                                       (t
                                        (insert-string (format "Value:  %s" value)))))
                                  (insert-string (format "Failed to get tag value %s for %s: %s %s %s"
                                                         tag-name guid (third res)
                                                         (fifth res) (sixth res)))))))))
        (overlay (make-overlay start end)))
    (overlay-put overlay 'face 'bold)
    (overlay-put overlay 'help-echo "The 'about' text of this object.  'v' to view; 'ret' to examine the tag")
    (overlay-put overlay 'action  
                 (list (lambda (tag)
                         (fluiddb-show-this (list :tag tag)))
                       tag))
    (overlay-put overlay 'view-action  
                 (list view-tag-value
                       guid tag))))



(defun fluiddb-show-object (guid)
  (let ((res (fluiddb-get-object guid)))
    (if (first res)
        (progn
          ;; ok result
          (let ((start (point)))
            (insert-string (format "Object %s: " guid))
            (fluiddb-make-title-markup start (point)))
          (newline)
          (let ((about (assoc 'about (second res)))
                (tags (cdr (assoc 'tagPaths (second res)))))
            (if about
                (progn
                  (insert-string "  About: ")
                  (let ((start (point)))
                    (insert-string (format "%s" (cdr about)))
                    (fluiddb-make-about-markup start (point) (cdr about))))
              (insert-string "  (Anonymous -- no 'about' string)"))
            (newline)
            (insert-string (format "  %s tags:" (length tags)))
            (newline)
            (loop 
             for index from 1
             for tag-name across tags
             do (progn
                  (insert-string (format "  %3d: " index))
                  (let ((start (point)))
                    (insert-string tag-name)
                    (fluiddb-make-tag-value-markup start (point) guid tag-name))
                  (newline)))))
      (insert-string
       (format "Error getting object %s -- %s %s %s %s" 
               guid
               (third res) (fifth res) (sixth res))))))


(defun fluiddb-show-this (item)
  (setq fluiddb-browse-object item)
  (erase-buffer)
  (goto-char (point-min))

  (ecase (car item)
    (:object (fluiddb-show-object (cadr item))))
  (toggle-read-only 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fluiddb-browse-object (guid)
  "Browse a specific object in the FluidDB"
  (interactive "sGUID: ")
  (fluiddb-get-a-buffer)
  (if (= (length  guid) 36)
      (fluiddb-show-this (list :object guid))
    (message "Not a valid GUID")))


(provide 'fluiddbinterface)
