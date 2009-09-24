;; fluiddbutils.el --- Some utility routines based on fluiddb I found useful
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


(defun fluiddb-query-and-fetch (query tags-to-fetch)
  "Perform 'query' and fetch all 'tags-to-fetch' for all objects found.
Returns a list of items, each item is a list comprising of 
a) the guid of the object, and
b) the list of values for each tag given.

In b) a value will be either nil if the value could not be retrieved
or a list of (value mime-type) for the value"
  (flet ((fetch-tag (guid tag)
                    (let ((res (fluiddb-get-object-tag-value guid tag "*/*")))
                      (message "fetch-tag %s %s -> %s %s" guid tag (second res) (third res))
                      (if (car res)
                          (list (second res) (fourth res))
                        nil))))
    (let* ((res (fluiddb-query-objects query))
           (ids (and (car res)
                     (coerce (cdr (assoc 'ids (second res)))
                             'list))))
      (message "Query >>%s<< returns %s: %s (%s)" query (length ids) ids res)
      (if (car res)
          (progn
            (if (null ids)
                (message "No results found from query"))
            (loop for guid in ids
                  collecting (cons guid
                                   (loop for tag in tags
                                         collecting (fetch-tag guid tag)))))
        (message "Query failed with code %s" (third res))))))




(defun fluiddb-query-and-fetch-and-insert-result (query tags-to-fetch)
  "Query the DB and fetch all 'tags-to-fetch' (a string space separated names).
Inserts all tag values in a row -- one row per result"
  (interactive "sQuery:\nsTags to fetch:")
  (let ((tags (split-string tags-to-fetch)))
    (cond 
     ((string-equal "" query)
      (message "No query given"))
     ((null tags)
      (message "No tags to fetch"))
     (t
      (let ((res (fluiddb-query-and-fetch query tags-to-fetch)))
        (loop for (guid . tag-values) in res
              do (progn
                   (loop for tag-value in tag-values
                         for first = t then nil
                         do (progn
                              (unless first (insert "	"))
                              (when tag-value (insert (format "%s" (first tag-value))))))
                   (newline))))))))



