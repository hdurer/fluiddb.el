;;; fluiddb.el --- Legacy function to work with FluidInfo
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

;; Fluidinfo used to be called FluidDB and the first versions of this
;; code used that name prefix.
;; The code in this file exists to restore these old names for those
;; who haven't migrated to the new name prefix yet.

(require 'fluidinfo)

(defalias 'fluiddb-make-permission-object 'fluidinfo-make-permission-object)
(defalias 'fluiddb-get-user 'fluidinfo-get-user)
(defalias 'fluiddb-get-object 'fluidinfo-get-object)
(defalias 'fluiddb-get-object-about 'fluidinfo-get-object-about)
(defalias 'fluiddb-query-objects 'fluidinfo-query-objects)
(defalias 'fluiddb-query-objects-tag-values 'fluidinfo-query-objects-tag-values)
(defalias 'fluiddb-set-objects-tag-values 'fluidinfo-set-objects-tag-values)
(defalias 'fluiddb-delete-objects-tag-values 'fluidinfo-delete-objects-tag-values)
(defalias 'fluiddb-create-object 'fluidinfo-create-object)
(defalias 'fluiddb-get-object-tag-value 'fluidinfo-get-object-tag-value)
(defalias 'fluiddb-get-object-about-tag-value 'fluidinfo-get-object-about-tag-value)
(defalias 'fluiddb-object-tag-has-value-p 'fluidinfo-object-tag-has-value-p)
(defalias 'fluiddb-object-about-tag-has-value-p 'fluidinfo-object-about-tag-has-value-p)
(defalias 'fluiddb-set-object-tag-value 'fluidinfo-set-object-tag-value)
(defalias 'fluiddb-set-object-about-tag-value 'fluidinfo-set-object-about-tag-value)
(defalias 'fluiddb-delete-object-tag-value 'fluidinfo-delete-object-tag-value)
(defalias 'fluiddb-delete-object-about-tag-value 'fluidinfo-delete-object-about-tag-value)
(defalias 'fluiddb-get-namespace 'fluidinfo-get-namespace)
(defalias 'fluiddb-create-namespace 'fluidinfo-create-namespace)
(defalias 'fluiddb-change-namespace 'fluidinfo-change-namespace)
(defalias 'fluiddb-delete-namespace 'fluidinfo-delete-namespace)
(defalias 'fluiddb-get-namespace-permissions 'fluidinfo-get-namespace-permissions)
(defalias 'fluiddb-set-namespace-permissions 'fluidinfo-set-namespace-permissions)
(defalias 'fluiddb-get-tag-permissions 'fluidinfo-get-tag-permissions)
(defalias 'fluiddb-set-tag-permissions 'fluidinfo-set-tag-permissions)
(defalias 'fluiddb-get-tag-value-permissions 'fluidinfo-get-tag-value-permissions)
(defalias 'fluiddb-set-tag-value-permissions 'fluidinfo-set-tag-value-permissions)
(defalias 'fluiddb-get-policy 'fluidinfo-get-policy)
(defalias 'fluiddb-set-policy 'fluidinfo-set-policy)
(defalias 'fluiddb-create-tag 'fluidinfo-create-tag)
(defalias 'fluiddb-get-tag 'fluidinfo-get-tag)
(defalias 'fluiddb-change-tag 'fluidinfo-change-tag)
(defalias 'fluiddb-delete-tag 'fluidinfo-delete-tag)

(provide 'fluiddb)
