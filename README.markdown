
fluiddb.el
==========

fluiddb.el is a set of Emacs Lisp files to support working with [FluidInfo's](http://fluidinfo.com/) [FluidDb](http://fluidinfo.com/fluiddb).

This support comes at two levels:
 - low-level support to do all API calls directly from Emacs, and
 - a high-level interface to browse FluidDb from within Emacs.

Note that the low-level support is currently only for synchronous calls.  Ideally we'll support a parallel set of asynchronous calls in the future.

The high-level interface is still in active development, allows read-only access only so far and is still lacking support to view permissions and policies.


The high-level interface
------------------------


The low-level interface
-----------------------

The API generally follows the style set by [cl-fluiddb](http://github.com/hdurer/cl-fluiddb).

If you want to do non-anonymous access, set the *fluiddb-credentials* variable to a cons of user-name and password.

Available functions are:


 - fluiddb-get-user (user-name)
 - fluiddb-get-object (guid)
 - fluiddb-query-objects (query)
 - fluiddb-create-object (&optional about)
 - fluiddb-get-object-tag-value (id tag &optional accept)
 - fluiddb-set-object-tag-value (id tag contents &optional content-type)
 - fluiddb-get-namespace (ns)
 - fluiddb-create-namespace (ns name description)
 - fluiddb-change-namespace (ns new-description)
 - fluiddb-delete-namespace (ns)
 - fluiddb-get-namespace-permissions (namespace action)
 - fluiddb-set-namespace-permissions (namespace action policy exceptions)
 - fluiddb-get-tag-permissions (tag action)
 - fluiddb-set-tag-permissions (tag action policy exceptions)
 - fluiddb-get-tag-value-permissions (tag action)
 - fluiddb-set-tag-value-permissions (tag action policy exceptions)
 - fluiddb-get-policy (user-name category action)
 - fluiddb-set-policy (user-name category action policy exceptions)
 - fluiddb-create-tag (ns name description indexed)
 - fluiddb-get-tag (ns tag)
 - fluiddb-change-tag (ns tag new-description)
 - fluiddb-delete-tag (ns tag-description)

Helper functions are:
 - fluiddb-make-permission-object (policy exceptions)
