
fluiddb.el
==========

fluiddb.el is a set of Emacs Lisp files to support working with [FluidInfo's](http://fluidinfo.com/) [FluidDb](http://fluidinfo.com/fluiddb).

This support comes at two levels:
 - low-level support to do all API calls directly from Emacs, and
 - a high-level interface to browse FluidDb from within Emacs.

Note that the low-level support is currently only for synchronous calls.  Ideally we'll support a parallel set of asynchronous calls in the future.

The high-level interface is still in active development, allows read-only access only so far and is still lacking support to view permissions and policies.


Dependencies
------------

You will need the `json.el` package.  I found mine via EmacsWiki [here](http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs).

The code makes use of url.el which is now shipped with Emacs.  
For older Emacs versions you will need to install that yourself -- very old versions don't seem to work according to user reports but Emacs 22 seems to be ok.


You will also need a user account on the FluidDB server (not strictly necessary for the pure browsing operations).  
Either sign up on the web site and then get your password on the `#fluiddb` IRC channel or send an email to `api@fluidinfo.com`.


The high-level interface
------------------------

Note that this "high-level" interface still requires good
understanding of the FluidDB workings; it is not intended for the
general user but for people developing against FluidDB to inspect and
change data.

You enter the high-level interface by invoking one of these functions:

 - fluiddb-browse-user (user-name)
 - fluiddb-browse-namespace (namespace-name)
 - fluiddb-browse-tag (namespace-and-tag)
 - fluiddb-browse-query (query)
 - fluiddb-browse-object (guid)

You will find yourself in a FluidDb specific buffer.

At any time the `g` key will re-load the current buffer (re-fetch the
data and completely re-draw the buffer).

Use the `b` and `f` keys to navigate in the browsing history (just
like the back and forward button on your web browser).

Use `TAB` and `S-TAB` to navigate the active regions of the buffer.

While the cursor is on an active region you can usually press `RET` to
do the common browsing action for the current object (e.g. to browse
to that object when on a tag name, user name, etc.).  Often `v` works
to view that object in a temporary buffer.

The key sequences `B u`, `B n`, `B t`, `B q`, and `B o` can be used
browse a complete unrelated instance (user, namespace, tag, query, and
object respectively) not reachable via some active region.

Where it makes sense (currently implemented only for the query results
view) you can press `t` to fetch object tag values for the shown
object and tags.



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


Installation
------------
Place the `fluiddb.el` and  `fluiddbinterface.el` files somewhere in your `load-path`.

Then modify your `.emacs.el` (or whatever init file you use) to contain either

    (require 'fluiddbinterface)

or more lazily:
    (autoload 'fluiddb-browse-user "fluiddbinterface.el" "Browse a specific user in the FluidDB" t)
    (autoload 'fluiddb-browse-namespace "fluiddbinterface.el" "Browse a specific namespace in the FluidDB" t) 
    (autoload 'fluiddb-browse-tag "fluiddbinterface.el" "Browse a specific tag in the FluidDB" t)
    (autoload 'fluiddb-browse-query "fluiddbinterface.el" "Do a query against FluidDB and show the results" t)
    (autoload 'fluiddb-browse-object "fluiddbinterface.el" "Browse a specific object in the FluidDB" t)
