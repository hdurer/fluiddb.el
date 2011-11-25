
fluidinfo.el / fluiddb.el
=========================

fluidinfo.el is a set of Emacs Lisp files to support working with  [Fluidinfo](http://fluidinfo.com/fluiddb).
(fluiddb.el is a legacy file using the old name "fluiddb" where now "fluidinfo" is used.)

This support comes at two levels:
 - low-level support to do all API calls directly from Emacs, and
 - a high-level interface to browse Fluidinfo from within Emacs.

Note that the low-level support is currently only for synchronous calls.  Ideally we'll support a parallel set of asynchronous calls in the future.

The high-level interface is still in development, allows read-only access only so far and is still lacking support to view permissions and policies.


Dependencies
------------

You will need the `json.el` package.  I found mine via EmacsWiki [here](http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs).

The code makes use of url.el which is now shipped with Emacs.  
For older Emacs versions you will need to install that yourself -- very old versions don't seem to work according to user reports but Emacs 22 seems to be ok.


You will also need a user account on the Fluidinfo server (not strictly necessary for the pure browsing operations).  
Either sign up on the web site and then get your password on the `#fluidinfo` IRC channel or send an email to `api@fluidinfo.com`.


The high-level interface
------------------------

Note that this "high-level" interface still requires good
understanding of the Fluidinfo workings; it is not intended for the
general user but for people developing against Fluidinfo to inspect and
change data.

You enter the high-level interface by invoking one of these functions:

 - fluidinfo-browse-user (user-name)
 - fluidinfo-browse-namespace (namespace-name)
 - fluidinfo-browse-tag (namespace-and-tag)
 - fluidinfo-browse-query (query)
 - fluidinfo-browse-object (guid)

You will find yourself in a Fluidinfo specific buffer.

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

The API generally follows the style set by [cl-fluidinfo](http://github.com/hdurer/cl-fluiddb).

If you want to do non-anonymous access, set the *fluidinfo-credentials* variable to a cons of user-name and password.

Available functions are:

 - fluidinfo-get-user (user-name)
 - fluidinfo-get-object (guid)
 - fluidinfo-query-objects (query)
 - fluidinfo-create-object (&optional about)
 - fluidinfo-get-object-tag-value (id tag &optional accept)
 - fluidinfo-set-object-tag-value (id tag contents &optional content-type)
 - fluidinfo-get-namespace (ns)
 - fluidinfo-create-namespace (ns name description)
 - fluidinfo-change-namespace (ns new-description)
 - fluidinfo-delete-namespace (ns)
 - fluidinfo-get-namespace-permissions (namespace action)
 - fluidinfo-set-namespace-permissions (namespace action policy exceptions)
 - fluidinfo-get-tag-permissions (tag action)
 - fluidinfo-set-tag-permissions (tag action policy exceptions)
 - fluidinfo-get-tag-value-permissions (tag action)
 - fluidinfo-set-tag-value-permissions (tag action policy exceptions)
 - fluidinfo-get-policy (user-name category action)
 - fluidinfo-set-policy (user-name category action policy exceptions)
 - fluidinfo-create-tag (ns name description indexed)
 - fluidinfo-get-tag (ns tag)
 - fluidinfo-change-tag (ns tag new-description)
 - fluidinfo-delete-tag (ns tag-description)

Helper functions are:

 - fluidinfo-make-permission-object (policy exceptions)


Installation
------------
Place the `fluidinfo.el` and  `fluidinfointerface.el` files somewhere in your `load-path`.

Then modify your `.emacs.el` (or whatever init file you use) to contain either

    (require 'fluidinfointerface)

or more lazily:
    (autoload 'fluidinfo-browse-user "fluidinfointerface.el" "Browse a specific user in the Fluidinfo" t)
    (autoload 'fluidinfo-browse-namespace "fluidinfointerface.el" "Browse a specific namespace in the Fluidinfo" t) 
    (autoload 'fluidinfo-browse-tag "fluidinfointerface.el" "Browse a specific tag in the Fluidinfo" t)
    (autoload 'fluidinfo-browse-query "fluidinfointerface.el" "Do a query against Fluidinfo and show the results" t)
    (autoload 'fluidinfo-browse-object "fluidinfointerface.el" "Browse a specific object in the Fluidinfo" t)
