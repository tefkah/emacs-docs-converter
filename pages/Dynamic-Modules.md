<!-- This is the GNU Emacs Lisp Reference Manual
corresponding to Emacs version 27.2.

Copyright (C) 1990-1996, 1998-2021 Free Software Foundation,
Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being "GNU General Public License," with the
Front-Cover Texts being "A GNU Manual," and with the Back-Cover
Texts as in (a) below.  A copy of the license is included in the
section entitled "GNU Free Documentation License."

(a) The FSF's Back-Cover Text is: "You have the freedom to copy and
modify this GNU manual.  Buying copies from the FSF supports it in
developing GNU and promoting software freedom." -->

<!-- Created by GNU Texinfo 6.7, http://www.gnu.org/software/texinfo/ -->

Previous: [Hooks for Loading](Hooks-for-Loading.html), Up: [Loading](Loading.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 16.11 Emacs Dynamic Modules

A *dynamic Emacs module* is a shared library that provides additional functionality for use in Emacs Lisp programs, just like a package written in Emacs Lisp would.

Functions that load Emacs Lisp packages can also load dynamic modules. They recognize dynamic modules by looking at their file-name extension, a.k.a. “suffix”. This suffix is platform-dependent.

*   Variable: **module-file-suffix**

    This variable holds the system-dependent value of the file-name extension of the module files. Its value is `.so` on POSIX hosts and `.dll` on MS-Windows.

Every dynamic module should export a C-callable function named `emacs_module_init`, which Emacs will call as part of the call to `load` or `require` which loads the module. It should also export a symbol named `plugin_is_GPL_compatible` to indicate that its code is released under the GPL or compatible license; Emacs will signal an error if your program tries to load modules that don’t export such a symbol.

If a module needs to call Emacs functions, it should do so through the API (Application Programming Interface) defined and documented in the header file `emacs-module.h` that is part of the Emacs distribution. See [Writing Dynamic Modules](Writing-Dynamic-Modules.html), for details of using that API when writing your own modules.

Modules can create `user-ptr` Lisp objects that embed pointers to C struct’s defined by the module. This is useful for keeping around complex data structures created by a module, to be passed back to the module’s functions. User-ptr objects can also have associated *finalizers* – functions to be run when the object is GC’ed; this is useful for freeing any resources allocated for the underlying data structure, such as memory, open file descriptors, etc. See [Module Values](Module-Values.html).

*   Function: **user-ptrp** *object*

    This function returns `t` if its argument is a `user-ptr` object.

<!---->

*   Function: **module-load** *file*

    Emacs calls this low-level primitive to load a module from the specified `file` and perform the necessary initialization of the module. This is the primitive which makes sure the module exports the `plugin_is_GPL_compatible` symbol, calls the module’s `emacs_module_init` function, and signals an error if that function returns an error indication, or if the use typed `C-g` during the initialization. If the initialization succeeds, `module-load` returns `t`. Note that `file` must already have the proper file-name extension, as this function doesn’t try looking for files with known extensions, unlike `load`.

    Unlike `load`, `module-load` doesn’t record the module in `load-history`, doesn’t print any messages, and doesn’t protect against recursive loads. Most users should therefore use `load`, `load-file`, `load-library`, or `require` instead of `module-load`.

Loadable modules in Emacs are enabled by using the `--with-modules` option at configure time.

***

Previous: [Hooks for Loading](Hooks-for-Loading.html), Up: [Loading](Loading.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
