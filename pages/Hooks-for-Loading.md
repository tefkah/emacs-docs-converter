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

Next: [Dynamic Modules](Dynamic-Modules.html), Previous: [Unloading](Unloading.html), Up: [Loading](Loading.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 16.10 Hooks for Loading

You can ask for code to be executed each time Emacs loads a library, by using the variable `after-load-functions`:

*   Variable: **after-load-functions**

    This abnormal hook is run after loading a file. Each function in the hook is called with a single argument, the absolute filename of the file that was just loaded.

If you want code to be executed when a *particular* library is loaded, use the macro `with-eval-after-load`:

*   Macro: **with-eval-after-load** *library body…*

    This macro arranges to evaluate `body` at the end of loading the file `library`, each time `library` is loaded. If `library` is already loaded, it evaluates `body` right away.

    You don’t need to give a directory or extension in the file name `library`. Normally, you just give a bare file name, like this:

        (with-eval-after-load "edebug" (def-edebug-spec c-point t))

    To restrict which files can trigger the evaluation, include a directory or an extension or both in `library`. Only a file whose absolute true name (i.e., the name with all symbolic links chased out) matches all the given name components will match. In the following example, `my_inst.elc` or `my_inst.elc.gz` in some directory `..../foo/bar` will trigger the evaluation, but not `my_inst.el`:

        (with-eval-after-load "foo/bar/my_inst.elc" …)

    `library` can also be a feature (i.e., a symbol), in which case `body` is evaluated at the end of any file where `(provide library)` is called.

    An error in `body` does not undo the load, but does prevent execution of the rest of `body`.

Normally, well-designed Lisp programs should not use `with-eval-after-load`. If you need to examine and set the variables defined in another library (those meant for outside use), you can do it immediately—there is no need to wait until the library is loaded. If you need to call functions defined by that library, you should load the library, preferably with `require` (see [Named Features](Named-Features.html)).

Next: [Dynamic Modules](Dynamic-Modules.html), Previous: [Unloading](Unloading.html), Up: [Loading](Loading.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]