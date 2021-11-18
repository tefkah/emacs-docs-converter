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

Next: [Calling Functions](Calling-Functions.html), Previous: [Function Names](Function-Names.html), Up: [Functions](Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 13.4 Defining Functions

We usually give a name to a function when it is first created. This is called *defining a function*, and we usually do it with the `defun` macro. This section also describes other ways to define a function.

*   Macro: **defun** *name args \[doc] \[declare] \[interactive] body…*

    `defun` is the usual way to define new Lisp functions. It defines the symbol `name` as a function with argument list `args` (see [Argument List](Argument-List.html)) and body forms given by `body`. Neither `name` nor `args` should be quoted.

    `doc`, if present, should be a string specifying the function’s documentation string (see [Function Documentation](Function-Documentation.html)). `declare`, if present, should be a `declare` form specifying function metadata (see [Declare Form](Declare-Form.html)). `interactive`, if present, should be an `interactive` form specifying how the function is to be called interactively (see [Interactive Call](Interactive-Call.html)).

    The return value of `defun` is undefined.

    Here are some examples:

        (defun foo () 5)
        (foo)
             ⇒ 5

    ```
    ```

        (defun bar (a &optional b &rest c)
            (list a b c))
        (bar 1 2 3 4 5)
             ⇒ (1 2 (3 4 5))

    <!---->

        (bar 1)
             ⇒ (1 nil nil)

    <!---->

        (bar)
        error→ Wrong number of arguments.

    ```
    ```

        (defun capitalize-backwards ()
          "Upcase the last letter of the word at point."
          (interactive)
          (backward-word 1)
          (forward-word 1)
          (backward-char 1)
          (capitalize-word 1))

    Be careful not to redefine existing functions unintentionally. `defun` redefines even primitive functions such as `car` without any hesitation or notification. Emacs does not prevent you from doing this, because redefining a function is sometimes done deliberately, and there is no way to distinguish deliberate redefinition from unintentional redefinition.

<!---->

*   Function: **defalias** *name definition \&optional doc*

    This function defines the symbol `name` as a function, with definition `definition` (which can be any valid Lisp function). Its return value is *undefined*.

    If `doc` is non-`nil`, it becomes the function documentation of `name`. Otherwise, any documentation provided by `definition` is used.

    Internally, `defalias` normally uses `fset` to set the definition. If `name` has a `defalias-fset-function` property, however, the associated value is used as a function to call in place of `fset`.

    The proper place to use `defalias` is where a specific function name is being defined—especially where that name appears explicitly in the source file being loaded. This is because `defalias` records which file defined the function, just like `defun` (see [Unloading](Unloading.html)).

    By contrast, in programs that manipulate function definitions for other purposes, it is better to use `fset`, which does not keep such records. See [Function Cells](Function-Cells.html).

You cannot create a new primitive function with `defun` or `defalias`, but you can use them to change the function definition of any symbol, even one such as `car` or `x-popup-menu` whose normal definition is a primitive. However, this is risky: for instance, it is next to impossible to redefine `car` without breaking Lisp completely. Redefining an obscure function such as `x-popup-menu` is less dangerous, but it still may not work as you expect. If there are calls to the primitive from C code, they call the primitive’s C definition directly, so changing the symbol’s definition will have no effect on them.

See also `defsubst`, which defines a function like `defun` and tells the Lisp compiler to perform inline expansion on it. See [Inline Functions](Inline-Functions.html).

To undefine a function name, use `fmakunbound`. See [Function Cells](Function-Cells.html).

Next: [Calling Functions](Calling-Functions.html), Previous: [Function Names](Function-Names.html), Up: [Functions](Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]