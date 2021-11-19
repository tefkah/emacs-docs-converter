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

Next: [Closures](Closures.html), Previous: [Generic Functions](Generic-Functions.html), Up: [Functions](Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 13.9 Accessing Function Cell Contents

The *function definition* of a symbol is the object stored in the function cell of the symbol. The functions described here access, test, and set the function cell of symbols.

See also the function `indirect-function`. See [Definition of indirect-function](Function-Indirection.html#Definition-of-indirect_002dfunction).

*   Function: **symbol-function** *symbol*

    This returns the object in the function cell of `symbol`. It does not check that the returned object is a legitimate function.

    If the function cell is void, the return value is `nil`. To distinguish between a function cell that is void and one set to `nil`, use `fboundp` (see below).

        (defun bar (n) (+ n 2))
        (symbol-function 'bar)
             ⇒ (lambda (n) (+ n 2))

    <!---->

        (fset 'baz 'bar)
             ⇒ bar

    <!---->

        (symbol-function 'baz)
             ⇒ bar

If you have never given a symbol any function definition, we say that that symbol’s function cell is *void*. In other words, the function cell does not have any Lisp object in it. If you try to call the symbol as a function, Emacs signals a `void-function` error.

Note that void is not the same as `nil` or the symbol `void`. The symbols `nil` and `void` are Lisp objects, and can be stored into a function cell just as any other object can be (and they can be valid functions if you define them in turn with `defun`). A void function cell contains no object whatsoever.

You can test the voidness of a symbol’s function definition with `fboundp`. After you have given a symbol a function definition, you can make it void once more using `fmakunbound`.

*   Function: **fboundp** *symbol*

    This function returns `t` if the symbol has an object in its function cell, `nil` otherwise. It does not check that the object is a legitimate function.

<!---->

*   Function: **fmakunbound** *symbol*

    This function makes `symbol`’s function cell void, so that a subsequent attempt to access this cell will cause a `void-function` error. It returns `symbol`. (See also `makunbound`, in [Void Variables](Void-Variables.html).)

        (defun foo (x) x)
        (foo 1)
             ⇒1

    <!---->

        (fmakunbound 'foo)
             ⇒ foo

    <!---->

        (foo 1)
        error→ Symbol's function definition is void: foo

<!---->

*   Function: **fset** *symbol definition*

    This function stores `definition` in the function cell of `symbol`. The result is `definition`. Normally `definition` should be a function or the name of a function, but this is not checked. The argument `symbol` is an ordinary evaluated argument.

    The primary use of this function is as a subroutine by constructs that define or alter functions, like `defun` or `advice-add` (see [Advising Functions](Advising-Functions.html)). You can also use it to give a symbol a function definition that is not a function, e.g., a keyboard macro (see [Keyboard Macros](Keyboard-Macros.html)):

        ;; Define a named keyboard macro.
        (fset 'kill-two-lines "\^u2\^k")
             ⇒ "\^u2\^k"

    It you wish to use `fset` to make an alternate name for a function, consider using `defalias` instead. See [Definition of defalias](Defining-Functions.html#Definition-of-defalias).

Next: [Closures](Closures.html), Previous: [Generic Functions](Generic-Functions.html), Up: [Functions](Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]