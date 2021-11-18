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

Next: [Function Forms](Function-Forms.html), Previous: [Classifying Lists](Classifying-Lists.html), Up: [Forms](Forms.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 10.2.4 Symbol Function Indirection

If the first element of the list is a symbol then evaluation examines the symbol’s function cell, and uses its contents instead of the original symbol. If the contents are another symbol, this process, called *symbol function indirection*, is repeated until it obtains a non-symbol. See [Function Names](Function-Names.html), for more information about symbol function indirection.

One possible consequence of this process is an infinite loop, in the event that a symbol’s function cell refers to the same symbol. Otherwise, we eventually obtain a non-symbol, which ought to be a function or other suitable object.

More precisely, we should now have a Lisp function (a lambda expression), a byte-code function, a primitive function, a Lisp macro, a special form, or an autoload object. Each of these types is a case described in one of the following sections. If the object is not one of these types, Emacs signals an `invalid-function` error.

The following example illustrates the symbol indirection process. We use `fset` to set the function cell of a symbol and `symbol-function` to get the function cell contents (see [Function Cells](Function-Cells.html)). Specifically, we store the symbol `car` into the function cell of `first`, and the symbol `first` into the function cell of `erste`.

    ;; Build this function cell linkage:
    ;;   -------------       -----        -------        -------
    ;;  | #<subr car> | <-- | car |  <-- | first |  <-- | erste |
    ;;   -------------       -----        -------        -------

<!---->

    (symbol-function 'car)
         ⇒ #<subr car>

<!---->

    (fset 'first 'car)
         ⇒ car

<!---->

    (fset 'erste 'first)
         ⇒ first

<!---->

    (erste '(1 2 3))   ; Call the function referenced by erste.
         ⇒ 1

By contrast, the following example calls a function without any symbol function indirection, because the first element is an anonymous Lisp function, not a symbol.

    ((lambda (arg) (erste arg))
     '(1 2 3))
         ⇒ 1

Executing the function itself evaluates its body; this does involve symbol function indirection when calling `erste`.

This form is rarely used and is now deprecated. Instead, you should write it as:

    (funcall (lambda (arg) (erste arg))
             '(1 2 3))

or just

    (let ((arg '(1 2 3))) (erste arg))

The built-in function `indirect-function` provides an easy way to perform symbol function indirection explicitly.

*   Function: **indirect-function** *function \&optional noerror*

    This function returns the meaning of `function` as a function. If `function` is a symbol, then it finds `function`’s function definition and starts over with that value. If `function` is not a symbol, then it returns `function` itself.

    This function returns `nil` if the final symbol is unbound. It signals a `cyclic-function-indirection` error if there is a loop in the chain of symbols.

    The optional argument `noerror` is obsolete, kept for backward compatibility, and has no effect.

    Here is how you could define `indirect-function` in Lisp:

        (defun indirect-function (function)
          (if (symbolp function)
              (indirect-function (symbol-function function))
            function))

Next: [Function Forms](Function-Forms.html), Previous: [Classifying Lists](Classifying-Lists.html), Up: [Forms](Forms.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
