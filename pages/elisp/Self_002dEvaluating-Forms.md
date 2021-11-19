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

Next: [Symbol Forms](Symbol-Forms.html), Up: [Forms](Forms.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 10.2.1 Self-Evaluating Forms

A *self-evaluating form* is any form that is not a list or symbol. Self-evaluating forms evaluate to themselves: the result of evaluation is the same object that was evaluated. Thus, the number 25 evaluates to 25, and the string `"foo"` evaluates to the string `"foo"`. Likewise, evaluating a vector does not cause evaluation of the elements of the vector—it returns the same vector with its contents unchanged.

    '123               ; A number, shown without evaluation.
         ⇒ 123

<!---->

    123                ; Evaluated as usual—result is the same.
         ⇒ 123

<!---->

    (eval '123)        ; Evaluated "by hand"—result is the same.
         ⇒ 123

<!---->

    (eval (eval '123)) ; Evaluating twice changes nothing.
         ⇒ 123

A self-evaluating form yields a value that becomes part of the program, and you should not try to modify it via `setcar`, `aset` or similar operations. The Lisp interpreter might unify the constants yielded by your program’s self-evaluating forms, so that these constants might share structure. See [Mutability](Mutability.html).

It is common to write numbers, characters, strings, and even vectors in Lisp code, taking advantage of the fact that they self-evaluate. However, it is quite unusual to do this for types that lack a read syntax, because there’s no way to write them textually. It is possible to construct Lisp expressions containing these types by means of a Lisp program. Here is an example:

    ;; Build an expression containing a buffer object.
    (setq print-exp (list 'print (current-buffer)))
         ⇒ (print #<buffer eval.texi>)

<!---->

    ;; Evaluate it.
    (eval print-exp)
         -| #<buffer eval.texi>
         ⇒ #<buffer eval.texi>

Next: [Symbol Forms](Symbol-Forms.html), Up: [Forms](Forms.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
