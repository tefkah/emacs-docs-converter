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

Next: [Evaluation Notation](Evaluation-Notation.html), Previous: [Some Terms](Some-Terms.html), Up: [Conventions](Conventions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 1.3.2 `nil` and `t`

In Emacs Lisp, the symbol `nil` has three separate meanings: it is a symbol with the name ‘`nil`’; it is the logical truth value `false`; and it is the empty list—the list of zero elements. When used as a variable, `nil` always has the value `nil`.

As far as the Lisp reader is concerned, ‘`()`’ and ‘`nil`’ are identical: they stand for the same object, the symbol `nil`. The different ways of writing the symbol are intended entirely for human readers. After the Lisp reader has read either ‘`()`’ or ‘`nil`’, there is no way to determine which representation was actually written by the programmer.

In this manual, we write `()` when we wish to emphasize that it means the empty list, and we write `nil` when we wish to emphasize that it means the truth value `false`. That is a good convention to use in Lisp programs also.

    (cons 'foo ())                ; Emphasize the empty list
    (setq foo-flag nil)           ; Emphasize the truth value false

In contexts where a truth value is expected, any non-`nil` value is considered to be `true`. However, `t` is the preferred way to represent the truth value `true`. When you need to choose a value that represents `true`, and there is no other basis for choosing, use `t`. The symbol `t` always has the value `t`.

In Emacs Lisp, `nil` and `t` are special symbols that always evaluate to themselves. This is so that you do not need to quote them to use them as constants in a program. An attempt to change their values results in a `setting-constant` error. See [Constant Variables](Constant-Variables.html).

*   Function: **booleanp** *object*

    Return non-`nil` if `object` is one of the two canonical boolean values: `t` or `nil`.

Next: [Evaluation Notation](Evaluation-Notation.html), Previous: [Some Terms](Some-Terms.html), Up: [Conventions](Conventions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
