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

Next: [Trace Buffer](Trace-Buffer.html), Previous: [Eval List](Eval-List.html), Up: [Edebug](Edebug.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.11 Printing in Edebug

If an expression in your program produces a value containing circular list structure, you may get an error when Edebug attempts to print it.

One way to cope with circular structure is to set `print-length` or `print-level` to truncate the printing. Edebug does this for you; it binds `print-length` and `print-level` to the values of the variables `edebug-print-length` and `edebug-print-level` (so long as they have non-`nil` values). See [Output Variables](Output-Variables.html).

*   User Option: **edebug-print-length**

    If non-`nil`, Edebug binds `print-length` to this value while printing results. The default value is `50`.

<!---->

*   User Option: **edebug-print-level**

    If non-`nil`, Edebug binds `print-level` to this value while printing results. The default value is `50`.

You can also print circular structures and structures that share elements more informatively by binding `print-circle` to a non-`nil` value.

Here is an example of code that creates a circular structure:

    (setq a (list 'x 'y))
    (setcar a a)

Custom printing prints this as ‘`Result: #1=(#1# y)`’. The ‘`#1=`’ notation labels the structure that follows it with the label ‘`1`’, and the ‘`#1#`’ notation references the previously labeled structure. This notation is used for any shared elements of lists or vectors.

*   User Option: **edebug-print-circle**

    If non-`nil`, Edebug binds `print-circle` to this value while printing results. The default value is `t`.

Other programs can also use custom printing; see `cust-print.el` for details.