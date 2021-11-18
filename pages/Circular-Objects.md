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

Next: [Type Predicates](Type-Predicates.html), Previous: [Editing Types](Editing-Types.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 2.6 Read Syntax for Circular Objects

To represent shared or circular structures within a complex of Lisp objects, you can use the reader constructs ‘`#n=`’ and ‘`#n#`’.

Use `#n=` before an object to label it for later reference; subsequently, you can use `#n#` to refer the same object in another place. Here, `n` is some integer. For example, here is how to make a list in which the first element recurs as the third element:

    (#1=(a) b #1#)

This differs from ordinary syntax such as this

    ((a) b (a))

which would result in a list whose first and third elements look alike but are not the same Lisp object. This shows the difference:

    (prog1 nil
      (setq x '(#1=(a) b #1#)))
    (eq (nth 0 x) (nth 2 x))
         ⇒ t
    (setq x '((a) b (a)))
    (eq (nth 0 x) (nth 2 x))
         ⇒ nil

You can also use the same syntax to make a circular structure, which appears as an element within itself. Here is an example:

    #1=(a #1#)

This makes a list whose second element is the list itself. Here’s how you can see that it really works:

    (prog1 nil
      (setq x '#1=(a #1#)))
    (eq x (cadr x))
         ⇒ t

The Lisp printer can produce this syntax to record circular and shared structure in a Lisp object, if you bind the variable `print-circle` to a non-`nil` value. See [Output Variables](Output-Variables.html).

Next: [Type Predicates](Type-Predicates.html), Previous: [Editing Types](Editing-Types.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
