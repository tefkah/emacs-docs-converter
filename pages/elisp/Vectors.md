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

Next: [Vector Functions](Vector-Functions.html), Previous: [Array Functions](Array-Functions.html), Up: [Sequences Arrays Vectors](Sequences-Arrays-Vectors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 6.4 Vectors

A *vector* is a general-purpose array whose elements can be any Lisp objects. (By contrast, the elements of a string can only be characters. See [Strings and Characters](Strings-and-Characters.html).) Vectors are used in Emacs for many purposes: as key sequences (see [Key Sequences](Key-Sequences.html)), as symbol-lookup tables (see [Creating Symbols](Creating-Symbols.html)), as part of the representation of a byte-compiled function (see [Byte Compilation](Byte-Compilation.html)), and more.

Like other arrays, vectors use zero-origin indexing: the first element has index 0.

Vectors are printed with square brackets surrounding the elements. Thus, a vector whose elements are the symbols `a`, `b` and `a` is printed as `[a b a]`. You can write vectors in the same way in Lisp input.

A vector, like a string or a number, is considered a constant for evaluation: the result of evaluating it is the same vector. This does not evaluate or even examine the elements of the vector. See [Self-Evaluating Forms](Self_002dEvaluating-Forms.html). Vectors written with square brackets should not be modified via `aset` or other destructive operations. See [Mutability](Mutability.html).

Here are examples illustrating these principles:

    (setq avector [1 two '(three) "four" [five]])
         ⇒ [1 two '(three) "four" [five]]
    (eval avector)
         ⇒ [1 two '(three) "four" [five]]
    (eq avector (eval avector))
         ⇒ t
