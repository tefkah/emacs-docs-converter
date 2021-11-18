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

Next: [Record Type](Record-Type.html), Previous: [Primitive Function Type](Primitive-Function-Type.html), Up: [Programming Types](Programming-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.16 Byte-Code Function Type

*Byte-code function objects* are produced by byte-compiling Lisp code (see [Byte Compilation](Byte-Compilation.html)). Internally, a byte-code function object is much like a vector; however, the evaluator handles this data type specially when it appears in a function call. See [Byte-Code Objects](Byte_002dCode-Objects.html).

The printed representation and read syntax for a byte-code function object is like that for a vector, with an additional ‘`#`’ before the opening ‘`[`’.
