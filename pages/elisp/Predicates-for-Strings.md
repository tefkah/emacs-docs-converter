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

Next: [Creating Strings](Creating-Strings.html), Previous: [String Basics](String-Basics.html), Up: [Strings and Characters](Strings-and-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 4.2 Predicates for Strings

For more information about general sequence and array predicates, see [Sequences Arrays Vectors](Sequences-Arrays-Vectors.html), and [Arrays](Arrays.html).

*   Function: **stringp** *object*

    This function returns `t` if `object` is a string, `nil` otherwise.

<!---->

*   Function: **string-or-null-p** *object*

    This function returns `t` if `object` is a string or `nil`. It returns `nil` otherwise.

<!---->

*   Function: **char-or-string-p** *object*

    This function returns `t` if `object` is a string or a character (i.e., an integer), `nil` otherwise.
