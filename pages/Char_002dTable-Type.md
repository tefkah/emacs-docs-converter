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

Next: [Bool-Vector Type](Bool_002dVector-Type.html), Previous: [Vector Type](Vector-Type.html), Up: [Programming Types](Programming-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.10 Char-Table Type

A *char-table* is a one-dimensional array of elements of any type, indexed by character codes. Char-tables have certain extra features to make them more useful for many jobs that involve assigning information to character codes—for example, a char-table can have a parent to inherit from, a default value, and a small number of extra slots to use for special purposes. A char-table can also specify a single value for a whole character set.

The printed representation of a char-table is like a vector except that there is an extra ‘`#^`’ at the beginning.[1](#FOOT1)

See [Char-Tables](Char_002dTables.html), for special functions to operate on char-tables. Uses of char-tables include:

*   Case tables (see [Case Tables](Case-Tables.html)).
*   Character category tables (see [Categories](Categories.html)).
*   Display tables (see [Display Tables](Display-Tables.html)).
*   Syntax tables (see [Syntax Tables](Syntax-Tables.html)).

***

#### Footnotes

##### [(1)](#DOCF1)

You may also encounter ‘`#^^`’, used for sub-char-tables.
