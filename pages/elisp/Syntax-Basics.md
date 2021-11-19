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

Next: [Syntax Descriptors](Syntax-Descriptors.html), Up: [Syntax Tables](Syntax-Tables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 35.1 Syntax Table Concepts

A syntax table is a data structure which can be used to look up the *syntax class* and other syntactic properties of each character. Syntax tables are used by Lisp programs for scanning and moving across text.

Internally, a syntax table is a char-table (see [Char-Tables](Char_002dTables.html)). The element at index `c` describes the character with code `c`; its value is a cons cell which specifies the syntax of the character in question. See [Syntax Table Internals](Syntax-Table-Internals.html), for details. However, instead of using `aset` and `aref` to modify and inspect syntax table contents, you should usually use the higher-level functions `char-syntax` and `modify-syntax-entry`, which are described in [Syntax Table Functions](Syntax-Table-Functions.html).

*   Function: **syntax-table-p** *object*

    This function returns `t` if `object` is a syntax table.

Each buffer has its own major mode, and each major mode has its own idea of the syntax class of various characters. For example, in Lisp mode, the character ‘`;`’ begins a comment, but in C mode, it terminates a statement. To support these variations, the syntax table is local to each buffer. Typically, each major mode has its own syntax table, which it installs in all buffers that use that mode. For example, the variable `emacs-lisp-mode-syntax-table` holds the syntax table used by Emacs Lisp mode, and `c-mode-syntax-table` holds the syntax table used by C mode. Changing a major mode’s syntax table alters the syntax in all of that mode’s buffers, as well as in any buffers subsequently put in that mode. Occasionally, several similar modes share one syntax table. See [Example Major Modes](Example-Major-Modes.html), for an example of how to set up a syntax table.

A syntax table can *inherit* from another syntax table, which is called its *parent syntax table*. A syntax table can leave the syntax class of some characters unspecified, by giving them the “inherit” syntax class; such a character then acquires the syntax class specified by the parent syntax table (see [Syntax Class Table](Syntax-Class-Table.html)). Emacs defines a *standard syntax table*, which is the default parent syntax table, and is also the syntax table used by Fundamental mode.

*   Function: **standard-syntax-table**

    This function returns the standard syntax table, which is the syntax table used in Fundamental mode.

Syntax tables are not used by the Emacs Lisp reader, which has its own built-in syntactic rules which cannot be changed. (Some Lisp systems provide ways to redefine the read syntax, but we decided to leave this feature out of Emacs Lisp for simplicity.)

Next: [Syntax Descriptors](Syntax-Descriptors.html), Up: [Syntax Tables](Syntax-Tables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
