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

Next: [Comments](Comments.html), Previous: [Printed Representation](Printed-Representation.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 2.2 Special Read Syntax

Emacs Lisp represents many special objects and constructs via special hash notations.

*   ‘`#<…>`’

    Objects that have no read syntax are presented like this (see [Printed Representation](Printed-Representation.html)).

*   ‘`##`’

    The printed representation of an interned symbol whose name is an empty string (see [Symbol Type](Symbol-Type.html)).

*   ‘`#'`’

    This is a shortcut for `function`, see [Anonymous Functions](Anonymous-Functions.html).

*   ‘`#:`’

    The printed representation of an uninterned symbol whose name is `foo` is ‘`#:foo`’ (see [Symbol Type](Symbol-Type.html)).

*   ‘`#N`’

    When printing circular structures, this construct is used to represent where the structure loops back onto itself, and ‘`N`’ is the starting list count:

        (let ((a (list 1)))
          (setcdr a a))
        => (1 . #0)

*   *   ‘`#N=`’
    *   ‘`#N#`’

    ‘`#N=`’ gives the name to an object, and ‘`#N#`’ represents that object, so when reading back the object, they will be the same object instead of copies (see [Circular Objects](Circular-Objects.html)).

*   ‘`#xN`’

    ‘`N`’ represented as a hexadecimal number (‘`#x2a`’).

*   ‘`#oN`’

    ‘`N`’ represented as an octal number (‘`#o52`’).

*   ‘`#bN`’

    ‘`N`’ represented as a binary number (‘`#b101010`’).

*   ‘`#(…)`’

    String text properties (see [Text Props and Strings](Text-Props-and-Strings.html)).

*   ‘`#^`’

    A char table (see [Char-Table Type](Char_002dTable-Type.html)).

*   ‘`#s(hash-table …)`’

    A hash table (see [Hash Table Type](Hash-Table-Type.html)).

*   ‘`?C`’

    A character (see [Basic Char Syntax](Basic-Char-Syntax.html)).

*   ‘`#$`’

    The current file name in byte-compiled files (see [Docs and Compilation](Docs-and-Compilation.html)). This is not meant to be used in Emacs Lisp source files.

*   ‘`#@N`’

    Skip the next ‘`N`’ characters (see [Comments](Comments.html)). This is used in byte-compiled files, and is not meant to be used in Emacs Lisp source files.

Next: [Comments](Comments.html), Previous: [Printed Representation](Printed-Representation.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]