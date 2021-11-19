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

Previous: [Low-Level Parsing](Low_002dLevel-Parsing.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 35.6.5 Parameters to Control Parsing

*   Variable: **multibyte-syntax-as-symbol**

    If this variable is non-`nil`, `scan-sexps` treats all non-ASCII characters as symbol constituents regardless of what the syntax table says about them. (However, `syntax-table `text properties can still override the syntax.)

<!---->

*   User Option: **parse-sexp-ignore-comments**

    If the value is non-`nil`, then comments are treated as whitespace by the functions in this section and by `forward-sexp`, `scan-lists` and `scan-sexps`.

The behavior of `parse-partial-sexp` is also affected by `parse-sexp-lookup-properties` (see [Syntax Properties](Syntax-Properties.html)).

*   Variable: **comment-end-can-be-escaped**

    If this buffer local variable is non-`nil`, a single character which usually terminates a comment doesn’t do so when that character is escaped. This is used in C and C++ Modes, where line comments starting with ‘`//`’ can be continued onto the next line by escaping the newline with ‘`\`’.

You can use `forward-comment` to move forward or backward over one comment or several comments.
