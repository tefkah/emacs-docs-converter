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

Next: [Parsing Expressions](Parsing-Expressions.html), Previous: [Syntax Properties](Syntax-Properties.html), Up: [Syntax Tables](Syntax-Tables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 35.5 Motion and Syntax

This section describes functions for moving across characters that have certain syntax classes.

*   Function: **skip-syntax-forward** *syntaxes \&optional limit*

    This function moves point forward across characters having syntax classes mentioned in `syntaxes` (a string of syntax class characters). It stops when it encounters the end of the buffer, or position `limit` (if specified), or a character it is not supposed to skip.

    If `syntaxes` starts with ‘`^`’, then the function skips characters whose syntax is *not* in `syntaxes`.

    The return value is the distance traveled, which is a nonnegative integer.

<!---->

*   Function: **skip-syntax-backward** *syntaxes \&optional limit*

    This function moves point backward across characters whose syntax classes are mentioned in `syntaxes`. It stops when it encounters the beginning of the buffer, or position `limit` (if specified), or a character it is not supposed to skip.

    If `syntaxes` starts with ‘`^`’, then the function skips characters whose syntax is *not* in `syntaxes`.

    The return value indicates the distance traveled. It is an integer that is zero or less.

<!---->

*   Function: **backward-prefix-chars**

    This function moves point backward over any number of characters with expression prefix syntax. This includes both characters in the expression prefix syntax class, and characters with the ‘`p`’ flag.
