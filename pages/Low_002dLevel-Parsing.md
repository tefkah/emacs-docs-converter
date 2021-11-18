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

Next: [Control Parsing](Control-Parsing.html), Previous: [Parser State](Parser-State.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 35.6.4 Low-Level Parsing

The most basic way to use the expression parser is to tell it to start at a given position with a certain state, and parse up to a specified end position.

*   Function: **parse-partial-sexp** *start limit \&optional target-depth stop-before state stop-comment*

    This function parses a sexp in the current buffer starting at `start`, not scanning past `limit`. It stops at position `limit` or when certain criteria described below are met, and sets point to the location where parsing stops. It returns a parser state describing the status of the parse at the point where it stops.

    If the third argument `target-depth` is non-`nil`, parsing stops if the depth in parentheses becomes equal to `target-depth`. The depth starts at 0, or at whatever is given in `state`.

    If the fourth argument `stop-before` is non-`nil`, parsing stops when it comes to any character that starts a sexp. If `stop-comment` is non-`nil`, parsing stops after the start of an unnested comment. If `stop-comment` is the symbol `syntax-table`, parsing stops after the start of an unnested comment or a string, or after the end of an unnested comment or a string, whichever comes first.

    If `state` is `nil`, `start` is assumed to be at the top level of parenthesis structure, such as the beginning of a function definition. Alternatively, you might wish to resume parsing in the middle of the structure. To do this, you must provide a `state` argument that describes the initial status of parsing. The value returned by a previous call to `parse-partial-sexp` will do nicely.

Next: [Control Parsing](Control-Parsing.html), Previous: [Parser State](Parser-State.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
