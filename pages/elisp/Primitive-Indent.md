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

Next: [Mode-Specific Indent](Mode_002dSpecific-Indent.html), Up: [Indentation](Indentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.17.1 Indentation Primitives

This section describes the primitive functions used to count and insert indentation. The functions in the following sections use these primitives. See [Size of Displayed Text](Size-of-Displayed-Text.html), for related functions.

*   Function: **current-indentation**

    This function returns the indentation of the current line, which is the horizontal position of the first nonblank character. If the contents are entirely blank, then this is the horizontal position of the end of the line.

<!---->

*   Command: **indent-to** *column \&optional minimum*

    This function indents from point with tabs and spaces until `column` is reached. If `minimum` is specified and non-`nil`, then at least that many spaces are inserted even if this requires going beyond `column`. Otherwise the function does nothing if point is already beyond `column`. The value is the column at which the inserted indentation ends.

    The inserted whitespace characters inherit text properties from the surrounding text (usually, from the preceding text only). See [Sticky Properties](Sticky-Properties.html).

<!---->

*   User Option: **indent-tabs-mode**

    If this variable is non-`nil`, indentation functions can insert tabs as well as spaces. Otherwise, they insert only spaces. Setting this variable automatically makes it buffer-local in the current buffer.
