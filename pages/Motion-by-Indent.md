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

Previous: [Indent Tabs](Indent-Tabs.html), Up: [Indentation](Indentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.17.6 Indentation-Based Motion Commands

These commands, primarily for interactive use, act based on the indentation in the text.

*   Command: **back-to-indentation**

    This command moves point to the first non-whitespace character in the current line (which is the line in which point is located). It returns `nil`.

<!---->

*   Command: **backward-to-indentation** *\&optional arg*

    This command moves point backward `arg` lines and then to the first nonblank character on that line. It returns `nil`. If `arg` is omitted or `nil`, it defaults to 1.

<!---->

*   Command: **forward-to-indentation** *\&optional arg*

    This command moves point forward `arg` lines and then to the first nonblank character on that line. It returns `nil`. If `arg` is omitted or `nil`, it defaults to 1.
