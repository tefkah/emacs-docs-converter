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

Next: [Derived Modes](Derived-Modes.html), Previous: [Auto Major Mode](Auto-Major-Mode.html), Up: [Major Modes](Major-Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.2.3 Getting Help about a Major Mode

The `describe-mode` function provides information about major modes. It is normally bound to `C-h m`. It uses the value of the variable `major-mode` (see [Major Modes](Major-Modes.html)), which is why every major mode command needs to set that variable.

*   Command: **describe-mode** *\&optional buffer*

    This command displays the documentation of the current buffer’s major mode and minor modes. It uses the `documentation` function to retrieve the documentation strings of the major and minor mode commands (see [Accessing Documentation](Accessing-Documentation.html)).

    If called from Lisp with a non-`nil` `buffer` argument, this function displays the documentation for that buffer’s major and minor modes, rather than those of the current buffer.
