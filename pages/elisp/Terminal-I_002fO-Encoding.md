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

Previous: [Explicit Encoding](Explicit-Encoding.html), Up: [Coding Systems](Coding-Systems.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 33.10.8 Terminal I/O Encoding

Emacs can use coding systems to decode keyboard input and encode terminal output. This is useful for terminals that transmit or display text using a particular encoding, such as Latin-1. Emacs does not set `last-coding-system-used` when encoding or decoding terminal I/O.

*   Function: **keyboard-coding-system** *\&optional terminal*

    This function returns the coding system used for decoding keyboard input from `terminal`. A value of `no-conversion` means no decoding is done. If `terminal` is omitted or `nil`, it means the selected frame’s terminal. See [Multiple Terminals](Multiple-Terminals.html).

<!---->

*   Command: **set-keyboard-coding-system** *coding-system \&optional terminal*

    This command specifies `coding-system` as the coding system to use for decoding keyboard input from `terminal`. If `coding-system` is `nil`, that means not to decode keyboard input. If `terminal` is a frame, it means that frame’s terminal; if it is `nil`, that means the currently selected frame’s terminal. See [Multiple Terminals](Multiple-Terminals.html).

<!---->

*   Function: **terminal-coding-system** *\&optional terminal*

    This function returns the coding system that is in use for encoding terminal output from `terminal`. A value of `no-conversion` means no encoding is done. If `terminal` is a frame, it means that frame’s terminal; if it is `nil`, that means the currently selected frame’s terminal.

<!---->

*   Command: **set-terminal-coding-system** *coding-system \&optional terminal*

    This command specifies `coding-system` as the coding system to use for encoding terminal output from `terminal`. If `coding-system` is `nil`, that means not to encode terminal output. If `terminal` is a frame, it means that frame’s terminal; if it is `nil`, that means the currently selected frame’s terminal.