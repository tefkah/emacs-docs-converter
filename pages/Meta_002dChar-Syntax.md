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

Next: [Other Char Bits](Other-Char-Bits.html), Previous: [Ctl-Char Syntax](Ctl_002dChar-Syntax.html), Up: [Character Type](Character-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.3.4 Meta-Character Syntax

A *meta character* is a character typed with the `META` modifier key. The integer that represents such a character has the 2\*\*27 bit set. We use high bits for this and other modifiers to make possible a wide range of basic character codes.

In a string, the 2\*\*7 bit attached to an ASCII character indicates a meta character; thus, the meta characters that can fit in a string have codes in the range from 128 to 255, and are the meta versions of the ordinary ASCII characters. See [Strings of Events](Strings-of-Events.html), for details about `META`-handling in strings.

The read syntax for meta characters uses ‘`\M-`’. For example, ‘`?\M-A`’ stands for `M-A`. You can use ‘`\M-`’ together with octal character codes (see below), with ‘`\C-`’, or with any other syntax for a character. Thus, you can write `M-A` as ‘`?\M-A`’, or as ‘`?\M-\101`’. Likewise, you can write `C-M-b` as ‘`?\M-\C-b`’, ‘`?\C-\M-b`’, or ‘`?\M-\002`’.
