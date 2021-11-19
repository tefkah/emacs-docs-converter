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

Next: [Meta-Char Syntax](Meta_002dChar-Syntax.html), Previous: [General Escape Syntax](General-Escape-Syntax.html), Up: [Character Type](Character-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.3.3 Control-Character Syntax

Control characters can be represented using yet another read syntax. This consists of a question mark followed by a backslash, caret, and the corresponding non-control character, in either upper or lower case. For example, both ‘`?\^I`’ and ‘`?\^i`’ are valid read syntax for the character `C-i`, the character whose value is 9.

Instead of the ‘`^`’, you can use ‘`C-`’; thus, ‘`?\C-i`’ is equivalent to ‘`?\^I`’ and to ‘`?\^i`’:

    ?\^I ⇒ 9     ?\C-I ⇒ 9

In strings and buffers, the only control characters allowed are those that exist in ASCII; but for keyboard input purposes, you can turn any character into a control character with ‘`C-`’. The character codes for these non-ASCII control characters include the 2\*\*26 bit as well as the code for the corresponding non-control character. Ordinary text terminals have no way of generating non-ASCII control characters, but you can generate them straightforwardly using X and other window systems.

For historical reasons, Emacs treats the `DEL` character as the control equivalent of `?`:

    ?\^? ⇒ 127     ?\C-? ⇒ 127

As a result, it is currently not possible to represent the character `Control-?`, which is a meaningful input character under X, using ‘`\C-`’. It is not easy to change this, as various Lisp files refer to `DEL` in this way.

For representing control characters to be found in files or strings, we recommend the ‘`^`’ syntax; for control characters in keyboard input, we prefer the ‘`C-`’ syntax. Which one you use does not affect the meaning of the program, but may guide the understanding of people who read it.

Next: [Meta-Char Syntax](Meta_002dChar-Syntax.html), Previous: [General Escape Syntax](General-Escape-Syntax.html), Up: [Character Type](Character-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
