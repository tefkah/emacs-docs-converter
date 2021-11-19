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

Next: [Event Input Misc](Event-Input-Misc.html), Previous: [Invoking the Input Method](Invoking-the-Input-Method.html), Up: [Reading Input](Reading-Input.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.8.5 Quoted Character Input

You can use the function `read-quoted-char` to ask the user to specify a character, and allow the user to specify a control or meta character conveniently, either literally or as an octal character code. The command `quoted-insert` uses this function.

*   Function: **read-quoted-char** *\&optional prompt*

    This function is like `read-char`, except that if the first character read is an octal digit (0–7), it reads any number of octal digits (but stopping if a non-octal digit is found), and returns the character represented by that numeric character code. If the character that terminates the sequence of octal digits is `RET`, it is discarded. Any other terminating character is used as input after this function returns.

    Quitting is suppressed when the first character is read, so that the user can enter a `C-g`. See [Quitting](Quitting.html).

    If `prompt` is supplied, it specifies a string for prompting the user. The prompt string is always displayed in the echo area, followed by a single ‘`-`’.

    In the following example, the user types in the octal number 177 (which is 127 in decimal).

        (read-quoted-char "What character")

    <!---->

        ---------- Echo Area ----------
        What character 1 7 7-
        ---------- Echo Area ----------

             ⇒ 127
