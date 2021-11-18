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

Next: [Minibuffer Commands](Minibuffer-Commands.html), Previous: [Multiple Queries](Multiple-Queries.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.9 Reading a Password

To read a password to pass to another program, you can use the function `read-passwd`.

*   Function: **read-passwd** *prompt \&optional confirm default*

    This function reads a password, prompting with `prompt`. It does not echo the password as the user types it; instead, it echoes ‘`*`’ for each character in the password. If you want to apply another character to hide the password, let-bind the variable `read-hide-char` with that character.

    The optional argument `confirm`, if non-`nil`, says to read the password twice and insist it must be the same both times. If it isn’t the same, the user has to type it over and over until the last two times match.

    The optional argument `default` specifies the default password to return if the user enters empty input. If `default` is `nil`, then `read-passwd` returns the null string in that case.
