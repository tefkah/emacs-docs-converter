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

Next: [Input Events](Input-Events.html), Previous: [Command Loop Info](Command-Loop-Info.html), Up: [Command Loop](Command-Loop.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 21.6 Adjusting Point After Commands

Emacs cannot display the cursor when point is in the middle of a sequence of text that has the `display` or `composition` property, or is invisible. Therefore, after a command finishes and returns to the command loop, if point is within such a sequence, the command loop normally moves point to the edge of the sequence, making this sequence effectively intangible.

A command can inhibit this feature by setting the variable `disable-point-adjustment`:

*   Variable: **disable-point-adjustment**

    If this variable is non-`nil` when a command returns to the command loop, then the command loop does not check for those text properties, and does not move point out of sequences that have them.

    The command loop sets this variable to `nil` before each command, so if a command sets it, the effect applies only to that command.

<!---->

*   Variable: **global-disable-point-adjustment**

    If you set this variable to a non-`nil` value, the feature of moving point out of these sequences is completely turned off.
