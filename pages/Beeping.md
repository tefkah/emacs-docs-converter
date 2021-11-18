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

Next: [Window Systems](Window-Systems.html), Previous: [Character Display](Character-Display.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 39.23 Beeping

This section describes how to make Emacs ring the bell (or blink the screen) to attract the user’s attention. Be conservative about how often you do this; frequent bells can become irritating. Also be careful not to use just beeping when signaling an error is more appropriate (see [Errors](Errors.html)).

*   Function: **ding** *\&optional do-not-terminate*

    This function beeps, or flashes the screen (see `visible-bell` below). It also terminates any keyboard macro currently executing unless `do-not-terminate` is non-`nil`.

<!---->

*   Function: **beep** *\&optional do-not-terminate*

    This is a synonym for `ding`.

<!---->

*   User Option: **visible-bell**

    This variable determines whether Emacs should flash the screen to represent a bell. Non-`nil` means yes, `nil` means no. This is effective on graphical displays, and on text terminals provided the terminal’s Termcap entry defines the visible bell capability (‘`vb`’).

<!---->

*   User Option: **ring-bell-function**

    If this is non-`nil`, it specifies how Emacs should ring the bell. Its value should be a function of no arguments. If this is non-`nil`, it takes precedence over the `visible-bell` variable.
