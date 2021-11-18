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

Previous: [Logging Messages](Logging-Messages.html), Up: [The Echo Area](The-Echo-Area.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.4.4 Echo Area Customization

These variables control details of how the echo area works.

*   Variable: **cursor-in-echo-area**

    This variable controls where the cursor appears when a message is displayed in the echo area. If it is non-`nil`, then the cursor appears at the end of the message. Otherwise, the cursor appears at point—not in the echo area at all.

    The value is normally `nil`; Lisp programs bind it to `t` for brief periods of time.

<!---->

*   Variable: **echo-area-clear-hook**

    This normal hook is run whenever the echo area is cleared—either by `(message nil)` or for any other reason.

<!---->

*   User Option: **echo-keystrokes**

    This variable determines how much time should elapse before command characters echo. Its value must be a number, and specifies the number of seconds to wait before echoing. If the user types a prefix key (such as `C-x`) and then delays this many seconds before continuing, the prefix key is echoed in the echo area. (Once echoing begins in a key sequence, all subsequent characters in the same key sequence are echoed immediately.)

    If the value is zero, then command input is not echoed.

<!---->

*   Variable: **message-truncate-lines**

    Normally, displaying a long message resizes the echo area to display the entire message. But if the variable `message-truncate-lines` is non-`nil`, the echo area does not resize, and the message is truncated to fit it.

The variable `max-mini-window-height`, which specifies the maximum height for resizing minibuffer windows, also applies to the echo area (which is really a special use of the minibuffer window; see [Minibuffer Windows](Minibuffer-Windows.html)).

Previous: [Logging Messages](Logging-Messages.html), Up: [The Echo Area](The-Echo-Area.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
