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

Next: [Recursive Mini](Recursive-Mini.html), Previous: [Minibuffer Windows](Minibuffer-Windows.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.12 Minibuffer Contents

These functions access the minibuffer prompt and contents.

*   Function: **minibuffer-prompt**

    This function returns the prompt string of the currently active minibuffer. If no minibuffer is active, it returns `nil`.

<!---->

*   Function: **minibuffer-prompt-end**

    This function returns the current position of the end of the minibuffer prompt, if a minibuffer is current. Otherwise, it returns the minimum valid buffer position.

<!---->

*   Function: **minibuffer-prompt-width**

    This function returns the current display-width of the minibuffer prompt, if a minibuffer is current. Otherwise, it returns zero.

<!---->

*   Function: **minibuffer-contents**

    This function returns the editable contents of the minibuffer (that is, everything except the prompt) as a string, if a minibuffer is current. Otherwise, it returns the entire contents of the current buffer.

<!---->

*   Function: **minibuffer-contents-no-properties**

    This is like `minibuffer-contents`, except that it does not copy text properties, just the characters themselves. See [Text Properties](Text-Properties.html).

<!---->

*   Command: **delete-minibuffer-contents**

    This command erases the editable contents of the minibuffer (that is, everything except the prompt), if a minibuffer is current. Otherwise, it erases the entire current buffer.
