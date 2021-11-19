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

Next: [Frame Titles](Frame-Titles.html), Previous: [Frame Parameters](Frame-Parameters.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 29.5 Terminal Parameters

Each terminal has a list of associated parameters. These *terminal parameters* are mostly a convenient way of storage for terminal-local variables, but some terminal parameters have a special meaning.

This section describes functions to read and change the parameter values of a terminal. They all accept as their argument either a terminal or a frame; the latter means use that frame’s terminal. An argument of `nil` means the selected frame’s terminal.

*   Function: **terminal-parameters** *\&optional terminal*

    This function returns an alist listing all the parameters of `terminal` and their values.

<!---->

*   Function: **terminal-parameter** *terminal parameter*

    This function returns the value of the parameter `parameter` (a symbol) of `terminal`. If `terminal` has no setting for `parameter`, this function returns `nil`.

<!---->

*   Function: **set-terminal-parameter** *terminal parameter value*

    This function sets the parameter `parameter` of `terminal` to the specified `value`, and returns the previous value of that parameter.

Here’s a list of a few terminal parameters that have a special meaning:

*   `background-mode`

    The classification of the terminal’s background color, either `light` or `dark`.

*   `normal-erase-is-backspace`

    Value is either 1 or 0, depending on whether `normal-erase-is-backspace-mode` is turned on or off on this terminal. See [DEL Does Not Delete](https://www.gnu.org/software/emacs/manual/html_node/emacs/DEL-Does-Not-Delete.html#DEL-Does-Not-Delete) in The Emacs Manual.

*   `terminal-initted`

    After the terminal is initialized, this is set to the terminal-specific initialization function.

*   `tty-mode-set-strings`

    When present, a list of strings containing escape sequences that Emacs will output while configuring a tty for rendering. Emacs emits these strings only when configuring a terminal: if you want to enable a mode on a terminal that is already active (for example, while in `tty-setup-hook`), explicitly output the necessary escape sequence using `send-string-to-terminal` in addition to adding the sequence to `tty-mode-set-strings`.

*   `tty-mode-reset-strings`

    When present, a list of strings that undo the effects of the strings in `tty-mode-set-strings`. Emacs emits these strings when exiting, deleting a terminal, or suspending itself.

Next: [Frame Titles](Frame-Titles.html), Previous: [Frame Parameters](Frame-Parameters.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
