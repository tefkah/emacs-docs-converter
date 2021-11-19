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

Previous: [Recursive Mini](Recursive-Mini.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.14 Minibuffer Miscellany

*   Function: **minibufferp** *\&optional buffer-or-name*

    This function returns non-`nil` if `buffer-or-name` is a minibuffer. If `buffer-or-name` is omitted, it tests the current buffer.

<!---->

*   Variable: **minibuffer-setup-hook**

    This is a normal hook that is run whenever the minibuffer is entered. See [Hooks](Hooks.html).

<!---->

*   Macro: **minibuffer-with-setup-hook** *function \&rest body*

    This macro executes `body` after arranging for the specified `function` to be called via `minibuffer-setup-hook`. By default, `function` is called before the other functions in the `minibuffer-setup-hook` list, but if `function` is of the form `(:append func)`<!-- /@w -->, `func` will be called *after* the other hook functions.

    The `body` forms should not use the minibuffer more than once. If the minibuffer is re-entered recursively, `function` will only be called once, for the outermost use of the minibuffer.

<!---->

*   Variable: **minibuffer-exit-hook**

    This is a normal hook that is run whenever the minibuffer is exited. See [Hooks](Hooks.html).

<!---->

*   Variable: **minibuffer-help-form**

    The current value of this variable is used to rebind `help-form` locally inside the minibuffer (see [Help Functions](Help-Functions.html)).

<!---->

*   Variable: **minibuffer-scroll-window**

    If the value of this variable is non-`nil`, it should be a window object. When the function `scroll-other-window` is called in the minibuffer, it scrolls this window (see [Textual Scrolling](Textual-Scrolling.html)).

<!---->

*   Function: **minibuffer-selected-window**

    This function returns the window that was selected just before the minibuffer window was selected. If the selected window is not a minibuffer window, it returns `nil`.

<!---->

*   Function: **minibuffer-message** *string \&rest args*

    This function displays `string` temporarily at the end of the minibuffer text, for a few seconds, or until the next input event arrives, whichever comes first. The variable `minibuffer-message-timeout` specifies the number of seconds to wait in the absence of input. It defaults to 2. If `args` is non-`nil`, the actual message is obtained by passing `string` and `args` through `format-message`. See [Formatting Strings](Formatting-Strings.html).

<!---->

*   Command: **minibuffer-inactive-mode**

    This is the major mode used in inactive minibuffers. It uses keymap `minibuffer-inactive-mode-map`. This can be useful if the minibuffer is in a separate frame. See [Minibuffers and Frames](Minibuffers-and-Frames.html).

Previous: [Recursive Mini](Recursive-Mini.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
