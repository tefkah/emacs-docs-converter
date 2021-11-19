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

Next: [Minibuffer Misc](Minibuffer-Misc.html), Previous: [Minibuffer Contents](Minibuffer-Contents.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.13 Recursive Minibuffers

These functions and variables deal with recursive minibuffers (see [Recursive Editing](Recursive-Editing.html)):

*   Function: **minibuffer-depth**

    This function returns the current depth of activations of the minibuffer, a nonnegative integer. If no minibuffers are active, it returns zero.

<!---->

*   User Option: **enable-recursive-minibuffers**

    If this variable is non-`nil`, you can invoke commands (such as `find-file`) that use minibuffers even while the minibuffer is active. Such invocation produces a recursive editing level for a new minibuffer. The outer-level minibuffer is invisible while you are editing the inner one.

    If this variable is `nil`, you cannot invoke minibuffer commands when the minibuffer is active, not even if you switch to another window to do it.

If a command name has a property `enable-recursive-minibuffers` that is non-`nil`, then the command can use the minibuffer to read arguments even if it is invoked from the minibuffer. A command can also achieve this by binding `enable-recursive-minibuffers` to `t` in the interactive declaration (see [Using Interactive](Using-Interactive.html)). The minibuffer command `next-matching-history-element` (normally `M-s` in the minibuffer) does the latter.
