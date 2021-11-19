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

Next: [Explicit Debug](Explicit-Debug.html), Previous: [Function Debugging](Function-Debugging.html), Up: [Debugger](Debugger.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.1.4 Entering the debugger when a variable is modified

Sometimes a problem with a function is due to a wrong setting of a variable. Setting up the debugger to trigger whenever the variable is changed is a quick way to find the origin of the setting.

*   Command: **debug-on-variable-change** *variable*

    This function arranges for the debugger to be called whenever `variable` is modified.

    It is implemented using the watchpoint mechanism, so it inherits the same characteristics and limitations: all aliases of `variable` will be watched together, only dynamic variables can be watched, and changes to the objects referenced by variables are not detected. For details, see [Watching Variables](Watching-Variables.html).

<!---->

*   Command: **cancel-debug-on-variable-change** *\&optional variable*

    This function undoes the effect of `debug-on-variable-change` on `variable`. When called interactively, it prompts for `variable` in the minibuffer. If `variable` is omitted or `nil`, it cancels break-on-change for all variables. Calling `cancel-debug-on-variable-change` does nothing to a variable which is not currently set up to break on change.
