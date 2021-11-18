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

Next: [Debugger Commands](Debugger-Commands.html), Previous: [Using Debugger](Using-Debugger.html), Up: [Debugger](Debugger.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.1.7 Backtraces

Debugger mode is derived from Backtrace mode, which is also used to show backtraces by Edebug and ERT. (see [Edebug](Edebug.html), and [the ERT manual](../ert/index.html#Top) in ERT: Emacs Lisp Regression Testing.)

The backtrace buffer shows you the functions that are executing and their argument values. When a backtrace buffer is created, it shows each stack frame on one, possibly very long, line. (A stack frame is the place where the Lisp interpreter records information about a particular invocation of a function.) The most recently called function will be at the top.

In a backtrace you can specify a stack frame by moving point to a line describing that frame. The frame whose line point is on is considered the *current frame*.

If a function name is underlined, that means Emacs knows where its source code is located. You can click with the mouse on that name, or move to it and type `RET`, to visit the source code. You can also type `RET` while point is on any name of a function or variable which is not underlined, to see help information for that symbol in a help buffer, if any exists. The `xref-find-definitions` command, bound to `M-.`, can also be used on any identifier in a backtrace (see [Looking Up Identifiers](https://www.gnu.org/software/emacs/manual/html_node/emacs/Looking-Up-Identifiers.html#Looking-Up-Identifiers) in The GNU Emacs Manual).

In backtraces, the tails of long lists and the ends of long strings, vectors or structures, as well as objects which are deeply nested, will be printed as underlined “...”. You can click with the mouse on a “...”, or type `RET` while point is on it, to show the part of the object that was hidden. To control how much abbreviation is done, customize `backtrace-line-length`.

Here is a list of commands for navigating and viewing backtraces:

*   `v`

    Toggle the display of local variables of the current stack frame.

*   `p`

    Move to the beginning of the frame, or to the beginning of the previous frame.

*   `n`

    Move to the beginning of the next frame.

*   `+`

    Add line breaks and indentation to the top-level Lisp form at point to make it more readable.

*   `-`

    Collapse the top-level Lisp form at point back to a single line.

*   `#`

    Toggle `print-circle` for the frame at point.

*   `:`

    Toggle `print-gensym` for the frame at point.

*   `.`

    Expand all the forms abbreviated with “...” in the frame at point.

Next: [Debugger Commands](Debugger-Commands.html), Previous: [Using Debugger](Using-Debugger.html), Up: [Debugger](Debugger.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
