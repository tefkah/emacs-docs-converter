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

Next: [Backtraces](Backtraces.html), Previous: [Explicit Debug](Explicit-Debug.html), Up: [Debugger](Debugger.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.1.6 Using the Debugger

When the debugger is entered, it displays the previously selected buffer in one window and a buffer named `*Backtrace*` in another window. The backtrace buffer contains one line for each level of Lisp function execution currently going on. At the beginning of this buffer is a message describing the reason that the debugger was invoked (such as the error message and associated data, if it was invoked due to an error).

The backtrace buffer is read-only and uses a special major mode, Debugger mode, in which letters are defined as debugger commands. The usual Emacs editing commands are available; thus, you can switch windows to examine the buffer that was being edited at the time of the error, switch buffers, visit files, or do any other sort of editing. However, the debugger is a recursive editing level (see [Recursive Editing](Recursive-Editing.html)) and it is wise to go back to the backtrace buffer and exit the debugger (with the `q` command) when you are finished with it. Exiting the debugger gets out of the recursive edit and buries the backtrace buffer. (You can customize what the `q` command does with the backtrace buffer by setting the variable `debugger-bury-or-kill`. For example, set it to `kill` if you prefer to kill the buffer rather than bury it. Consult the variable’s documentation for more possibilities.)

When the debugger has been entered, the `debug-on-error` variable is temporarily set according to `eval-expression-debug-on-error`. If the latter variable is non-`nil`, `debug-on-error` will temporarily be set to `t`. This means that any further errors that occur while doing a debugging session will (by default) trigger another backtrace. If this is not what you want, you can either set `eval-expression-debug-on-error` to `nil`, or set `debug-on-error` to `nil` in `debugger-mode-hook`.

The debugger itself must be run byte-compiled, since it makes assumptions about the state of the Lisp interpreter. These assumptions are false if the debugger is running interpreted.

Next: [Backtraces](Backtraces.html), Previous: [Explicit Debug](Explicit-Debug.html), Up: [Debugger](Debugger.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
