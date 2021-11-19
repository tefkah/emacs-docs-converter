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

Next: [Using Debugger](Using-Debugger.html), Previous: [Variable Debugging](Variable-Debugging.html), Up: [Debugger](Debugger.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.1.5 Explicit Entry to the Debugger

You can cause the debugger to be called at a certain point in your program by writing the expression `(debug)` at that point. To do this, visit the source file, insert the text ‘`(debug)`’ at the proper place, and type `C-M-x` (`eval-defun`, a Lisp mode key binding). **Warning:** if you do this for temporary debugging purposes, be sure to undo this insertion before you save the file!

The place where you insert ‘`(debug)`’ must be a place where an additional form can be evaluated and its value ignored. (If the value of `(debug)` isn’t ignored, it will alter the execution of the program!) The most common suitable places are inside a `progn` or an implicit `progn` (see [Sequencing](Sequencing.html)).

If you don’t know exactly where in the source code you want to put the debug statement, but you want to display a backtrace when a certain message is displayed, you can set `debug-on-message` to a regular expression matching the desired message.
