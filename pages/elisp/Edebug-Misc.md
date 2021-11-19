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

Next: [Breaks](Breaks.html), Previous: [Jumping](Jumping.html), Up: [Edebug](Edebug.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.5 Miscellaneous Edebug Commands

Some miscellaneous Edebug commands are described here.

*   `?`

    Display the help message for Edebug (`edebug-help`).

*   *   `a`
    *   `C-]`

    Abort one level back to the previous command level (`abort-recursive-edit`).

*   `q`

    Return to the top level editor command loop (`top-level`). This exits all recursive editing levels, including all levels of Edebug activity. However, instrumented code protected with `unwind-protect` or `condition-case` forms may resume debugging.

*   `Q`

    Like `q`, but don’t stop even for protected code (`edebug-top-level-nonstop`).

*   `r`

    Redisplay the most recently known expression result in the echo area (`edebug-previous-result`).

*   `d`

    Display a backtrace, excluding Edebug’s own functions for clarity (`edebug-pop-to-backtrace`).

    See [Backtraces](Backtraces.html), for a description of backtraces and the commands which work on them.

    If you would like to see Edebug’s functions in the backtrace, use `M-x edebug-backtrace-show-instrumentation`. To hide them again use `M-x edebug-backtrace-hide-instrumentation`.

    If a backtrace frame starts with ‘`>`’ that means that Edebug knows where the source code for the frame is located. Use `s` to jump to the source code for the current frame.

    The backtrace buffer is killed automatically when you continue execution.

You can invoke commands from Edebug that activate Edebug again recursively. Whenever Edebug is active, you can quit to the top level with `q` or abort one recursive edit level with `C-]`. You can display a backtrace of all the pending evaluations with `d`.
