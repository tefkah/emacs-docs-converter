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

Previous: [Condition Variables](Condition-Variables.html), Up: [Threads](Threads.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 37.4 The Thread List

The `list-threads` command lists all the currently alive threads. In the resulting buffer, each thread is identified either by the name passed to `make-thread` (see [Basic Thread Functions](Basic-Thread-Functions.html)), or by its unique internal identifier if it was not created with a name. The status of each thread at the time of the creation or last update of the buffer is shown, in addition to the object the thread was blocked on at the time, if it was blocked.

*   Variable: **thread-list-refresh-seconds**

    The `*Threads*` buffer will automatically update twice per second. You can make the refresh rate faster or slower by customizing this variable.

Here are the commands available in the thread list buffer:

*   `b`

    Show a backtrace of the thread at point. This will show where in its code the thread had yielded or was blocked at the moment you pressed `b`. Be aware that the backtrace is a snapshot; the thread could have meanwhile resumed execution, and be in a different state, or could have exited.

    You may use `g` in the thread’s backtrace buffer to get an updated backtrace, as backtrace buffers do not automatically update. See [Backtraces](Backtraces.html), for a description of backtraces and the other commands which work on them.

*   `s`

    Signal the thread at point. After `s`, type `q` to send a quit signal or `e` to send an error signal. Threads may implement handling of signals, but the default behavior is to exit on any signal. Therefore you should only use this command if you understand how to restart the target thread, because your Emacs session may behave incorrectly if necessary threads are killed.

*   `g`

    Update the list of threads and their statuses.

Previous: [Condition Variables](Condition-Variables.html), Up: [Threads](Threads.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]