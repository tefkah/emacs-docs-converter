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

Previous: [Accepting Output](Accepting-Output.html), Up: [Output from Processes](Output-from-Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 38.9.5 Processes and Threads

Because threads were a relatively late addition to Emacs Lisp, and due to the way dynamic binding was sometimes used in conjunction with `accept-process-output`, by default a process is locked to the thread that created it. When a process is locked to a thread, output from the process can only be accepted by that thread.

A Lisp program can specify to which thread a process is to be locked, or instruct Emacs to unlock a process, in which case its output can be processed by any thread. Only a single thread will wait for output from a given process at one time—once one thread begins waiting for output, the process is temporarily locked until `accept-process-output` or `sit-for` returns.

If the thread exits, all the processes locked to it are unlocked.

*   Function: **process-thread** *process*

    Return the thread to which `process` is locked. If `process` is unlocked, return `nil`.

<!---->

*   Function: **set-process-thread** *process thread*

    Set the locking thread of `process` to `thread`. `thread` may be `nil`, in which case the process is unlocked.
