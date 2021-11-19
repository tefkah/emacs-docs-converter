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

Next: [Processes and Threads](Processes-and-Threads.html), Previous: [Decoding Output](Decoding-Output.html), Up: [Output from Processes](Output-from-Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 38.9.4 Accepting Output from Processes

Output from asynchronous subprocesses normally arrives only while Emacs is waiting for some sort of external event, such as elapsed time or terminal input. Occasionally it is useful in a Lisp program to explicitly permit output to arrive at a specific point, or even to wait until output arrives from a process.

*   Function: **accept-process-output** *\&optional process seconds millisec just-this-one*

    This function allows Emacs to read pending output from processes. The output is given to their filter functions. If `process` is non-`nil` then this function does not return until some output has been received from `process` or `process` has closed the connection.

    The arguments `seconds` and `millisec` let you specify timeout periods. The former specifies a period measured in seconds and the latter specifies one measured in milliseconds. The two time periods thus specified are added together, and `accept-process-output` returns after that much time, even if there is no subprocess output.

    The argument `millisec` is obsolete (and should not be used), because `seconds` can be floating point to specify waiting a fractional number of seconds. If `seconds` is 0, the function accepts whatever output is pending but does not wait.

    If `process` is a process, and the argument `just-this-one` is non-`nil`, only output from that process is handled, suspending output from other processes until some output has been received from that process or the timeout expires. If `just-this-one` is an integer, also inhibit running timers. This feature is generally not recommended, but may be necessary for specific applications, such as speech synthesis.

    The function `accept-process-output` returns non-`nil` if it got output from `process`, or from any process if `process` is `nil`; this can occur even after a process has exited if the corresponding connection contains buffered data. The function returns `nil` if the timeout expired or the connection was closed before output arrived.

If a connection from a process contains buffered data, `accept-process-output` can return non-`nil` even after the process has exited. Therefore, although the following loop:

    ;; This loop contains a bug.
    (while (process-live-p process)
      (accept-process-output process))

will often read all output from `process`, it has a race condition and can miss some output if `process-live-p` returns `nil` while the connection still contains data. Better is to write the loop like this:

    (while (accept-process-output process))

If you have passed a non-`nil` `stderr` to `make-process`, it will have a standard error process. See [Asynchronous Processes](Asynchronous-Processes.html). In that case, waiting for process output from the main process doesn’t wait for output from the standard error process. To make sure you have received both all of standard output and all of standard error from a process, use the following code:

    (while (accept-process-output process))
    (while (accept-process-output stderr-process))

Reading pending standard error from a process running on a remote host is not possible this way.

Next: [Processes and Threads](Processes-and-Threads.html), Previous: [Decoding Output](Decoding-Output.html), Up: [Output from Processes](Output-from-Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
