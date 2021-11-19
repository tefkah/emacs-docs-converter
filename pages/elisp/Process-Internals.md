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

Previous: [Window Internals](Window-Internals.html), Up: [Object Internals](Object-Internals.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### E.9.3 Process Internals

The fields of a process (for a complete list, see the definition of `struct Lisp_Process` in `process.h`) include:

*   `name`

    A Lisp string, the name of the process.

*   `command`

    A list containing the command arguments that were used to start this process. For a network or serial process, it is `nil` if the process is running or `t` if the process is stopped.

*   `filter`

    A Lisp function used to accept output from the process.

*   `sentinel`

    A Lisp function called whenever the state of the process changes.

*   `buffer`

    The associated buffer of the process.

*   `pid`

    An integer, the operating system’s process ID. Pseudo-processes such as network or serial connections use a value of 0.

*   `childp`

    A flag, `t` if this is really a child process. For a network or serial connection, it is a plist based on the arguments to `make-network-process` or `make-serial-process`.

*   `mark`

    A marker indicating the position of the end of the last output from this process inserted into the buffer. This is often but not always the end of the buffer.

*   `kill_without_query`

    If this is non-zero, killing Emacs while this process is still running does not ask for confirmation about killing the process.

*   `raw_status`

    The raw process status, as returned by the `wait` system call.

*   `status`

    The process status, as `process-status` should return it. This is a Lisp symbol, a cons cell, or a list.

*   *   `tick`
    *   `update_tick`

    If these two fields are not equal, a change in the status of the process needs to be reported, either by running the sentinel or by inserting a message in the process buffer.

*   `pty_flag`

    Non-zero if communication with the subprocess uses a pty; zero if it uses a pipe.

*   `infd`

    The file descriptor for input from the process.

*   `outfd`

    The file descriptor for output to the process.

*   `tty_name`

    The name of the terminal that the subprocess is using, or `nil` if it is using pipes.

*   `decode_coding_system`

    Coding-system for decoding the input from this process.

*   `decoding_buf`

    A working buffer for decoding.

*   `decoding_carryover`

    Size of carryover in decoding.

*   `encode_coding_system`

    Coding-system for encoding the output to this process.

*   `encoding_buf`

    A working buffer for encoding.

*   `inherit_coding_system_flag`

    Flag to set `coding-system` of the process buffer from the coding system used to decode process output.

*   `type`

    Symbol indicating the type of process: `real`, `network`, `serial`.

Previous: [Window Internals](Window-Internals.html), Up: [Object Internals](Object-Internals.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
