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

Next: [System Processes](System-Processes.html), Previous: [Sentinels](Sentinels.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 38.11 Querying Before Exit

When Emacs exits, it terminates all its subprocesses. For subprocesses that run a program, it sends them the `SIGHUP` signal; connections are simply closed. Because subprocesses may be doing valuable work, Emacs normally asks the user to confirm that it is ok to terminate them. Each process has a query flag, which, if non-`nil`, says that Emacs should ask for confirmation before exiting and thus killing that process. The default for the query flag is `t`, meaning *do* query.

*   Function: **process-query-on-exit-flag** *process*

    This returns the query flag of `process`.

<!---->

*   Function: **set-process-query-on-exit-flag** *process flag*

    This function sets the query flag of `process` to `flag`. It returns `flag`.

    Here is an example of using `set-process-query-on-exit-flag` on a shell process to avoid querying:

        (set-process-query-on-exit-flag (get-process "shell") nil)
             ⇒ nil

<!---->

*   User Option: **confirm-kill-processes**

    If this user option is set to `t` (the default), then Emacs asks for confirmation before killing processes on exit. If it is `nil`, Emacs kills processes without confirmation, i.e., the query flag of all processes is ignored.
