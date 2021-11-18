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

Next: [Network](Network.html), Previous: [System Processes](System-Processes.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 38.13 Transaction Queues

You can use a *transaction queue* to communicate with a subprocess using transactions. First use `tq-create` to create a transaction queue communicating with a specified process. Then you can call `tq-enqueue` to send a transaction.

*   Function: **tq-create** *process*

    This function creates and returns a transaction queue communicating with `process`. The argument `process` should be a subprocess capable of sending and receiving streams of bytes. It may be a child process, or it may be a TCP connection to a server, possibly on another machine.

<!---->

*   Function: **tq-enqueue** *queue question regexp closure fn \&optional delay-question*

    This function sends a transaction to queue `queue`. Specifying the queue has the effect of specifying the subprocess to talk to.

    The argument `question` is the outgoing message that starts the transaction. The argument `fn` is the function to call when the corresponding answer comes back; it is called with two arguments: `closure`, and the answer received.

    The argument `regexp` is a regular expression that should match text at the end of the entire answer, but nothing before; that’s how `tq-enqueue` determines where the answer ends.

    If the argument `delay-question` is non-`nil`, delay sending this question until the process has finished replying to any previous questions. This produces more reliable results with some processes.

<!---->

*   Function: **tq-close** *queue*

    Shut down transaction queue `queue`, waiting for all pending transactions to complete, and then terminate the connection or child process.

Transaction queues are implemented by means of a filter function. See [Filter Functions](Filter-Functions.html).
