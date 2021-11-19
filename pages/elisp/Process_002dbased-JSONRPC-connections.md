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

Next: [JSONRPC JSON object format](JSONRPC-JSON-object-format.html), Previous: [JSONRPC Overview](JSONRPC-Overview.html), Up: [JSONRPC](JSONRPC.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.30.2 Process-based JSONRPC connections

For convenience, the `jsonrpc` library comes with a built-in `jsonrpc-process-connection` transport implementation that can talk to local subprocesses (using the standard input and standard output); or TCP hosts (using sockets); or any other remote endpoint that Emacs’s process object can represent (see [Processes](Processes.html)).

Using this transport, the JSONRPC messages are encoded on the wire as plain text and prefaced by some basic HTTP-style enveloping headers, such as “Content-Length”.

For an example of an application using this transport scheme on top of JSONRPC, see the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/specification).

Along with the mandatory `:request-dispatcher` and `:notification-dispatcher` initargs, users of the `jsonrpc-process-connection` class should pass the following initargs as keyword-value pairs to `make-instance`:

*   `:process`

    Value must be a live process object or a function of no arguments producing one such object. If passed a process object, the object is expected to contain a pre-established connection; otherwise, the function is called immediately after the object is made.

*   `:on-shutdown`

    Value must be a function of a single argument, the `jsonrpc-process-connection` object. The function is called after the underlying process object has been deleted (either deliberately by `jsonrpc-shutdown`, or unexpectedly, because of some external cause).
