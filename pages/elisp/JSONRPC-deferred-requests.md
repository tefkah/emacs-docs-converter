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

Previous: [JSONRPC JSON object format](JSONRPC-JSON-object-format.html), Up: [JSONRPC](JSONRPC.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.30.4 Deferred JSONRPC requests

In many RPC situations, synchronization between the two communicating endpoints is a matter of correctly designing the RPC application: when synchronization is needed, requests (which are blocking) should be used; when it isn’t, notifications should suffice. However, when Emacs acts as one of these endpoints, asynchronous events (e.g. timer- or process-related) may be triggered while there is still uncertainty about the state of the remote endpoint. Furthermore, acting on these events may only sometimes demand synchronization, depending on the event’s specific nature.

The `:deferred` keyword argument to `jsonrpc-request` and `jsonrpc-async-request` is designed to let the caller indicate that the specific request needs synchronization and its actual issuance may be delayed to the future, until some condition is satisfied. Specifying `:deferred` for a request doesn’t mean it *will* be delayed, only that it *can* be. If the request isn’t sent immediately, `jsonrpc` will make renewed efforts to send it at certain key times during communication, such as when receiving or sending other messages to the endpoint.

Before any attempt to send the request, the application-specific conditions are checked. Since the `jsonrpc` library can’t know what these conditions are, the program can use the `jsonrpc-connection-ready-p` generic function (see [Generic Functions](Generic-Functions.html)) to specify them. The default method for this function returns `t`, but you can add overriding methods that return `nil` in some situations, based on the arguments passed to it, which are the `jsonrpc-connection` object (see [JSONRPC Overview](JSONRPC-Overview.html)) and whichever value you passed as the `:deferred` keyword argument.
