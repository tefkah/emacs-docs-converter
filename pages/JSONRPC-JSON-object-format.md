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

Next: [JSONRPC deferred requests](JSONRPC-deferred-requests.html), Previous: [Process-based JSONRPC connections](Process_002dbased-JSONRPC-connections.html), Up: [JSONRPC](JSONRPC.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.30.3 JSONRPC JSON object format

JSONRPC JSON objects are exchanged as Lisp plists (see [Property Lists](Property-Lists.html)): JSON-compatible plists are handed to the dispatcher functions and, likewise, JSON-compatible plists should be given to `jsonrpc-notify`, `jsonrpc-request`, and `jsonrpc-async-request`.

To facilitate handling plists, this library makes liberal use of `cl-lib` library (see [cl-lib](https://www.gnu.org/software/emacs/manual/html_node/cl/index.html#Top) in Common Lisp Extensions for GNU Emacs Lisp) and suggests (but doesn’t force) its clients to do the same. A macro `jsonrpc-lambda` can be used to create a lambda for destructuring a JSON-object like in this example:

    (jsonrpc-async-request
     myproc :frobnicate `(:foo "trix")
     :success-fn (jsonrpc-lambda (&key bar baz &allow-other-keys)
                   (message "Server replied back with %s and %s!"
                            bar baz))
     :error-fn (jsonrpc-lambda (&key code message _data)
                 (message "Sadly, server reports %s: %s"
                          code message)))
