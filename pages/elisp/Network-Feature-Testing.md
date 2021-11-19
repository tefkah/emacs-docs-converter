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

Previous: [Network Options](Network-Options.html), Up: [Low-Level Network](Low_002dLevel-Network.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 38.17.3 Testing Availability of Network Features

To test for the availability of a given network feature, use `featurep` like this:

    (featurep 'make-network-process '(keyword value))

The result of this form is `t` if it works to specify `keyword` with value `value` in `make-network-process`. Here are some of the `keyword`—`value` pairs you can test in this way.

*   `(:nowait t)`

    Non-`nil` if non-blocking connect is supported.

*   `(:type datagram)`

    Non-`nil` if datagrams are supported.

*   `(:family local)`

    Non-`nil` if local (a.k.a. “UNIX domain”) sockets are supported.

*   `(:family ipv6)`

    Non-`nil` if IPv6 is supported.

*   `(:service t)`

    Non-`nil` if the system can select the port for a server.

To test for the availability of a given network option, use `featurep` like this:

    (featurep 'make-network-process 'keyword)

The accepted `keyword` values are `:bindtodevice`, etc. For the complete list, see [Network Options](Network-Options.html). This form returns non-`nil` if that particular network option is supported by `make-network-process` (or `set-network-process-option`).
