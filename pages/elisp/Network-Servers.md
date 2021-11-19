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

Next: [Datagrams](Datagrams.html), Previous: [Network](Network.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 38.15 Network Servers

You create a server by calling `make-network-process` (see [Network Processes](Network-Processes.html)) with `:server t`. The server will listen for connection requests from clients. When it accepts a client connection request, that creates a new network connection, itself a process object, with the following parameters:

*   The connection’s process name is constructed by concatenating the server process’s `name` with a client identification string. The client identification string for an IPv4 connection looks like ‘`<a.b.c.d:p>`’, which represents an address and port number. Otherwise, it is a unique number in brackets, as in ‘`<nnn>`’. The number is unique for each connection in the Emacs session.

*   If the server has a non-default filter, the connection process does not get a separate process buffer; otherwise, Emacs creates a new buffer for the purpose. The buffer name is the server’s buffer name or process name, concatenated with the client identification string.

    The server’s process buffer value is never used directly, but the log function can retrieve it and use it to log connections by inserting text there.

*   The communication type and the process filter and sentinel are inherited from those of the server. The server never directly uses its filter and sentinel; their sole purpose is to initialize connections made to the server.

*   The connection’s process contact information is set according to the client’s addressing information (typically an IP address and a port number). This information is associated with the `process-contact` keywords `:host`, `:service`, `:remote`.

*   The connection’s local address is set up according to the port number used for the connection.

*   The client process’s plist is initialized from the server’s plist.

Next: [Datagrams](Datagrams.html), Previous: [Network](Network.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
