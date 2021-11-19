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

Next: [Serial Ports](Serial-Ports.html), Previous: [Low-Level Network](Low_002dLevel-Network.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 38.18 Misc Network Facilities

These additional functions are useful for creating and operating on network connections. Note that they are supported only on some systems.

*   Function: **network-interface-list** *\&optional full family*

    This function returns a list describing the network interfaces of the machine you are using. The value is an alist whose elements have the form `(ifname . address)`. `ifname` is a string naming the interface, `address` has the same form as the `local-address` and `remote-address` arguments to `make-network-process`, i.e. a vector of integers. By default both IPv4 and IPv6 addresses are returned if possible.

    Optional argument `full` non-`nil` means to instead return a list of one or more elements of the form `(ifname addr bcast netmask)`<!-- /@w -->. `ifname` is a non-unique string naming the interface. `addr`, `bcast`, and `netmask` are vectors of integers detailing the IP address, broadcast address, and network mask.

    Optional argument `family` specified as symbol `ipv4` or `ipv6` restricts the returned information to IPv4 and IPv6 addresses respectively, independently of the value of `full`. Specifying `ipv6` when IPv6 support is not available will result in an error being signaled.

    Some examples:

        (network-interface-list) ⇒
        (("vmnet8" .
          [172 16 76 1 0])
         ("vmnet1" .
          [172 16 206 1 0])
         ("lo0" .
          [65152 0 0 0 0 0 0 1 0])
         ("lo0" .
          [0 0 0 0 0 0 0 1 0])
         ("lo0" .
          [127 0 0 1 0]))

    <!---->

        (network-interface-list t) ⇒
        (("vmnet8"
          [172 16 76 1 0]
          [172 16 76 255 0]
          [255 255 255 0 0])
         ("vmnet1"
          [172 16 206 1 0]
          [172 16 206 255 0]
          [255 255 255 0 0])
         ("lo0"
          [65152 0 0 0 0 0 0 1 0]
          [65152 0 0 0 65535 65535 65535 65535 0]
          [65535 65535 65535 65535 0 0 0 0 0])
         ("lo0"
          [0 0 0 0 0 0 0 1 0]
          [0 0 0 0 0 0 0 1 0]
          [65535 65535 65535 65535 65535 65535 65535 65535 0])
         ("lo0"
          [127 0 0 1 0]
          [127 255 255 255 0]
          [255 0 0 0 0]))

<!---->

*   Function: **network-interface-info** *ifname*

    This function returns information about the network interface named `ifname`. The value is a list of the form `(addr bcast netmask hwaddr flags)`.

    *   `addr`

        The Internet protocol address.

    *   `bcast`

        The broadcast address.

    *   `netmask`

        The network mask.

    *   `hwaddr`

        The layer 2 address (Ethernet MAC address, for instance).

    *   `flags`

        The current flags of the interface.

    Note that this function returns only IPv4 information.

<!---->

*   Function: **format-network-address** *address \&optional omit-port*

    This function converts the Lisp representation of a network address to a string.

    A five-element vector `[a b c d p]` represents an IPv4 address `a`.`b`.`c`.`d` and port number `p`. `format-network-address` converts that to the string `"a.b.c.d:p"`.

    A nine-element vector `[a b c d e f g h p]` represents an IPv6 address along with a port number. `format-network-address` converts that to the string `"[a:b:c:d:e:f:g:h]:p"`.

    If the vector does not include the port number, `p`, or if `omit-port` is non-`nil`, the result does not include the `:p` suffix.

<!---->

*   Function: **network-lookup-address-info** *name \&optional family*

    This function is used to perform hostname lookups on `name`, which is expected to be an ASCII-only string, otherwise an error is signaled. Call `puny-encode-domain` on `name` first if you wish to lookup internationalized hostnames.

    If successful it returns a list of Lisp representations of network addresses, otherwise it returns `nil`. In the latter case, it also displays the error message hopefully explaining what went wrong.

    By default both IPv4 and IPv6 lookups are attempted. The optional argument `family` controls this behavior, specifying the symbol `ipv4` or `ipv6` restricts lookups to IPv4 and IPv6 respectively.

Next: [Serial Ports](Serial-Ports.html), Previous: [Low-Level Network](Low_002dLevel-Network.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
