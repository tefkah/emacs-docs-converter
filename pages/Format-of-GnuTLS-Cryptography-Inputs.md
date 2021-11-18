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

Next: [GnuTLS Cryptographic Functions](GnuTLS-Cryptographic-Functions.html), Up: [GnuTLS Cryptography](GnuTLS-Cryptography.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.27.1 Format of GnuTLS Cryptography Inputs

The inputs to GnuTLS cryptographic functions can be specified in several ways, both as primitive Emacs Lisp types or as lists.

The list form is currently similar to how `md5` and `secure-hash` operate.

*   `buffer`

    Simply passing a buffer as input means the whole buffer should be used.

*   `string`

    A string as input will be used directly. It may be modified by the function (unlike most other Emacs Lisp functions) to reduce the chance of exposing sensitive data after the function does its work.

*   `(buffer-or-string start end coding-system noerror)`

    This specifies a buffer or a string as described above, but an optional range can be specified with `start` and `end`.

    In addition an optional `coding-system` can be specified if needed.

    The last optional item, `noerror`, overrides the normal error when the text can’t be encoded using the specified or chosen coding system. When `noerror` is non-`nil`, this function silently uses `raw-text` coding instead.

*   `(iv-auto length)`

    This will generate an IV (Initialization Vector) of the specified length using the GnuTLS `GNUTLS_RND_NONCE` generator and pass it to the function. This ensures that the IV is unpredictable and unlikely to be reused in the same session. The actual value of the IV is returned by the function as described below.
