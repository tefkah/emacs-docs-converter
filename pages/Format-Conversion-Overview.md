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

Next: [Format Conversion Round-Trip](Format-Conversion-Round_002dTrip.html), Up: [Format Conversion](Format-Conversion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 25.13.1 Overview

The function `insert-file-contents`:

*   initially, inserts bytes from the file into the buffer;
*   decodes bytes to characters as appropriate;
*   processes formats as defined by entries in `format-alist`; and
*   calls functions in `after-insert-file-functions`.

The function `write-region`:

*   initially, calls functions in `write-region-annotate-functions`;
*   processes formats as defined by entries in `format-alist`;
*   encodes characters to bytes as appropriate; and
*   modifies the file with the bytes.

This shows the symmetry of the lowest-level operations; reading and writing handle things in opposite order. The rest of this section describes the two facilities surrounding the three variables named above, as well as some related functions. [Coding Systems](Coding-Systems.html), for details on character encoding and decoding.
