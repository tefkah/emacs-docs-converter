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

Next: [Base 64](Base-64.html), Previous: [Replacing](Replacing.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.24 Dealing With Compressed Data

When `auto-compression-mode` is enabled, Emacs automatically uncompresses compressed files when you visit them, and automatically recompresses them if you alter and save them. See [Compressed Files](https://www.gnu.org/software/emacs/manual/html_node/emacs/Compressed-Files.html#Compressed-Files) in The GNU Emacs Manual.

The above feature works by calling an external executable (e.g., `gzip`). Emacs can also be compiled with support for built-in decompression using the zlib library, which is faster than calling an external program.

*   Function: **zlib-available-p**

    This function returns non-`nil` if built-in zlib decompression is available.

<!---->

*   Function: **zlib-decompress-region** *start end \&optional allow-partial*

    This function decompresses the region between `start` and `end`, using built-in zlib decompression. The region should contain data that were compressed with gzip or zlib. On success, the function replaces the contents of the region with the decompressed data. If `allow-partial` is `nil` or omitted, then on failure, the function leaves the region unchanged and returns `nil`. Otherwise, it returns the number of bytes that were not decompressed and replaces the region text by whatever data was successfully decompressed. This function can be called only in unibyte buffers.
