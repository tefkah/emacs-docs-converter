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

Next: [File Names](File-Names.html), Previous: [Changing Files](Changing-Files.html), Up: [Files](Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 25.8 Files and Secondary Storage

After Emacs changes a file, there are two reasons the changes might not survive later failures of power or media, both having to do with efficiency. First, the operating system might alias written data with data already stored elsewhere on secondary storage until one file or the other is later modified; this will lose both files if the only copy on secondary storage is lost due to media failure. Second, the operating system might not write data to secondary storage immediately, which will lose the data if power is lost.

Although both sorts of failures can largely be avoided by a suitably configured file system, such systems are typically more expensive or less efficient. In more-typical systems, to survive media failure you can copy the file to a different device, and to survive a power failure you can use the `write-region` function with the `write-region-inhibit-fsync` variable set to `nil`. See [Writing to Files](Writing-to-Files.html).
