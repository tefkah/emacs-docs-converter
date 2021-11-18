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

Previous: [Package Archives](Package-Archives.html), Up: [Packaging](Packaging.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 41.5 Interfacing to an archive web server

A web server providing access to a package archive must support the following queries:

*   archive-contents

    Return a lisp form describing the archive contents. The form is a list of ’package-desc’ structures (see `package.el`), except the first element of the list is the archive version.

*   \<package name>-readme.txt

    Return the long description of the package.

*   \<file name>.sig

    Return the signature for the file.

*   \<file name>

    Return the file. This will be the tarball for a multi-file package, or the single file for a simple package.
