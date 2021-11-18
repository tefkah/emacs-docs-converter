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

Previous: [Record Functions](Record-Functions.html), Up: [Records](Records.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 7.2 Backward Compatibility

Code compiled with older versions of `cl-defstruct` that doesn’t use records may run into problems when used in a new Emacs. To alleviate this, Emacs detects when an old `cl-defstruct` is used, and enables a mode in which `type-of` handles old struct objects as if they were records.

*   Function: **cl-old-struct-compat-mode** *arg*

    If `arg` is positive, enable backward compatibility with old-style structs.
