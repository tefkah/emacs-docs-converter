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

Next: [C Dialect](C-Dialect.html), Previous: [Stack-allocated Objects](Stack_002dallocated-Objects.html), Up: [GNU Emacs Internals](GNU-Emacs-Internals.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### E.5 Memory Usage

These functions and variables give information about the total amount of memory allocation that Emacs has done, broken down by data type. Note the difference between these and the values returned by `garbage-collect`; those count objects that currently exist, but these count the number or size of all allocations, including those for objects that have since been freed.

*   Variable: **cons-cells-consed**

    The total number of cons cells that have been allocated so far in this Emacs session.

<!---->

*   Variable: **floats-consed**

    The total number of floats that have been allocated so far in this Emacs session.

<!---->

*   Variable: **vector-cells-consed**

    The total number of vector cells that have been allocated so far in this Emacs session. This includes vector-like objects such as markers and overlays, plus certain objects not visible to users.

<!---->

*   Variable: **symbols-consed**

    The total number of symbols that have been allocated so far in this Emacs session.

<!---->

*   Variable: **string-chars-consed**

    The total number of string characters that have been allocated so far in this session.

<!---->

*   Variable: **intervals-consed**

    The total number of intervals that have been allocated so far in this Emacs session.

<!---->

*   Variable: **strings-consed**

    The total number of strings that have been allocated so far in this Emacs session.
