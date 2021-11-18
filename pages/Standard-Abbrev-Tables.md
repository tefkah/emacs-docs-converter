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

Next: [Abbrev Properties](Abbrev-Properties.html), Previous: [Abbrev Expansion](Abbrev-Expansion.html), Up: [Abbrevs](Abbrevs.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 36.5 Standard Abbrev Tables

Here we list the variables that hold the abbrev tables for the preloaded major modes of Emacs.

*   Variable: **global-abbrev-table**

    This is the abbrev table for mode-independent abbrevs. The abbrevs defined in it apply to all buffers. Each buffer may also have a local abbrev table, whose abbrev definitions take precedence over those in the global table.

<!---->

*   Variable: **local-abbrev-table**

    The value of this buffer-local variable is the (mode-specific) abbreviation table of the current buffer. It can also be a list of such tables.

<!---->

*   Variable: **abbrev-minor-mode-table-alist**

    The value of this variable is a list of elements of the form `(mode . abbrev-table)` where `mode` is the name of a variable: if the variable is bound to a non-`nil` value, then the `abbrev-table` is active, otherwise it is ignored. `abbrev-table` can also be a list of abbrev tables.

<!---->

*   Variable: **fundamental-mode-abbrev-table**

    This is the local abbrev table used in Fundamental mode; in other words, it is the local abbrev table in all buffers in Fundamental mode.

<!---->

*   Variable: **text-mode-abbrev-table**

    This is the local abbrev table used in Text mode.

<!---->

*   Variable: **lisp-mode-abbrev-table**

    This is the local abbrev table used in Lisp mode. It is the parent of the local abbrev table used in Emacs Lisp mode. See [Abbrev Table Properties](Abbrev-Table-Properties.html).
