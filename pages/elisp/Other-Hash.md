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

Previous: [Defining Hash](Defining-Hash.html), Up: [Hash Tables](Hash-Tables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 8.4 Other Hash Table Functions

Here are some other functions for working with hash tables.

*   Function: **hash-table-p** *table*

    This returns non-`nil` if `table` is a hash table object.

<!---->

*   Function: **copy-hash-table** *table*

    This function creates and returns a copy of `table`. Only the table itself is copied—the keys and values are shared.

<!---->

*   Function: **hash-table-count** *table*

    This function returns the actual number of entries in `table`.

<!---->

*   Function: **hash-table-test** *table*

    This returns the `test` value that was given when `table` was created, to specify how to hash and compare keys. See `make-hash-table` (see [Creating Hash](Creating-Hash.html)).

<!---->

*   Function: **hash-table-weakness** *table*

    This function returns the `weak` value that was specified for hash table `table`.

<!---->

*   Function: **hash-table-rehash-size** *table*

    This returns the rehash size of `table`.

<!---->

*   Function: **hash-table-rehash-threshold** *table*

    This returns the rehash threshold of `table`.

<!---->

*   Function: **hash-table-size** *table*

    This returns the current nominal size of `table`.
