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

Next: [Defining Hash](Defining-Hash.html), Previous: [Creating Hash](Creating-Hash.html), Up: [Hash Tables](Hash-Tables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 8.2 Hash Table Access

This section describes the functions for accessing and storing associations in a hash table. In general, any Lisp object can be used as a hash key, unless the comparison method imposes limits. Any Lisp object can also be used as the value.

*   Function: **gethash** *key table \&optional default*

    This function looks up `key` in `table`, and returns its associated `value`—or `default`, if `key` has no association in `table`.

<!---->

*   Function: **puthash** *key value table*

    This function enters an association for `key` in `table`, with value `value`. If `key` already has an association in `table`, `value` replaces the old associated value.

<!---->

*   Function: **remhash** *key table*

    This function removes the association for `key` from `table`, if there is one. If `key` has no association, `remhash` does nothing.

    **Common Lisp note:** In Common Lisp, `remhash` returns non-`nil` if it actually removed an association and `nil` otherwise. In Emacs Lisp, `remhash` always returns `nil`.

<!---->

*   Function: **clrhash** *table*

    This function removes all the associations from hash table `table`, so that it becomes empty. This is also called *clearing* the hash table.

    **Common Lisp note:** In Common Lisp, `clrhash` returns the empty `table`. In Emacs Lisp, it returns `nil`.

<!---->

*   Function: **maphash** *function table*

    This function calls `function` once for each of the associations in `table`. The function `function` should accept two arguments—a `key` listed in `table`, and its associated `value`. `maphash` returns `nil`.
