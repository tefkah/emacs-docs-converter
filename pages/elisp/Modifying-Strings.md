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

Next: [Text Comparison](Text-Comparison.html), Previous: [Creating Strings](Creating-Strings.html), Up: [Strings and Characters](Strings-and-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 4.4 Modifying Strings

You can alter the contents of a mutable string via operations described in this section. See [Mutability](Mutability.html).

The most basic way to alter the contents of an existing string is with `aset` (see [Array Functions](Array-Functions.html)). `(aset string idx char)` stores `char` into `string` at index `idx`. Each character occupies one or more bytes, and if `char` needs a different number of bytes from the character already present at that index, `aset` signals an error.

A more powerful function is `store-substring`:

*   Function: **store-substring** *string idx obj*

    This function alters part of the contents of the string `string`, by storing `obj` starting at index `idx`. The argument `obj` may be either a character or a (smaller) string.

    Since it is impossible to change the length of an existing string, it is an error if `obj` doesn’t fit within `string`’s actual length, or if any new character requires a different number of bytes from the character currently present at that point in `string`.

To clear out a string that contained a password, use `clear-string`:

*   Function: **clear-string** *string*

    This makes `string` a unibyte string and clears its contents to zeros. It may also change `string`’s length.
