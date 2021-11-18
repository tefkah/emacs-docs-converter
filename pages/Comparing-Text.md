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

Next: [Insertion](Insertion.html), Previous: [Buffer Contents](Buffer-Contents.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.3 Comparing Text

This function lets you compare portions of the text in a buffer, without copying them into strings first.

*   Function: **compare-buffer-substrings** *buffer1 start1 end1 buffer2 start2 end2*

    This function lets you compare two substrings of the same buffer or two different buffers. The first three arguments specify one substring, giving a buffer (or a buffer name) and two positions within the buffer. The last three arguments specify the other substring in the same way. You can use `nil` for `buffer1`, `buffer2`, or both to stand for the current buffer.

    The value is negative if the first substring is less, positive if the first is greater, and zero if they are equal. The absolute value of the result is one plus the index of the first differing characters within the substrings.

    This function ignores case when comparing characters if `case-fold-search` is non-`nil`. It always ignores text properties.

    Suppose you have the text ‘`foobarbar haha!rara!`’<!-- /@w --> in the current buffer; then in this example the two substrings are ‘`rbar `’ and ‘`rara!`’. The value is 2 because the first substring is greater at the second character.

        (compare-buffer-substrings nil 6 11 nil 16 21)
             ⇒ 2
