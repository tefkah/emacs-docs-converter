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

Next: [Replacing](Replacing.html), Previous: [Registers](Registers.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.22 Transposition of Text

This function can be used to transpose stretches of text:

*   Function: **transpose-regions** *start1 end1 start2 end2 \&optional leave-markers*

    This function exchanges two nonoverlapping portions of the buffer (if they overlap, the function signals an error). Arguments `start1` and `end1` specify the bounds of one portion and arguments `start2` and `end2` specify the bounds of the other portion.

    Normally, `transpose-regions` relocates markers with the transposed text; a marker previously positioned within one of the two transposed portions moves along with that portion, thus remaining between the same two characters in their new position. However, if `leave-markers` is non-`nil`, `transpose-regions` does not do this—it leaves all markers unrelocated.
