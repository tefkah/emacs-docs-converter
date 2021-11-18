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

Next: [Translation of Characters](Translation-of-Characters.html), Previous: [Character Sets](Character-Sets.html), Up: [Non-ASCII Characters](Non_002dASCII-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 33.8 Scanning for Character Sets

Sometimes it is useful to find out which character set a particular character belongs to. One use for this is in determining which coding systems (see [Coding Systems](Coding-Systems.html)) are capable of representing all of the text in question; another is to determine the font(s) for displaying that text.

*   Function: **charset-after** *\&optional pos*

    This function returns the charset of highest priority containing the character at position `pos` in the current buffer. If `pos` is omitted or `nil`, it defaults to the current value of point. If `pos` is out of range, the value is `nil`.

<!---->

*   Function: **find-charset-region** *beg end \&optional translation*

    This function returns a list of the character sets of highest priority that contain characters in the current buffer between positions `beg` and `end`.

    The optional argument `translation` specifies a translation table to use for scanning the text (see [Translation of Characters](Translation-of-Characters.html)). If it is non-`nil`, then each character in the region is translated through this table, and the value returned describes the translated characters instead of the characters actually in the buffer.

<!---->

*   Function: **find-charset-string** *string \&optional translation*

    This function returns a list of character sets of highest priority that contain characters in `string`. It is just like `find-charset-region`, except that it applies to the contents of `string` instead of part of the current buffer.
