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

Next: [Creating Markers](Creating-Markers.html), Previous: [Overview of Markers](Overview-of-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 31.2 Predicates on Markers

You can test an object to see whether it is a marker, or whether it is either an integer or a marker. The latter test is useful in connection with the arithmetic functions that work with both markers and integers.

*   Function: **markerp** *object*

    This function returns `t` if `object` is a marker, `nil` otherwise. Note that integers are not markers, even though many functions will accept either a marker or an integer.

<!---->

*   Function: **integer-or-marker-p** *object*

    This function returns `t` if `object` is an integer or a marker, `nil` otherwise.

<!---->

*   Function: **number-or-marker-p** *object*

    This function returns `t` if `object` is a number (either integer or floating point) or a marker, `nil` otherwise.
