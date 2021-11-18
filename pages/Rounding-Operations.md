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

Next: [Bitwise Operations](Bitwise-Operations.html), Previous: [Arithmetic Operations](Arithmetic-Operations.html), Up: [Numbers](Numbers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 3.7 Rounding Operations

The functions `ffloor`, `fceiling`, `fround`, and `ftruncate` take a floating-point argument and return a floating-point result whose value is a nearby integer. `ffloor` returns the nearest integer below; `fceiling`, the nearest integer above; `ftruncate`, the nearest integer in the direction towards zero; `fround`, the nearest integer.

*   Function: **ffloor** *float*

    This function rounds `float` to the next lower integral value, and returns that value as a floating-point number.

<!---->

*   Function: **fceiling** *float*

    This function rounds `float` to the next higher integral value, and returns that value as a floating-point number.

<!---->

*   Function: **ftruncate** *float*

    This function rounds `float` towards zero to an integral value, and returns that value as a floating-point number.

<!---->

*   Function: **fround** *float*

    This function rounds `float` to the nearest integral value, and returns that value as a floating-point number. Rounding a value equidistant between two integers returns the even integer.
