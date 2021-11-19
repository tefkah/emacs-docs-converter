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

Next: [Random Numbers](Random-Numbers.html), Previous: [Bitwise Operations](Bitwise-Operations.html), Up: [Numbers](Numbers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 3.9 Standard Mathematical Functions

These mathematical functions allow integers as well as floating-point numbers as arguments.

*   *   Function: **sin** *arg*
    *   Function: **cos** *arg*
    *   Function: **tan** *arg*

    These are the basic trigonometric functions, with argument `arg` measured in radians.

<!---->

*   Function: **asin** *arg*

    The value of `(asin arg)` is a number between -pi/2 and pi/2 (inclusive) whose sine is `arg`. If `arg` is out of range (outside \[-1, 1]), `asin` returns a NaN.

<!---->

*   Function: **acos** *arg*

    The value of `(acos arg)` is a number between 0 and pi (inclusive) whose cosine is `arg`. If `arg` is out of range (outside \[-1, 1]), `acos` returns a NaN.

<!---->

*   Function: **atan** *y \&optional x*

    The value of `(atan y)` is a number between -pi/2 and pi/2 (exclusive) whose tangent is `y`. If the optional second argument `x` is given, the value of `(atan y x)` is the angle in radians between the vector `[x, y]` and the `X` axis.

<!---->

*   Function: **exp** *arg*

    This is the exponential function; it returns *e* to the power `arg`.

<!---->

*   Function: **log** *arg \&optional base*

    This function returns the logarithm of `arg`, with base `base`. If you don’t specify `base`, the natural base *e* is used. If `arg` or `base` is negative, `log` returns a NaN.

<!---->

*   Function: **expt** *x y*

    This function returns `x` raised to power `y`. If both arguments are integers and `y` is nonnegative, the result is an integer; in this case, overflow signals an error, so watch out. If `x` is a finite negative number and `y` is a finite non-integer, `expt` returns a NaN.

<!---->

*   Function: **sqrt** *arg*

    This returns the square root of `arg`. If `arg` is finite and less than zero, `sqrt` returns a NaN.

In addition, Emacs defines the following common mathematical constants:

*   Variable: **float-e**

    The mathematical constant *e* (2.71828…).

<!---->

*   Variable: **float-pi**

    The mathematical constant *pi* (3.14159…).

Next: [Random Numbers](Random-Numbers.html), Previous: [Bitwise Operations](Bitwise-Operations.html), Up: [Numbers](Numbers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
