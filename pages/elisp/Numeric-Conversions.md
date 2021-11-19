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

Next: [Arithmetic Operations](Arithmetic-Operations.html), Previous: [Comparison of Numbers](Comparison-of-Numbers.html), Up: [Numbers](Numbers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 3.5 Numeric Conversions

To convert an integer to floating point, use the function `float`.

*   Function: **float** *number*

    This returns `number` converted to floating point. If `number` is already floating point, `float` returns it unchanged.

There are four functions to convert floating-point numbers to integers; they differ in how they round. All accept an argument `number` and an optional argument `divisor`. Both arguments may be integers or floating-point numbers. `divisor` may also be `nil`. If `divisor` is `nil` or omitted, these functions convert `number` to an integer, or return it unchanged if it already is an integer. If `divisor` is non-`nil`, they divide `number` by `divisor` and convert the result to an integer. If `divisor` is zero (whether integer or floating point), Emacs signals an `arith-error` error.

*   Function: **truncate** *number \&optional divisor*

    This returns `number`, converted to an integer by rounding towards zero.

        (truncate 1.2)
             ⇒ 1
        (truncate 1.7)
             ⇒ 1
        (truncate -1.2)
             ⇒ -1
        (truncate -1.7)
             ⇒ -1

<!---->

*   Function: **floor** *number \&optional divisor*

    This returns `number`, converted to an integer by rounding downward (towards negative infinity).

    If `divisor` is specified, this uses the kind of division operation that corresponds to `mod`, rounding downward.

        (floor 1.2)
             ⇒ 1
        (floor 1.7)
             ⇒ 1
        (floor -1.2)
             ⇒ -2
        (floor -1.7)
             ⇒ -2
        (floor 5.99 3)
             ⇒ 1

<!---->

*   Function: **ceiling** *number \&optional divisor*

    This returns `number`, converted to an integer by rounding upward (towards positive infinity).

        (ceiling 1.2)
             ⇒ 2
        (ceiling 1.7)
             ⇒ 2
        (ceiling -1.2)
             ⇒ -1
        (ceiling -1.7)
             ⇒ -1

<!---->

*   Function: **round** *number \&optional divisor*

    This returns `number`, converted to an integer by rounding towards the nearest integer. Rounding a value equidistant between two integers returns the even integer.

        (round 1.2)
             ⇒ 1
        (round 1.7)
             ⇒ 2
        (round -1.2)
             ⇒ -1
        (round -1.7)
             ⇒ -2

Next: [Arithmetic Operations](Arithmetic-Operations.html), Previous: [Comparison of Numbers](Comparison-of-Numbers.html), Up: [Numbers](Numbers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
