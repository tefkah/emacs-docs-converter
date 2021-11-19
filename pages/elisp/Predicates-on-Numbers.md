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

Next: [Comparison of Numbers](Comparison-of-Numbers.html), Previous: [Float Basics](Float-Basics.html), Up: [Numbers](Numbers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 3.3 Type Predicates for Numbers

The functions in this section test for numbers, or for a specific type of number. The functions `integerp` and `floatp` can take any type of Lisp object as argument (they would not be of much use otherwise), but the `zerop` predicate requires a number as its argument. See also `integer-or-marker-p` and `number-or-marker-p`, in [Predicates on Markers](Predicates-on-Markers.html).

*   Function: **bignump** *object*

    This predicate tests whether its argument is a large integer, and returns `t` if so, `nil` otherwise. Unlike small integers, large integers can be `=` or `eql` even if they are not `eq`.

<!---->

*   Function: **fixnump** *object*

    This predicate tests whether its argument is a small integer, and returns `t` if so, `nil` otherwise. Small integers can be compared with `eq`.

<!---->

*   Function: **floatp** *object*

    This predicate tests whether its argument is floating point and returns `t` if so, `nil` otherwise.

<!---->

*   Function: **integerp** *object*

    This predicate tests whether its argument is an integer, and returns `t` if so, `nil` otherwise.

<!---->

*   Function: **numberp** *object*

    This predicate tests whether its argument is a number (either integer or floating point), and returns `t` if so, `nil` otherwise.

<!---->

*   Function: **natnump** *object*

    This predicate (whose name comes from the phrase “natural number”) tests to see whether its argument is a nonnegative integer, and returns `t` if so, `nil` otherwise. 0 is considered non-negative.

    `wholenump` is a synonym for `natnump`.

<!---->

*   Function: **zerop** *number*

    This predicate tests whether its argument is zero, and returns `t` if so, `nil` otherwise. The argument must be a number.

    `(zerop x)` is equivalent to `(= x 0)`.

Next: [Comparison of Numbers](Comparison-of-Numbers.html), Previous: [Float Basics](Float-Basics.html), Up: [Numbers](Numbers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
