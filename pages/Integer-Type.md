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

Next: [Floating-Point Type](Floating_002dPoint-Type.html), Up: [Programming Types](Programming-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.1 Integer Type

Under the hood, there are two kinds of integers—small integers, called *fixnums*, and large integers, called *bignums*.

The range of values for a fixnum depends on the machine. The minimum range is -536,870,912 to 536,870,911 (30 bits; i.e., -2\*\*29 to 2\*\*29 - 1) but many machines provide a wider range.

Bignums can have arbitrary precision. Operations that overflow a fixnum will return a bignum instead.

All numbers can be compared with `eql` or `=`; fixnums can also be compared with `eq`. To test whether an integer is a fixnum or a bignum, you can compare it to `most-negative-fixnum` and `most-positive-fixnum`, or you can use the convenience predicates `fixnump` and `bignump` on any object.

The read syntax for integers is a sequence of (base ten) digits with an optional sign at the beginning and an optional period at the end. The printed representation produced by the Lisp interpreter never has a leading ‘`+`’ or a final ‘`.`’.

    -1               ; The integer -1.
    1                ; The integer 1.
    1.               ; Also the integer 1.
    +1               ; Also the integer 1.

See [Numbers](Numbers.html), for more information.
