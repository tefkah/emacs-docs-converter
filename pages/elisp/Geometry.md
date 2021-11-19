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

Previous: [Window Frame Parameters](Window-Frame-Parameters.html), Up: [Frame Parameters](Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.4.4 Geometry

Here’s how to examine the data in an X-style window geometry specification:

*   Function: **x-parse-geometry** *geom*

    The function `x-parse-geometry` converts a standard X window geometry string to an alist that you can use as part of the argument to `make-frame`.

    The alist describes which parameters were specified in `geom`, and gives the values specified for them. Each element looks like `(parameter . value)`. The possible `parameter` values are `left`, `top`, `width`, and `height`.

    For the size parameters, the value must be an integer. The position parameter names `left` and `top` are not totally accurate, because some values indicate the position of the right or bottom edges instead. The `value` possibilities for the position parameters are: an integer, a list `(+ pos)`, or a list `(- pos)`; as previously described (see [Position Parameters](Position-Parameters.html)).

    Here is an example:

        (x-parse-geometry "35x70+0-0")
             ⇒ ((height . 70) (width . 35)
                 (top - 0) (left . 0))
