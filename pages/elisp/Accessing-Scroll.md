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

Next: [Strings of Events](Strings-of-Events.html), Previous: [Accessing Mouse](Accessing-Mouse.html), Up: [Input Events](Input-Events.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.7.14 Accessing Scroll Bar Events

These functions are useful for decoding scroll bar events.

*   Function: **scroll-bar-event-ratio** *event*

    This function returns the fractional vertical position of a scroll bar event within the scroll bar. The value is a cons cell `(portion . whole)` containing two integers whose ratio is the fractional position.

<!---->

*   Function: **scroll-bar-scale** *ratio total*

    This function multiplies (in effect) `ratio` by `total`, rounding the result to an integer. The argument `ratio` is not a number, but rather a pair `(num . denom)`—typically a value returned by `scroll-bar-event-ratio`.

    This function is handy for scaling a position on a scroll bar into a buffer position. Here’s how to do that:

        (+ (point-min)
           (scroll-bar-scale
              (posn-x-y (event-start event))
              (- (point-max) (point-min))))

    Recall that scroll bar events have two integers forming a ratio, in place of a pair of x and y coordinates.
