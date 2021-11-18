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

Next: [Overlay Arrow](Overlay-Arrow.html), Previous: [Fringe Bitmaps](Fringe-Bitmaps.html), Up: [Fringes](Fringes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.13.5 Customizing Fringe Bitmaps

*   Function: **define-fringe-bitmap** *bitmap bits \&optional height width align*

    This function defines the symbol `bitmap` as a new fringe bitmap, or replaces an existing bitmap with that name.

    The argument `bits` specifies the image to use. It should be either a string or a vector of integers, where each element (an integer) corresponds to one row of the bitmap. Each bit of an integer corresponds to one pixel of the bitmap, where the low bit corresponds to the rightmost pixel of the bitmap. (Note that this order of bits is opposite of the order in XBM images; see [XBM Images](XBM-Images.html).)

    The height is normally the length of `bits`. However, you can specify a different height with non-`nil` `height`. The width is normally 8, but you can specify a different width with non-`nil` `width`. The width must be an integer between 1 and 16.

    The argument `align` specifies the positioning of the bitmap relative to the range of rows where it is used; the default is to center the bitmap. The allowed values are `top`, `center`, or `bottom`.

    The `align` argument may also be a list `(align periodic)` where `align` is interpreted as described above. If `periodic` is non-`nil`, it specifies that the rows in `bits` should be repeated enough times to reach the specified height.

<!---->

*   Function: **destroy-fringe-bitmap** *bitmap*

    This function destroys the fringe bitmap identified by `bitmap`. If `bitmap` identifies a standard fringe bitmap, it actually restores the standard definition of that bitmap, instead of eliminating it entirely.

<!---->

*   Function: **set-fringe-bitmap-face** *bitmap \&optional face*

    This sets the face for the fringe bitmap `bitmap` to `face`. If `face` is `nil`, it selects the `fringe` face. The bitmap’s face controls the color to draw it in.

    `face` is merged with the `fringe` face, so normally `face` should specify only the foreground color.

Next: [Overlay Arrow](Overlay-Arrow.html), Previous: [Fringe Bitmaps](Fringe-Bitmaps.html), Up: [Fringes](Fringes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
