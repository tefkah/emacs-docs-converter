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

Next: [Customizing Bitmaps](Customizing-Bitmaps.html), Previous: [Fringe Cursors](Fringe-Cursors.html), Up: [Fringes](Fringes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.13.4 Fringe Bitmaps

The *fringe bitmaps* are the actual bitmaps which represent the logical fringe indicators for truncated or continued lines, buffer boundaries, overlay arrows, etc. Each bitmap is represented by a symbol. These symbols are referred to by the variable `fringe-indicator-alist`, which maps fringe indicators to bitmaps (see [Fringe Indicators](Fringe-Indicators.html)), and the variable `fringe-cursor-alist`, which maps fringe cursors to bitmaps (see [Fringe Cursors](Fringe-Cursors.html)).

Lisp programs can also directly display a bitmap in the left or right fringe, by using a `display` property for one of the characters appearing in the line (see [Other Display Specs](Other-Display-Specs.html)). Such a display specification has the form

    (fringe bitmap [face])

`fringe` is either the symbol `left-fringe` or `right-fringe`. `bitmap` is a symbol identifying the bitmap to display. The optional `face` names a face whose foreground and background colors are to be used to display the bitmap, using the attributes of the `fringe` face for colors that `face` didn’t specify. If `face` is omitted, that means to use the attributes of the `default` face for the colors which the `fringe` face didn’t specify. For predictable results that don’t depend on the attributes of the `default` and `fringe` faces, we recommend you never omit `face`, but always provide a specific face. In particular, if you want the bitmap to be always displayed in the `fringe` face, use `fringe` as `face`.

For instance, to display an arrow in the left fringe, using the `warning` face, you could say something like:

    (overlay-put
     (make-overlay (point) (point))
     'before-string (propertize
                     "x" 'display
                     `(left-fringe right-arrow warning)))

Here is a list of the standard fringe bitmaps defined in Emacs, and how they are currently used in Emacs (via `fringe-indicator-alist` and `fringe-cursor-alist`):

*   `left-arrow`, `right-arrow`

    Used to indicate truncated lines.

*   `left-curly-arrow`, `right-curly-arrow`

    Used to indicate continued lines.

*   `right-triangle`, `left-triangle`

    The former is used by overlay arrows. The latter is unused.

*   *   `up-arrow`, `down-arrow`
    *   `bottom-left-angle`, `bottom-right-angle`
    *   `top-left-angle`, `top-right-angle`
    *   `left-bracket`, `right-bracket`
    *   `empty-line`

    Used to indicate buffer boundaries.

*   *   `filled-rectangle`, `hollow-rectangle`
    *   `filled-square`, `hollow-square`
    *   `vertical-bar`, `horizontal-bar`

    Used for different types of fringe cursors.

*   `exclamation-mark`, `question-mark`

    Not used by core Emacs features.

The next subsection describes how to define your own fringe bitmaps.

*   Function: **fringe-bitmaps-at-pos** *\&optional pos window*

    This function returns the fringe bitmaps of the display line containing position `pos` in window `window`. The return value has the form `(left right ov)`, where `left` is the symbol for the fringe bitmap in the left fringe (or `nil` if no bitmap), `right` is similar for the right fringe, and `ov` is non-`nil` if there is an overlay arrow in the left fringe.

    The value is `nil` if `pos` is not visible in `window`. If `window` is `nil`, that stands for the selected window. If `pos` is `nil`, that stands for the value of point in `window`.

Next: [Customizing Bitmaps](Customizing-Bitmaps.html), Previous: [Fringe Cursors](Fringe-Cursors.html), Up: [Fringes](Fringes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
