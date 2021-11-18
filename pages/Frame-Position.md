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

Next: [Frame Size](Frame-Size.html), Previous: [Frame Font](Frame-Font.html), Up: [Frame Geometry](Frame-Geometry.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.3.3 Frame Position

On graphical systems, the position of a normal top-level frame is specified as the absolute position of its outer frame (see [Frame Geometry](Frame-Geometry.html)). The position of a child frame (see [Child Frames](Child-Frames.html)) is specified via pixel offsets of its outer edges relative to the native position of its parent frame.

You can access or change the position of a frame using the frame parameters `left` and `top` (see [Position Parameters](Position-Parameters.html)). Here are two additional functions for working with the positions of an existing, visible frame. For both functions, the argument `frame` must denote a live frame and defaults to the selected frame.

*   Function: **frame-position** *\&optional frame*

    For a normal, non-child frame this function returns a cons of the pixel coordinates of its outer position (see [Frame Layout](Frame-Layout.html)) with respect to the origin `(0, 0)` of its display. For a child frame (see [Child Frames](Child-Frames.html)) this function returns the pixel coordinates of its outer position with respect to an origin `(0, 0)` at the native position of `frame`’s parent.

    Negative values never indicate an offset from the right or bottom edge of `frame`’s display or parent frame. Rather, they mean that `frame`’s outer position is on the left and/or above the origin of its display or the native position of its parent frame. This usually means that `frame` is only partially visible (or completely invisible). However, on systems where the display’s origin does not coincide with its top-left corner, the frame may be visible on a secondary monitor.

    On a text terminal frame both values are zero.

<!---->

*   Function: **set-frame-position** *frame x y*

    This function sets the outer frame position of `frame` to (`x`, `y`). The latter arguments specify pixels and normally count from the origin at the position (0, 0) of `frame`’s display. For child frames, they count from the native position of `frame`’s parent frame.

    Negative parameter values position the right edge of the outer frame by `-x` pixels left from the right edge of the screen (or the parent frame’s native rectangle) and the bottom edge by `-y` pixels up from the bottom edge of the screen (or the parent frame’s native rectangle).

    Note that negative values do not permit to align the right or bottom edge of `frame` exactly at the right or bottom edge of its display or parent frame. Neither do they allow to specify a position that does not lie within the edges of the display or parent frame. The frame parameters `left` and `top` (see [Position Parameters](Position-Parameters.html)) allow to do that, but may still fail to provide good results for the initial or a new frame.

    This function has no effect on text terminal frames.

<!---->

*   Variable: **move-frame-functions**

    This hook specifies the functions that are run when an Emacs frame is moved (assigned a new position) by the window-system or window manager. The functions are run with one argument, the frame that moved. For a child frame (see [Child Frames](Child-Frames.html)), the functions are run only when the position of the frame changes in relation to that of its parent frame.

Next: [Frame Size](Frame-Size.html), Previous: [Frame Font](Frame-Font.html), Up: [Frame Geometry](Frame-Geometry.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
