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

Next: [Size Parameters](Size-Parameters.html), Previous: [Basic Parameters](Basic-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.4.3.2 Position Parameters

Parameters describing the X- and Y-offsets of a frame are always measured in pixels. For a normal, non-child frame they specify the frame’s outer position (see [Frame Geometry](Frame-Geometry.html)) relative to its display’s origin. For a child frame (see [Child Frames](Child-Frames.html)) they specify the frame’s outer position relative to the native position of the frame’s parent frame. (Note that none of these parameters is meaningful on TTY frames.)

*   `left`

    The position, in pixels, of the left outer edge of the frame with respect to the left edge of the frame’s display or parent frame. It can be specified in one of the following ways.

    *   an integer

        A positive integer always relates the left edge of the frame to the left edge of its display or parent frame. A negative integer relates the right frame edge to the right edge of the display or parent frame.

    *   `(+ pos)`

        This specifies the position of the left frame edge relative to the left edge of its display or parent frame. The integer `pos` may be positive or negative; a negative value specifies a position outside the screen or parent frame or on a monitor other than the primary one (for multi-monitor displays).

    *   `(- pos)`

        This specifies the position of the right frame edge relative to the right edge of the display or parent frame. The integer `pos` may be positive or negative; a negative value specifies a position outside the screen or parent frame or on a monitor other than the primary one (for multi-monitor displays).

    *   a floating-point value

        A floating-point value in the range 0.0 to 1.0 specifies the left edge’s offset via the *left position ratio* of the frame—the ratio of the left edge of its outer frame to the width of the frame’s workarea (see [Multiple Terminals](Multiple-Terminals.html)) or its parent’s native frame (see [Child Frames](Child-Frames.html)) minus the width of the outer frame. Thus, a left position ratio of 0.0 flushes a frame to the left, a ratio of 0.5 centers it and a ratio of 1.0 flushes it to the right of its display or parent frame. Similarly, the *top position ratio* of a frame is the ratio of the frame’s top position to the height of its workarea or parent frame minus the height of the frame.

        Emacs will try to keep the position ratios of a child frame unaltered if that frame has a non-`nil` `keep-ratio` parameter (see [Frame Interaction Parameters](Frame-Interaction-Parameters.html)) and its parent frame is resized.

        Since the outer size of a frame (see [Frame Geometry](Frame-Geometry.html)) is usually unavailable before a frame has been made visible, it is generally not advisable to use floating-point values when creating decorated frames. Floating-point values are more suited for ensuring that an (undecorated) child frame is positioned nicely within the area of its parent frame.

    Some window managers ignore program-specified positions. If you want to be sure the position you specify is not ignored, specify a non-`nil` value for the `user-position` parameter as in the following example:

        (modify-frame-parameters
          nil '((user-position . t) (left . (+ -4))))

    In general, it is not a good idea to position a frame relative to the right or bottom edge of its display. Positioning the initial or a new frame is either not accurate (because the size of the outer frame is not yet fully known before the frame has been made visible) or will cause additional flicker (if the frame has to be repositioned after becoming visible).

    Note also, that positions specified relative to the right/bottom edge of a display, workarea or parent frame as well as floating-point offsets are stored internally as integer offsets relative to the left/top edge of the display, workarea or parent frame edge. They are also returned as such by functions like `frame-parameters` and restored as such by the desktop saving routines.

*   `top`

    The screen position of the top (or bottom) edge, in pixels, with respect to the top (or bottom) edge of the display or parent frame. It works just like `left`, except vertically instead of horizontally.

*   `icon-left`

    The screen position of the left edge of the frame’s icon, in pixels, counting from the left edge of the screen. This takes effect when the frame is iconified, if the window manager supports this feature. If you specify a value for this parameter, then you must also specify a value for `icon-top` and vice versa.

*   `icon-top`

    The screen position of the top edge of the frame’s icon, in pixels, counting from the top edge of the screen. This takes effect when the frame is iconified, if the window manager supports this feature.

*   `user-position`

    When you create a frame and specify its screen position with the `left` and `top` parameters, use this parameter to say whether the specified position was user-specified (explicitly requested in some way by a human user) or merely program-specified (chosen by a program). A non-`nil` value says the position was user-specified.

    Window managers generally heed user-specified positions, and some heed program-specified positions too. But many ignore program-specified positions, placing the window in a default fashion or letting the user place it with the mouse. Some window managers, including `twm`, let the user specify whether to obey program-specified positions or ignore them.

    When you call `make-frame`, you should specify a non-`nil` value for this parameter if the values of the `left` and `top` parameters represent the user’s stated preference; otherwise, use `nil`.

*   `z-group`

    This parameter specifies a relative position of the frame’s window-system window in the stacking (Z-) order of the frame’s display.

    If this is `above`, the window-system will display the window that corresponds to the frame above all other window-system windows that do not have the `above` property set. If this is `nil`, the frame’s window is displayed below all windows that have the `above` property set and above all windows that have the `below` property set. If this is `below`, the frame’s window is displayed below all windows that do not have the `below` property set.

    To position the frame above or below a specific other frame use the function `frame-restack` (see [Raising and Lowering](Raising-and-Lowering.html)).

Next: [Size Parameters](Size-Parameters.html), Previous: [Basic Parameters](Basic-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
