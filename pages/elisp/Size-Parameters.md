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

Next: [Layout Parameters](Layout-Parameters.html), Previous: [Position Parameters](Position-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.4.3.3 Size Parameters

Frame parameters usually specify frame sizes in character units. On graphical displays, the `default` face determines the actual pixel sizes of these character units (see [Face Attributes](Face-Attributes.html)).

*   `width`

    This parameter specifies the width of the frame. It can be specified as in the following ways:

    *   an integer

        A positive integer specifies the width of the frame’s text area (see [Frame Geometry](Frame-Geometry.html)) in characters.

    *   a cons cell

        If this is a cons cell with the symbol `text-pixels` in its CAR, the CDR of that cell specifies the width of the frame’s text area in pixels.

    *   a floating-point value

        A floating-point number between 0.0 and 1.0 can be used to specify the width of a frame via its *width ratio*—the ratio of its outer width (see [Frame Geometry](Frame-Geometry.html)) to the width of the frame’s workarea (see [Multiple Terminals](Multiple-Terminals.html)) or its parent frame’s (see [Child Frames](Child-Frames.html)) native frame. Thus, a value of 0.5 makes the frame occupy half of the width of its workarea or parent frame, a value of 1.0 the full width. Similarly, the *height ratio* of a frame is the ratio of its outer height to the height of its workarea or its parent’s native frame.

        Emacs will try to keep the width and height ratio of a child frame unaltered if that frame has a non-`nil` `keep-ratio` parameter (see [Frame Interaction Parameters](Frame-Interaction-Parameters.html)) and its parent frame is resized.

        Since the outer size of a frame is usually unavailable before a frame has been made visible, it is generally not advisable to use floating-point values when creating decorated frames. Floating-point values are more suited to ensure that a child frame always fits within the area of its parent frame as, for example, when customizing `display-buffer-alist` (see [Choosing Window](Choosing-Window.html)) via `display-buffer-in-child-frame`.

    Regardless of how this parameter was specified, functions reporting the value of this parameter like `frame-parameters` always report the width of the frame’s text area in characters as an integer rounded, if necessary, to a multiple of the frame’s default character width. That value is also used by the desktop saving routines.

*   `height`

    This parameter specifies the height of the frame. It works just like `width`, except vertically instead of horizontally.

*   `user-size`

    This does for the size parameters `height` and `width` what the `user-position` parameter (see [user-position](Position-Parameters.html)) does for the position parameters `top` and `left`.

*   `min-width`

    This parameter specifies the minimum native width (see [Frame Geometry](Frame-Geometry.html)) of the frame, in characters. Normally, the functions that establish a frame’s initial width or resize a frame horizontally make sure that all the frame’s windows, vertical scroll bars, fringes, margins and vertical dividers can be displayed. This parameter, if non-`nil` allows to make a frame narrower than that with the consequence that any components that do not fit will be clipped by the window manager.

*   `min-height`

    This parameter specifies the minimum native height (see [Frame Geometry](Frame-Geometry.html)) of the frame, in characters. Normally, the functions that establish a frame’s initial size or resize a frame make sure that all the frame’s windows, horizontal scroll bars and dividers, mode and header lines, the echo area and the internal menu and tool bar can be displayed. This parameter, if non-`nil` allows to make a frame smaller than that with the consequence that any components that do not fit will be clipped by the window manager.

*   `fullscreen`

    This parameter specifies whether to maximize the frame’s width, height or both. Its value can be `fullwidth`, `fullheight`, `fullboth`, or `maximized`. A *fullwidth* frame is as wide as possible, a *fullheight* frame is as tall as possible, and a *fullboth* frame is both as wide and as tall as possible. A *maximized* frame is like a “fullboth” frame, except that it usually keeps its title bar and the buttons for resizing and closing the frame. Also, maximized frames typically avoid hiding any task bar or panels displayed on the desktop. A “fullboth” frame, on the other hand, usually omits the title bar and occupies the entire available screen space.

    Full-height and full-width frames are more similar to maximized frames in this regard. However, these typically display an external border which might be absent with maximized frames. Hence the heights of maximized and full-height frames and the widths of maximized and full-width frames often differ by a few pixels.

    With some window managers you may have to customize the variable `frame-resize-pixelwise` (see [Frame Size](Frame-Size.html)) in order to make a frame truly appear maximized or full-screen. Moreover, some window managers might not support smooth transition between the various full-screen or maximization states. Customizing the variable `x-frame-normalize-before-maximize` can help to overcome that.

    Full-screen on macOS hides both the tool-bar and the menu-bar, however both will be displayed if the mouse pointer is moved to the top of the screen.

*   `fullscreen-restore`

    This parameter specifies the desired fullscreen state of the frame after invoking the `toggle-frame-fullscreen` command (see [Frame Commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Commands.html#Frame-Commands) in The GNU Emacs Manual) in the “fullboth” state. Normally this parameter is installed automatically by that command when toggling the state to fullboth. If, however, you start Emacs in the “fullboth” state, you have to specify the desired behavior in your initial file as, for example

        (setq default-frame-alist
            '((fullscreen . fullboth)
              (fullscreen-restore . fullheight)))

    This will give a new frame full height after typing in it `F11` for the first time.

*   `fit-frame-to-buffer-margins`

    This parameter allows to override the value of the option `fit-frame-to-buffer-margins` when fitting this frame to the buffer of its root window with `fit-frame-to-buffer` (see [Resizing Windows](Resizing-Windows.html)).

*   `fit-frame-to-buffer-sizes`

    This parameter allows to override the value of the option `fit-frame-to-buffer-sizes` when fitting this frame to the buffer of its root window with `fit-frame-to-buffer` (see [Resizing Windows](Resizing-Windows.html)).

Next: [Layout Parameters](Layout-Parameters.html), Previous: [Position Parameters](Position-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
