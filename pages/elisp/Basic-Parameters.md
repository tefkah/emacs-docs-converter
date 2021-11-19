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

Next: [Position Parameters](Position-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.4.3.1 Basic Parameters

These frame parameters give the most basic information about the frame. `title` and `name` are meaningful on all terminals.

*   `display`

    The display on which to open this frame. It should be a string of the form ‘`host:dpy.screen`’, just like the `DISPLAY` environment variable. See [Multiple Terminals](Multiple-Terminals.html), for more details about display names.

*   `display-type`

    This parameter describes the range of possible colors that can be used in this frame. Its value is `color`, `grayscale` or `mono`.

*   `title`

    If a frame has a non-`nil` title, it appears in the window system’s title bar at the top of the frame, and also in the mode line of windows in that frame if `mode-line-frame-identification` uses ‘`%F`’ (see [%-Constructs](_0025_002dConstructs.html)). This is normally the case when Emacs is not using a window system, and can only display one frame at a time. See [Frame Titles](Frame-Titles.html).

*   `name`

    The name of the frame. The frame name serves as a default for the frame title, if the `title` parameter is unspecified or `nil`. If you don’t specify a name, Emacs sets the frame name automatically (see [Frame Titles](Frame-Titles.html)).

    If you specify the frame name explicitly when you create the frame, the name is also used (instead of the name of the Emacs executable) when looking up X resources for the frame.

*   `explicit-name`

    If the frame name was specified explicitly when the frame was created, this parameter will be that name. If the frame wasn’t explicitly named, this parameter will be `nil`.

Next: [Position Parameters](Position-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
