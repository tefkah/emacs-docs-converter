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

Next: [Minibuffers and Frames](Minibuffers-and-Frames.html), Previous: [Deleting Frames](Deleting-Frames.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 29.8 Finding All Frames

*   Function: **frame-list**

    This function returns a list of all the live frames, i.e., those that have not been deleted. It is analogous to `buffer-list` for buffers, and includes frames on all terminals. The list that you get is newly created, so modifying the list doesn’t have any effect on the internals of Emacs.

<!---->

*   Function: **visible-frame-list**

    This function returns a list of just the currently visible frames. See [Visibility of Frames](Visibility-of-Frames.html). Frames on text terminals always count as visible, even though only the selected one is actually displayed.

<!---->

*   Function: **frame-list-z-order** *\&optional display*

    This function returns a list of Emacs’ frames, in Z (stacking) order (see [Raising and Lowering](Raising-and-Lowering.html)). The optional argument `display` specifies which display to poll. `display` should be either a frame or a display name (a string). If omitted or `nil`, that stands for the selected frame’s display. It returns `nil` if `display` contains no Emacs frame.

    Frames are listed from topmost (first) to bottommost (last). As a special case, if `display` is non-`nil` and specifies a live frame, it returns the child frames of that frame in Z (stacking) order.

    This function is not meaningful on text terminals.

<!---->

*   Function: **next-frame** *\&optional frame minibuf*

    This function lets you cycle conveniently through all the frames on a specific terminal from an arbitrary starting point. It returns the frame following `frame`, in the list of all live frames, on `frame`’s terminal. The argument `frame` must specify a live frame and defaults to the selected frame. It never returns a frame whose `no-other-frame` parameter (see [Frame Interaction Parameters](Frame-Interaction-Parameters.html)) is non-`nil`.

    The second argument, `minibuf`, says which frames to consider:

    *   `nil`

        Exclude minibuffer-only frames.

    *   `visible`

        Consider all visible frames.

    *   0

        Consider all visible or iconified frames.

    *   a window

        Consider only the frames using that particular window as their minibuffer.

    *   anything else

        Consider all frames.

<!---->

*   Function: **previous-frame** *\&optional frame minibuf*

    Like `next-frame`, but cycles through all frames in the opposite direction.

See also `next-window` and `previous-window`, in [Cyclic Window Ordering](Cyclic-Window-Ordering.html).

Next: [Minibuffers and Frames](Minibuffers-and-Frames.html), Previous: [Deleting Frames](Deleting-Frames.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
