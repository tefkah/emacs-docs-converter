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

Next: [Side Window Options and Functions](Side-Window-Options-and-Functions.html), Up: [Side Windows](Side-Windows.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 28.17.1 Displaying Buffers in Side Windows

The following action function for `display-buffer` (see [Buffer Display Action Functions](Buffer-Display-Action-Functions.html)) creates or reuses a side window for displaying the specified buffer.

*   Function: **display-buffer-in-side-window** *buffer alist*

    This function displays `buffer` in a side window of the selected frame. It returns the window used for displaying `buffer`, `nil` if no such window can be found or created.

    `alist` is an association list of symbols and values as for `display-buffer`. The following symbols in `alist` are special for this function:

    *   `side`

        Denotes the side of the frame where the window shall be located. Valid values are `left`, `top`, `right` and `bottom`. If unspecified, the window is located at the bottom of the frame.

    *   `slot`

        Denotes a slot at the specified side where to locate the window. A value of zero means to preferably position the window in the middle of the specified side. A negative value means to use a slot preceding (that is, above or on the left of) the middle slot. A positive value means to use a slot following (that is, below or on the right of) the middle slot. Hence, all windows on a specific side are ordered by their `slot` value. If unspecified, the window is located in the middle of the specified side.

    If you specify the same slot on the same side for two or more different buffers, the buffer displayed last is shown in the corresponding window. Hence, slots can be used for sharing the same side window between buffers.

    This function installs the `window-side` and `window-slot` parameters (see [Window Parameters](Window-Parameters.html)) and makes them persistent. It does not install any other window parameters unless they have been explicitly provided via a `window-parameters` entry in `alist`.

By default, side windows cannot be split via `split-window` (see [Splitting Windows](Splitting-Windows.html)). Also, a side window is not reused or split by any buffer display action (see [Buffer Display Action Functions](Buffer-Display-Action-Functions.html)) unless it is explicitly specified as target of that action. Note also that `delete-other-windows` cannot make a side window the only window on its frame (see [Deleting Windows](Deleting-Windows.html)).

Once set up, side windows also change the behavior of the commands `switch-to-prev-buffer` and `switch-to-next-buffer` (see [Window History](Window-History.html)). In particular, these commands will refrain from showing, in a side window, buffers that have not been displayed in that window before. They will also refrain from having a normal, non-side window show a buffer that has been already displayed in a side window. A notable exception to the latter rule occurs when an application, after displaying a buffer, resets that buffer’s local variables.

Next: [Side Window Options and Functions](Side-Window-Options-and-Functions.html), Up: [Side Windows](Side-Windows.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
