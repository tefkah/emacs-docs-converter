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

Next: [Windows and Frames](Windows-and-Frames.html), Up: [Windows](Windows.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 28.1 Basic Concepts of Emacs Windows

A *window* is an area of the screen that is used to display a buffer (see [Buffers](Buffers.html)). In Emacs Lisp, windows are represented by a special Lisp object type.

Windows are grouped into frames (see [Frames](Frames.html)). Each frame contains at least one window; the user can subdivide it into multiple, non-overlapping windows to view several buffers at once. Lisp programs can use multiple windows for a variety of purposes. In Rmail, for example, you can view a summary of message titles in one window, and the contents of the selected message in another window.

Emacs uses the word “window” with a different meaning than in graphical desktop environments and window systems, such as the X Window System. When Emacs is run on X, each of its graphical X windows is an Emacs frame (containing one or more Emacs windows). When Emacs is run on a text terminal, the frame fills the entire terminal screen.

Unlike X windows, Emacs windows are *tiled*; they never overlap within the area of the frame. When a window is created, resized, or deleted, the change in window space is taken from or given to the adjacent windows, so that the total area of the frame is unchanged.

*   Function: **windowp** *object*

    This function returns `t` if `object` is a window (whether or not it displays a buffer). Otherwise, it returns `nil`.

A *live window* is one that is actually displaying a buffer in a frame.

*   Function: **window-live-p** *object*

    This function returns `t` if `object` is a live window and `nil` otherwise. A live window is one that displays a buffer.

The windows in each frame are organized into a *window tree*. See [Windows and Frames](Windows-and-Frames.html). The leaf nodes of each window tree are live windows—the ones actually displaying buffers. The internal nodes of the window tree are *internal windows*, which are not live.

A *valid window* is one that is either live or internal. A valid window can be *deleted*, i.e., removed from its frame (see [Deleting Windows](Deleting-Windows.html)); then it is no longer valid, but the Lisp object representing it might be still referenced from other Lisp objects. A deleted window may be made valid again by restoring a saved window configuration (see [Window Configurations](Window-Configurations.html)).

You can distinguish valid windows from deleted windows with `window-valid-p`.

*   Function: **window-valid-p** *object*

    This function returns `t` if `object` is a live window, or an internal window in a window tree. Otherwise, it returns `nil`, including for the case where `object` is a deleted window.

In each frame, at any time, exactly one Emacs window is designated as *selected within the frame*. For the selected frame, that window is called the *selected window*—the one in which most editing takes place, and in which the cursor for selected windows appears (see [Cursor Parameters](Cursor-Parameters.html)). Keyboard input that inserts or deletes text is also normally directed to this window. The selected window’s buffer is usually also the current buffer, except when `set-buffer` has been used (see [Current Buffer](Current-Buffer.html)). As for non-selected frames, the window selected within the frame becomes the selected window if the frame is ever selected. See [Selecting Windows](Selecting-Windows.html).

*   Function: **selected-window**

    This function returns the selected window (which is always a live window).

Sometimes several windows collectively and cooperatively display a buffer, for example, under the management of Follow Mode (see [(emacs)Follow Mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Follow-Mode.html#Follow-Mode)), where the windows together display a bigger portion of the buffer than one window could alone. It is often useful to consider such a *window group* as a single entity. Several functions such as `window-group-start` (see [Window Start and End](Window-Start-and-End.html)) allow you to do this by supplying, as an argument, one of the windows as a stand in for the whole group.

*   Function: **selected-window-group**

    When the selected window is a member of a group of windows, this function returns a list of the windows in the group, ordered such that the first window in the list is displaying the earliest part of the buffer, and so on. Otherwise the function returns a list containing just the selected window.

    The selected window is considered part of a group when the buffer local variable `selected-window-group-function` is set to a function. In this case, `selected-window-group` calls it with no arguments and returns its result (which should be the list of windows in the group).

Next: [Windows and Frames](Windows-and-Frames.html), Up: [Windows](Windows.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
