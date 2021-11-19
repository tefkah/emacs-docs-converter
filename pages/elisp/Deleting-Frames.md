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

Next: [Finding All Frames](Finding-All-Frames.html), Previous: [Frame Titles](Frame-Titles.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 29.7 Deleting Frames

A *live frame* is one that has not been deleted. When a frame is deleted, it is removed from its terminal display, although it may continue to exist as a Lisp object until there are no more references to it.

*   Command: **delete-frame** *\&optional frame force*

    This function deletes the frame `frame`. The argument `frame` must specify a live frame (see below) and defaults to the selected frame.

    It first deletes any child frame of `frame` (see [Child Frames](Child-Frames.html)) and any frame whose `delete-before` frame parameter (see [Frame Interaction Parameters](Frame-Interaction-Parameters.html)) specifies `frame`. All such deletions are performed recursively; so this step makes sure that no other frames with `frame` as their ancestor will exist. Then, unless `frame` specifies a tooltip, this function runs the hook `delete-frame-functions` (each function getting one argument, `frame`) before actually killing the frame. After actually killing the frame and removing the frame from the frame list, `delete-frame` runs `after-delete-frame-functions`.

    Note that a frame cannot be deleted as long as its minibuffer serves as surrogate minibuffer for another frame (see [Minibuffers and Frames](Minibuffers-and-Frames.html)). Normally, you cannot delete a frame if all other frames are invisible, but if `force` is non-`nil`, then you are allowed to do so.

<!---->

*   Function: **frame-live-p** *frame*

    This function returns non-`nil` if the frame `frame` has not been deleted. The possible non-`nil` return values are like those of `framep`. See [Frames](Frames.html).

Some window managers provide a command to delete a window. These work by sending a special message to the program that operates the window. When Emacs gets one of these commands, it generates a `delete-frame` event, whose normal definition is a command that calls the function `delete-frame`. See [Misc Events](Misc-Events.html).

*   Command: **delete-other-frames** *\&optional frame*

    This command deletes all frames on `frame`’s terminal, except `frame`. If `frame` uses another frame’s minibuffer, that minibuffer frame is left untouched. The argument `frame` must specify a live frame and defaults to the selected frame. Internally, this command works by calling `delete-frame` with `force` `nil` for all frames that shall be deleted.

    This function does not delete any of `frame`’s child frames (see [Child Frames](Child-Frames.html)). If `frame` is a child frame, it deletes `frame`’s siblings only.

Next: [Finding All Frames](Finding-All-Frames.html), Previous: [Frame Titles](Frame-Titles.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
