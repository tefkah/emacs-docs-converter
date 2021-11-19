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

Next: [Child Frames](Child-Frames.html), Previous: [Raising and Lowering](Raising-and-Lowering.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 29.13 Frame Configurations

A *frame configuration* records the current arrangement of frames, all their properties, and the window configuration of each one. (See [Window Configurations](Window-Configurations.html).)

*   Function: **current-frame-configuration**

    This function returns a frame configuration list that describes the current arrangement of frames and their contents.

<!---->

*   Function: **set-frame-configuration** *configuration \&optional nodelete*

    This function restores the state of frames described in `configuration`. However, this function does not restore deleted frames.

    Ordinarily, this function deletes all existing frames not listed in `configuration`. But if `nodelete` is non-`nil`, the unwanted frames are iconified instead.
