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

Next: [Frame Position](Frame-Position.html), Previous: [Frame Layout](Frame-Layout.html), Up: [Frame Geometry](Frame-Geometry.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.3.2 Frame Font

Each frame has a *default font* which specifies the default character size for that frame. This size is meant when retrieving or changing the size of a frame in terms of columns or lines (see [Size Parameters](Size-Parameters.html)). It is also used when resizing (see [Window Sizes](Window-Sizes.html)) or splitting (see [Splitting Windows](Splitting-Windows.html)) windows.

The terms *line height* and *canonical character height* are sometimes used instead of “default character height”. Similarly, the terms *column width* and *canonical character width* are used instead of “default character width”.

*   *   Function: **frame-char-height** *\&optional frame*
    *   Function: **frame-char-width** *\&optional frame*

    These functions return the default height and width of a character in `frame`, measured in pixels. Together, these values establish the size of the default font on `frame`. The values depend on the choice of font for `frame`, see [Font and Color Parameters](Font-and-Color-Parameters.html).

The default font can be also set directly with the following function:

*   Command: **set-frame-font** *font \&optional keep-size frames*

    This sets the default font to `font`. When called interactively, it prompts for the name of a font, and uses that font on the selected frame. When called from Lisp, `font` should be a font name (a string), a font object, font entity, or a font spec.

    If the optional argument `keep-size` is `nil`, this keeps the number of frame lines and columns fixed. (If non-`nil`, the option `frame-inhibit-implied-resize` described in the next section will override this.) If `keep-size` is non-`nil` (or with a prefix argument), it tries to keep the size of the display area of the current frame fixed by adjusting the number of lines and columns.

    If the optional argument `frames` is `nil`, this applies the font to the selected frame only. If `frames` is non-`nil`, it should be a list of frames to act upon, or `t` meaning all existing and all future graphical frames.

Next: [Frame Position](Frame-Position.html), Previous: [Frame Layout](Frame-Layout.html), Up: [Frame Geometry](Frame-Geometry.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
