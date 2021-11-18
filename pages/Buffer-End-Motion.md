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

Next: [Text Lines](Text-Lines.html), Previous: [Word Motion](Word-Motion.html), Up: [Motion](Motion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 30.2.3 Motion to an End of the Buffer

To move point to the beginning of the buffer, write:

    (goto-char (point-min))

Likewise, to move to the end of the buffer, use:

    (goto-char (point-max))

Here are two commands that users use to do these things. They are documented here to warn you not to use them in Lisp programs, because they set the mark and display messages in the echo area.

*   Command: **beginning-of-buffer** *\&optional n*

    This function moves point to the beginning of the buffer (or the limits of the accessible portion, when narrowing is in effect), setting the mark at the previous position (except in Transient Mark mode, if the mark is already active, it does not set the mark.)

    If `n` is non-`nil`, then it puts point `n` tenths of the way from the beginning of the accessible portion of the buffer. In an interactive call, `n` is the numeric prefix argument, if provided; otherwise `n` defaults to `nil`.

    **Warning:** Don’t use this function in Lisp programs!

<!---->

*   Command: **end-of-buffer** *\&optional n*

    This function moves point to the end of the buffer (or the limits of the accessible portion, when narrowing is in effect), setting the mark at the previous position (except in Transient Mark mode when the mark is already active). If `n` is non-`nil`, then it puts point `n` tenths of the way from the end of the accessible portion of the buffer.

    In an interactive call, `n` is the numeric prefix argument, if provided; otherwise `n` defaults to `nil`.

    **Warning:** Don’t use this function in Lisp programs!

Next: [Text Lines](Text-Lines.html), Previous: [Word Motion](Word-Motion.html), Up: [Motion](Motion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
