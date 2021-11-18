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

Next: [The Mark](The-Mark.html), Previous: [Marker Insertion Types](Marker-Insertion-Types.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 31.6 Moving Marker Positions

This section describes how to change the position of an existing marker. When you do this, be sure you know whether the marker is used outside of your program, and, if so, what effects will result from moving it—otherwise, confusing things may happen in other parts of Emacs.

*   Function: **set-marker** *marker position \&optional buffer*

    This function moves `marker` to `position` in `buffer`. If `buffer` is not provided, it defaults to the current buffer.

    If `position` is `nil` or a marker that points nowhere, then `marker` is set to point nowhere.

    The value returned is `marker`.

        (setq m (point-marker))
             ⇒ #<marker at 4714 in markers.texi>

    <!---->

        (set-marker m 55)
             ⇒ #<marker at 55 in markers.texi>

    <!---->

        (setq b (get-buffer "foo"))
             ⇒ #<buffer foo>

    <!---->

        (set-marker m 0 b)
             ⇒ #<marker at 1 in foo>

<!---->

*   Function: **move-marker** *marker position \&optional buffer*

    This is another name for `set-marker`.
