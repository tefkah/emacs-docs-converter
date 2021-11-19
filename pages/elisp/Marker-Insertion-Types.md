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

Next: [Moving Markers](Moving-Markers.html), Previous: [Information from Markers](Information-from-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 31.5 Marker Insertion Types

When you insert text directly at the place where a marker points, there are two possible ways to relocate that marker: it can point before the inserted text, or point after it. You can specify which one a given marker should do by setting its *insertion type*. Note that use of `insert-before-markers` ignores markers’ insertion types, always relocating a marker to point after the inserted text.

*   Function: **set-marker-insertion-type** *marker type*

    This function sets the insertion type of marker `marker` to `type`. If `type` is `t`, `marker` will advance when text is inserted at its position. If `type` is `nil`, `marker` does not advance when text is inserted there.

<!---->

*   Function: **marker-insertion-type** *marker*

    This function reports the current insertion type of `marker`.

All functions that create markers without accepting an argument that specifies the insertion type, create them with insertion type `nil` (see [Creating Markers](Creating-Markers.html)). Also, the mark has, by default, insertion type `nil`.