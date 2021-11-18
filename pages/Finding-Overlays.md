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

Previous: [Overlay Properties](Overlay-Properties.html), Up: [Overlays](Overlays.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.9.3 Searching for Overlays

*   Function: **overlays-at** *pos \&optional sorted*

    This function returns a list of all the overlays that cover the character at position `pos` in the current buffer. If `sorted` is non-`nil`, the list is in decreasing order of priority, otherwise it is in no particular order. An overlay contains position `pos` if it begins at or before `pos`, and ends after `pos`.

    To illustrate usage, here is a Lisp function that returns a list of the overlays that specify property `prop` for the character at point:

        (defun find-overlays-specifying (prop)
          (let ((overlays (overlays-at (point)))
                found)
            (while overlays
              (let ((overlay (car overlays)))
                (if (overlay-get overlay prop)
                    (setq found (cons overlay found))))
              (setq overlays (cdr overlays)))
            found))

<!---->

*   Function: **overlays-in** *beg end*

    This function returns a list of the overlays that overlap the region `beg` through `end`. An overlay overlaps with a region if it contains one or more characters in the region; empty overlays (see [empty overlay](Managing-Overlays.html)) overlap if they are at `beg`, strictly between `beg` and `end`, or at `end` when `end` denotes the position at the end of the buffer.

<!---->

*   Function: **next-overlay-change** *pos*

    This function returns the buffer position of the next beginning or end of an overlay, after `pos`. If there is none, it returns `(point-max)`.

<!---->

*   Function: **previous-overlay-change** *pos*

    This function returns the buffer position of the previous beginning or end of an overlay, before `pos`. If there is none, it returns `(point-min)`.

As an example, here’s a simplified (and inefficient) version of the primitive function `next-single-char-property-change` (see [Property Search](Property-Search.html)). It searches forward from position `pos` for the next position where the value of a given property `prop`, as obtained from either overlays or text properties, changes.

    (defun next-single-char-property-change (position prop)
      (save-excursion
        (goto-char position)
        (let ((propval (get-char-property (point) prop)))
          (while (and (not (eobp))
                      (eq (get-char-property (point) prop) propval))
            (goto-char (min (next-overlay-change (point))
                            (next-single-property-change (point) prop)))))
        (point)))

Previous: [Overlay Properties](Overlay-Properties.html), Up: [Overlays](Overlays.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
