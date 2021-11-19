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

Next: [Focus Events](Focus-Events.html), Previous: [Repeat Events](Repeat-Events.html), Up: [Input Events](Input-Events.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.7.8 Motion Events

Emacs sometimes generates *mouse motion* events to describe motion of the mouse without any button activity. Mouse motion events are represented by lists that look like this:

    (mouse-movement POSITION)

`position` is a mouse position list (see [Click Events](Click-Events.html)), specifying the current position of the mouse cursor. As with the end-position of a drag event, this position list may represent a location outside the boundaries of the initially selected frame, in which case the list contains that frame in place of a window.

The special form `track-mouse` enables generation of motion events within its body. Outside of `track-mouse` forms, Emacs does not generate events for mere motion of the mouse, and these events do not appear. See [Mouse Tracking](Mouse-Tracking.html).

*   Variable: **mouse-fine-grained-tracking**

    When non-`nil`, mouse motion events are generated even for very small movements. Otherwise, motion events are not generated as long as the mouse cursor remains pointing to the same glyph in the text.
