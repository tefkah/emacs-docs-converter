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

Next: [Mouse Position](Mouse-Position.html), Previous: [Child Frames](Child-Frames.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 29.15 Mouse Tracking

Sometimes it is useful to *track* the mouse, which means to display something to indicate where the mouse is and move the indicator as the mouse moves. For efficient mouse tracking, you need a way to wait until the mouse actually moves.

The convenient way to track the mouse is to ask for events to represent mouse motion. Then you can wait for motion by waiting for an event. In addition, you can easily handle any other sorts of events that may occur. That is useful, because normally you don’t want to track the mouse forever—only until some other event, such as the release of a button.

*   Macro: **track-mouse** *body…*

    This macro executes `body`, with generation of mouse motion events enabled. Typically, `body` would use `read-event` to read the motion events and modify the display accordingly. See [Motion Events](Motion-Events.html), for the format of mouse motion events.

    The value of `track-mouse` is that of the last form in `body`. You should design `body` to return when it sees the up-event that indicates the release of the button, or whatever kind of event means it is time to stop tracking.

    The `track-mouse` form causes Emacs to generate mouse motion events by binding the variable `track-mouse` to a non-`nil` value. If that variable has the special value `dragging`, it additionally instructs the display engine to refrain from changing the shape of the mouse pointer. This is desirable in Lisp programs that require mouse dragging across large portions of Emacs display, which might otherwise cause the mouse pointer to change its shape according to the display portion it hovers on (see [Pointer Shape](Pointer-Shape.html)). Therefore, Lisp programs that need the mouse pointer to retain its original shape during dragging should bind `track-mouse` to the value `dragging` at the beginning of their `body`.

The usual purpose of tracking mouse motion is to indicate on the screen the consequences of pushing or releasing a button at the current position.

In many cases, you can avoid the need to track the mouse by using the `mouse-face` text property (see [Special Properties](Special-Properties.html)). That works at a much lower level and runs more smoothly than Lisp-level mouse tracking.

Next: [Mouse Position](Mouse-Position.html), Previous: [Child Frames](Child-Frames.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]