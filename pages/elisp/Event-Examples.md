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

Next: [Classifying Events](Classifying-Events.html), Previous: [Misc Events](Misc-Events.html), Up: [Input Events](Input-Events.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.7.11 Event Examples

If the user presses and releases the left mouse button over the same location, that generates a sequence of events like this:

    (down-mouse-1 (#<window 18 on NEWS> 2613 (0 . 38) -864320))
    (mouse-1      (#<window 18 on NEWS> 2613 (0 . 38) -864180))

While holding the control key down, the user might hold down the second mouse button, and drag the mouse from one line to the next. That produces two events, as shown here:

    (C-down-mouse-2 (#<window 18 on NEWS> 3440 (0 . 27) -731219))
    (C-drag-mouse-2 (#<window 18 on NEWS> 3440 (0 . 27) -731219)
                    (#<window 18 on NEWS> 3510 (0 . 28) -729648))

While holding down the meta and shift keys, the user might press the second mouse button on the window’s mode line, and then drag the mouse into another window. That produces a pair of events like these:

    (M-S-down-mouse-2 (#<window 18 on NEWS> mode-line (33 . 31) -457844))
    (M-S-drag-mouse-2 (#<window 18 on NEWS> mode-line (33 . 31) -457844)
                      (#<window 20 on carlton-sanskrit.tex> 161 (33 . 3)
                       -453816))

The frame with input focus might not take up the entire screen, and the user might move the mouse outside the scope of the frame. Inside the `track-mouse` special form, that produces an event like this:

    (mouse-movement (#<frame *ielm* 0x102849a30> nil (563 . 205) 532301936))

To handle a SIGUSR1 signal, define an interactive function, and bind it to the `signal usr1` event sequence:

    (defun usr1-handler ()
      (interactive)
      (message "Got USR1 signal"))
    (global-set-key [signal usr1] 'usr1-handler)
