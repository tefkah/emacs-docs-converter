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

Next: [Overlay Properties](Overlay-Properties.html), Up: [Overlays](Overlays.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.9.1 Managing Overlays

This section describes the functions to create, delete and move overlays, and to examine their contents. Overlay changes are not recorded in the buffer’s undo list, since the overlays are not part of the buffer’s contents.

*   Function: **overlayp** *object*

    This function returns `t` if `object` is an overlay.

<!---->

*   Function: **make-overlay** *start end \&optional buffer front-advance rear-advance*

    This function creates and returns an overlay that belongs to `buffer` and ranges from `start` to `end`. Both `start` and `end` must specify buffer positions; they may be integers or markers. If `buffer` is omitted, the overlay is created in the current buffer.

    An overlay whose `start` and `end` specify the same buffer position is known as *empty*. A non-empty overlay can become empty if the text between its `start` and `end` is deleted. When that happens, the overlay is by default not deleted, but you can cause it to be deleted by giving it the ‘`evaporate`’ property (see [evaporate property](Overlay-Properties.html)).

    The arguments `front-advance` and `rear-advance` specify the marker insertion type for the start of the overlay and for the end of the overlay, respectively. See [Marker Insertion Types](Marker-Insertion-Types.html). If they are both `nil`, the default, then the overlay extends to include any text inserted at the beginning, but not text inserted at the end. If `front-advance` is non-`nil`, text inserted at the beginning of the overlay is excluded from the overlay. If `rear-advance` is non-`nil`, text inserted at the end of the overlay is included in the overlay.

<!---->

*   Function: **overlay-start** *overlay*

    This function returns the position at which `overlay` starts, as an integer.

<!---->

*   Function: **overlay-end** *overlay*

    This function returns the position at which `overlay` ends, as an integer.

<!---->

*   Function: **overlay-buffer** *overlay*

    This function returns the buffer that `overlay` belongs to. It returns `nil` if `overlay` has been deleted.

<!---->

*   Function: **delete-overlay** *overlay*

    This function deletes `overlay`. The overlay continues to exist as a Lisp object, and its property list is unchanged, but it ceases to be attached to the buffer it belonged to, and ceases to have any effect on display.

    A deleted overlay is not permanently disconnected. You can give it a position in a buffer again by calling `move-overlay`.

<!---->

*   Function: **move-overlay** *overlay start end \&optional buffer*

    This function moves `overlay` to `buffer`, and places its bounds at `start` and `end`. Both arguments `start` and `end` must specify buffer positions; they may be integers or markers.

    If `buffer` is omitted, `overlay` stays in the same buffer it was already associated with; if `overlay` was deleted, it goes into the current buffer.

    The return value is `overlay`.

    This is the only valid way to change the endpoints of an overlay. Do not try modifying the markers in the overlay by hand, as that fails to update other vital data structures and can cause some overlays to be lost.

<!---->

*   Function: **remove-overlays** *\&optional start end name value*

    This function removes all the overlays between `start` and `end` whose property `name` has the value `value`. It can move the endpoints of the overlays in the region, or split them.

    If `name` is omitted or `nil`, it means to delete all overlays in the specified region. If `start` and/or `end` are omitted or `nil`, that means the beginning and end of the buffer respectively. Therefore, `(remove-overlays)` removes all the overlays in the current buffer.

<!---->

*   Function: **copy-overlay** *overlay*

    This function returns a copy of `overlay`. The copy has the same endpoints and properties as `overlay`. However, the marker insertion type for the start of the overlay and for the end of the overlay are set to their default values (see [Marker Insertion Types](Marker-Insertion-Types.html)).

Here are some examples:

    ;; Create an overlay.
    (setq foo (make-overlay 1 10))
         ⇒ #<overlay from 1 to 10 in display.texi>
    (overlay-start foo)
         ⇒ 1
    (overlay-end foo)
         ⇒ 10
    (overlay-buffer foo)
         ⇒ #<buffer display.texi>
    ;; Give it a property we can check later.
    (overlay-put foo 'happy t)
         ⇒ t
    ;; Verify the property is present.
    (overlay-get foo 'happy)
         ⇒ t
    ;; Move the overlay.
    (move-overlay foo 5 20)
         ⇒ #<overlay from 5 to 20 in display.texi>
    (overlay-start foo)
         ⇒ 5
    (overlay-end foo)
         ⇒ 20
    ;; Delete the overlay.
    (delete-overlay foo)
         ⇒ nil
    ;; Verify it is deleted.
    foo
         ⇒ #<overlay in no buffer>
    ;; A deleted overlay has no position.
    (overlay-start foo)
         ⇒ nil
    (overlay-end foo)
         ⇒ nil
    (overlay-buffer foo)
         ⇒ nil
    ;; Undelete the overlay.
    (move-overlay foo 1 20)
         ⇒ #<overlay from 1 to 20 in display.texi>
    ;; Verify the results.
    (overlay-start foo)
         ⇒ 1
    (overlay-end foo)
         ⇒ 20
    (overlay-buffer foo)
         ⇒ #<buffer display.texi>
    ;; Moving and deleting the overlay does not change its properties.
    (overlay-get foo 'happy)
         ⇒ t

Emacs stores the overlays of each buffer in two lists, divided around an arbitrary center position. One list extends backwards through the buffer from that center position, and the other extends forwards from that center position. The center position can be anywhere in the buffer.

*   Function: **overlay-recenter** *pos*

    This function recenters the overlays of the current buffer around position `pos`. That makes overlay lookup faster for positions near `pos`, but slower for positions far away from `pos`.

A loop that scans the buffer forwards, creating overlays, can run faster if you do `(overlay-recenter (point-max))` first.

Next: [Overlay Properties](Overlay-Properties.html), Up: [Overlays](Overlays.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
