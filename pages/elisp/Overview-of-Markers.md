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

Next: [Predicates on Markers](Predicates-on-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 31.1 Overview of Markers

A marker specifies a buffer and a position in that buffer. A marker can be used to represent a position in functions that require one, just as an integer could be used. In that case, the marker’s buffer is normally ignored. Of course, a marker used in this way usually points to a position in the buffer that the function operates on, but that is entirely the programmer’s responsibility. See [Positions](Positions.html), for a complete description of positions.

A marker has three attributes: the marker position, the marker buffer, and the insertion type. The marker position is an integer that is equivalent (at a given time) to the marker as a position in that buffer. But the marker’s position value can change during the life of the marker, and often does. Insertion and deletion of text in the buffer relocate the marker. The idea is that a marker positioned between two characters remains between those two characters despite insertion and deletion elsewhere in the buffer. Relocation changes the integer equivalent of the marker.

Deleting text around a marker’s position leaves the marker between the characters immediately before and after the deleted text. Inserting text at the position of a marker normally leaves the marker either in front of or after the new text, depending on the marker’s *insertion type* (see [Marker Insertion Types](Marker-Insertion-Types.html))—unless the insertion is done with `insert-before-markers` (see [Insertion](Insertion.html)).

Insertion and deletion in a buffer must check all the markers and relocate them if necessary. This slows processing in a buffer with a large number of markers. For this reason, it is a good idea to make a marker point nowhere if you are sure you don’t need it any more. Markers that can no longer be accessed are eventually removed (see [Garbage Collection](Garbage-Collection.html)).

Because it is common to perform arithmetic operations on a marker position, most of these operations (including `+` and `-`) accept markers as arguments. In such cases, the marker stands for its current position.

Here are examples of creating markers, setting markers, and moving point to markers:

    ;; Make a new marker that initially does not point anywhere:
    (setq m1 (make-marker))
         ⇒ #<marker in no buffer>

```
```

    ;; Set m1 to point between the 99th and 100th characters
    ;;   in the current buffer:
    (set-marker m1 100)
         ⇒ #<marker at 100 in markers.texi>

```
```

    ;; Now insert one character at the beginning of the buffer:
    (goto-char (point-min))
         ⇒ 1
    (insert "Q")
         ⇒ nil

```
```

    ;; m1 is updated appropriately.
    m1
         ⇒ #<marker at 101 in markers.texi>

```
```

    ;; Two markers that point to the same position
    ;;   are not eq, but they are equal.
    (setq m2 (copy-marker m1))
         ⇒ #<marker at 101 in markers.texi>
    (eq m1 m2)
         ⇒ nil
    (equal m1 m2)
         ⇒ t

```
```

    ;; When you are finished using a marker, make it point nowhere.
    (set-marker m1 nil)
         ⇒ #<marker in no buffer>

Next: [Predicates on Markers](Predicates-on-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
