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

Next: [Information from Markers](Information-from-Markers.html), Previous: [Predicates on Markers](Predicates-on-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 31.3 Functions that Create Markers

When you create a new marker, you can make it point nowhere, or point to the present position of point, or to the beginning or end of the accessible portion of the buffer, or to the same place as another given marker.

The next four functions all return markers with insertion type `nil`. See [Marker Insertion Types](Marker-Insertion-Types.html).

*   Function: **make-marker**

    This function returns a newly created marker that does not point anywhere.

        (make-marker)
             ⇒ #<marker in no buffer>

<!---->

*   Function: **point-marker**

    This function returns a new marker that points to the present position of point in the current buffer. See [Point](Point.html). For an example, see `copy-marker`, below.

<!---->

*   Function: **point-min-marker**

    This function returns a new marker that points to the beginning of the accessible portion of the buffer. This will be the beginning of the buffer unless narrowing is in effect. See [Narrowing](Narrowing.html).

<!---->

*   Function: **point-max-marker**

    This function returns a new marker that points to the end of the accessible portion of the buffer. This will be the end of the buffer unless narrowing is in effect. See [Narrowing](Narrowing.html).

    Here are examples of this function and `point-min-marker`, shown in a buffer containing a version of the source file for the text of this chapter.

        (point-min-marker)
             ⇒ #<marker at 1 in markers.texi>
        (point-max-marker)
             ⇒ #<marker at 24080 in markers.texi>

    ```
    ```

        (narrow-to-region 100 200)
             ⇒ nil

    <!---->

        (point-min-marker)
             ⇒ #<marker at 100 in markers.texi>

    <!---->

        (point-max-marker)
             ⇒ #<marker at 200 in markers.texi>

<!---->

*   Function: **copy-marker** *\&optional marker-or-integer insertion-type*

    If passed a marker as its argument, `copy-marker` returns a new marker that points to the same place and the same buffer as does `marker-or-integer`. If passed an integer as its argument, `copy-marker` returns a new marker that points to position `marker-or-integer` in the current buffer.

    The new marker’s insertion type is specified by the argument `insertion-type`. See [Marker Insertion Types](Marker-Insertion-Types.html).

        (copy-marker 0)
             ⇒ #<marker at 1 in markers.texi>

    ```
    ```

        (copy-marker 90000)
             ⇒ #<marker at 24080 in markers.texi>

    An error is signaled if `marker` is neither a marker nor an integer.

Two distinct markers are considered `equal` (even though not `eq`) to each other if they have the same position and buffer, or if they both point nowhere.

    (setq p (point-marker))
         ⇒ #<marker at 2139 in markers.texi>

```
```

    (setq q (copy-marker p))
         ⇒ #<marker at 2139 in markers.texi>

```
```

    (eq p q)
         ⇒ nil

```
```

    (equal p q)
         ⇒ t

Next: [Information from Markers](Information-from-Markers.html), Previous: [Predicates on Markers](Predicates-on-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]