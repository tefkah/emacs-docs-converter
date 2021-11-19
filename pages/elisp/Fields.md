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

Next: [Not Intervals](Not-Intervals.html), Previous: [Clickable Text](Clickable-Text.html), Up: [Text Properties](Text-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.19.9 Defining and Using Fields

A field is a range of consecutive characters in the buffer that are identified by having the same value (comparing with `eq`) of the `field` property (either a text-property or an overlay property). This section describes special functions that are available for operating on fields.

You specify a field with a buffer position, `pos`. We think of each field as containing a range of buffer positions, so the position you specify stands for the field containing that position.

When the characters before and after `pos` are part of the same field, there is no doubt which field contains `pos`: the one those characters both belong to. When `pos` is at a boundary between fields, which field it belongs to depends on the stickiness of the `field` properties of the two surrounding characters (see [Sticky Properties](Sticky-Properties.html)). The field whose property would be inherited by text inserted at `pos` is the field that contains `pos`.

There is an anomalous case where newly inserted text at `pos` would not inherit the `field` property from either side. This happens if the previous character’s `field` property is not rear-sticky, and the following character’s `field` property is not front-sticky. In this case, `pos` belongs to neither the preceding field nor the following field; the field functions treat it as belonging to an empty field whose beginning and end are both at `pos`.

In all of these functions, if `pos` is omitted or `nil`, the value of point is used by default. If narrowing is in effect, then `pos` should fall within the accessible portion. See [Narrowing](Narrowing.html).

*   Function: **field-beginning** *\&optional pos escape-from-edge limit*

    This function returns the beginning of the field specified by `pos`.

    If `pos` is at the beginning of its field, and `escape-from-edge` is non-`nil`, then the return value is always the beginning of the preceding field that *ends* at `pos`, regardless of the stickiness of the `field` properties around `pos`.

    If `limit` is non-`nil`, it is a buffer position; if the beginning of the field is before `limit`, then `limit` will be returned instead.

<!---->

*   Function: **field-end** *\&optional pos escape-from-edge limit*

    This function returns the end of the field specified by `pos`.

    If `pos` is at the end of its field, and `escape-from-edge` is non-`nil`, then the return value is always the end of the following field that *begins* at `pos`, regardless of the stickiness of the `field` properties around `pos`.

    If `limit` is non-`nil`, it is a buffer position; if the end of the field is after `limit`, then `limit` will be returned instead.

<!---->

*   Function: **field-string** *\&optional pos*

    This function returns the contents of the field specified by `pos`, as a string.

<!---->

*   Function: **field-string-no-properties** *\&optional pos*

    This function returns the contents of the field specified by `pos`, as a string, discarding text properties.

<!---->

*   Function: **delete-field** *\&optional pos*

    This function deletes the text of the field specified by `pos`.

<!---->

*   Function: **constrain-to-field** *new-pos old-pos \&optional escape-from-edge only-in-line inhibit-capture-property*

    This function constrains `new-pos` to the field that `old-pos` belongs to—in other words, it returns the position closest to `new-pos` that is in the same field as `old-pos`.

    If `new-pos` is `nil`, then `constrain-to-field` uses the value of point instead, and moves point to the resulting position in addition to returning that position.

    If `old-pos` is at the boundary of two fields, then the acceptable final positions depend on the argument `escape-from-edge`. If `escape-from-edge` is `nil`, then `new-pos` must be in the field whose `field` property equals what new characters inserted at `old-pos` would inherit. (This depends on the stickiness of the `field` property for the characters before and after `old-pos`.) If `escape-from-edge` is non-`nil`, `new-pos` can be anywhere in the two adjacent fields. Additionally, if two fields are separated by another field with the special value `boundary`, then any point within this special field is also considered to be on the boundary.

    Commands like `C-a` with no argument, that normally move backward to a specific kind of location and stay there once there, probably should specify `nil` for `escape-from-edge`. Other motion commands that check fields should probably pass `t`.

    If the optional argument `only-in-line` is non-`nil`, and constraining `new-pos` in the usual way would move it to a different line, `new-pos` is returned unconstrained. This used in commands that move by line, such as `next-line` and `beginning-of-line`, so that they respect field boundaries only in the case where they can still move to the right line.

    If the optional argument `inhibit-capture-property` is non-`nil`, and `old-pos` has a non-`nil` property of that name, then any field boundaries are ignored.

    You can cause `constrain-to-field` to ignore all field boundaries (and so never constrain anything) by binding the variable `inhibit-field-text-motion` to a non-`nil` value.

Next: [Not Intervals](Not-Intervals.html), Previous: [Clickable Text](Clickable-Text.html), Up: [Text Properties](Text-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
