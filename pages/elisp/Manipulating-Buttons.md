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

Next: [Button Buffer Commands](Button-Buffer-Commands.html), Previous: [Making Buttons](Making-Buttons.html), Up: [Buttons](Buttons.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.19.4 Manipulating Buttons

These are functions for getting and setting properties of buttons. Often these are used by a button’s invocation function to determine what to do.

Where a `button` parameter is specified, it means an object referring to a specific button, either an overlay (for overlay buttons), or a buffer-position or marker (for text property buttons). Such an object is passed as the first argument to a button’s invocation function when it is invoked.

*   Function: **button-start** *button*

    Return the position at which `button` starts.

<!---->

*   Function: **button-end** *button*

    Return the position at which `button` ends.

<!---->

*   Function: **button-get** *button prop*

    Get the property of button `button` named `prop`.

<!---->

*   Function: **button-put** *button prop val*

    Set `button`’s `prop` property to `val`.

<!---->

*   Function: **button-activate** *button \&optional use-mouse-action*

    Call `button`’s `action` property (i.e., invoke the function that is the value of that property, passing it the single argument `button`). If `use-mouse-action` is non-`nil`, try to invoke the button’s `mouse-action` property instead of `action`; if the button has no `mouse-action` property, use `action` as normal. If the `button-data` property is present in `button`, use that as the argument for the `action` function instead of `button`.

<!---->

*   Function: **button-label** *button*

    Return `button`’s text label.

<!---->

*   Function: **button-type** *button*

    Return `button`’s button-type.

<!---->

*   Function: **button-has-type-p** *button type*

    Return `t` if `button` has button-type `type`, or one of `type`’s subtypes.

<!---->

*   Function: **button-at** *pos*

    Return the button at position `pos` in the current buffer, or `nil`. If the button at `pos` is a text property button, the return value is a marker pointing to `pos`.

<!---->

*   Function: **button-type-put** *type prop val*

    Set the button-type `type`’s `prop` property to `val`.

<!---->

*   Function: **button-type-get** *type prop*

    Get the property of button-type `type` named `prop`.

<!---->

*   Function: **button-type-subtype-p** *type supertype*

    Return `t` if button-type `type` is a subtype of `supertype`.

Next: [Button Buffer Commands](Button-Buffer-Commands.html), Previous: [Making Buttons](Making-Buttons.html), Up: [Buttons](Buttons.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
