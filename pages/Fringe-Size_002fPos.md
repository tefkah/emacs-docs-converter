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

Next: [Fringe Indicators](Fringe-Indicators.html), Up: [Fringes](Fringes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.13.1 Fringe Size and Position

The following buffer-local variables control the position and width of fringes in windows showing that buffer.

*   Variable: **fringes-outside-margins**

    The fringes normally appear between the display margins and the window text. If the value is non-`nil`, they appear outside the display margins. See [Display Margins](Display-Margins.html).

<!---->

*   Variable: **left-fringe-width**

    This variable, if non-`nil`, specifies the width of the left fringe in pixels. A value of `nil` means to use the left fringe width from the window’s frame.

<!---->

*   Variable: **right-fringe-width**

    This variable, if non-`nil`, specifies the width of the right fringe in pixels. A value of `nil` means to use the right fringe width from the window’s frame.

Any buffer which does not specify values for these variables uses the values specified by the `left-fringe` and `right-fringe` frame parameters (see [Layout Parameters](Layout-Parameters.html)).

The above variables actually take effect via the function `set-window-buffer` (see [Buffers and Windows](Buffers-and-Windows.html)), which calls `set-window-fringes` as a subroutine. If you change one of these variables, the fringe display is not updated in existing windows showing the buffer, unless you call `set-window-buffer` again in each affected window. You can also use `set-window-fringes` to control the fringe display in individual windows.

*   Function: **set-window-fringes** *window left \&optional right outside-margins persistent*

    This function sets the fringe widths of window `window`. If `window` is `nil`, the selected window is used.

    The argument `left` specifies the width in pixels of the left fringe, and likewise `right` for the right fringe. A value of `nil` for either one stands for the default width. If `outside-margins` is non-`nil`, that specifies that fringes should appear outside of the display margins.

    If `window` is not large enough to accommodate fringes of the desired width, this leaves the fringes of `window` unchanged.

    The values specified here may be later overridden by invoking `set-window-buffer` (see [Buffers and Windows](Buffers-and-Windows.html)) on `window` with its `keep-margins` argument `nil` or omitted. However, if the optional fifth argument `persistent` is non-`nil` and the other arguments are processed successfully, the values specified here unconditionally survive subsequent invocations of `set-window-buffer`. This can be used to permanently turn off fringes in the minibuffer window, consult the description of `set-window-scroll-bars` for an example (see [Scroll Bars](Scroll-Bars.html)).

<!---->

*   Function: **window-fringes** *\&optional window*

    This function returns information about the fringes of a window `window`. If `window` is omitted or `nil`, the selected window is used. The value has the form `(left-width right-width outside-margins persistent)`.

Next: [Fringe Indicators](Fringe-Indicators.html), Up: [Fringes](Fringes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
