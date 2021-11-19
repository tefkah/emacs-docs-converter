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

Next: [Window Dividers](Window-Dividers.html), Previous: [Fringes](Fringes.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 39.14 Scroll Bars

Normally the frame parameter `vertical-scroll-bars` controls whether the windows in the frame have vertical scroll bars, and whether they are on the left or right. The frame parameter `scroll-bar-width` specifies how wide they are (`nil` meaning the default).

The frame parameter `horizontal-scroll-bars` controls whether the windows in the frame have horizontal scroll bars. The frame parameter `scroll-bar-height` specifies how high they are (`nil` meaning the default). See [Layout Parameters](Layout-Parameters.html).

Horizontal scroll bars are not available on all platforms. The function `horizontal-scroll-bars-available-p` which takes no argument returns non-`nil` if they are available on your system.

The following three functions take as argument a live frame which defaults to the selected one.

*   Function: **frame-current-scroll-bars** *\&optional frame*

    This function reports the scroll bar types for frame `frame`. The value is a cons cell `(vertical-type . horizontal-type)`, where `vertical-type` is either `left`, `right`, or `nil` (which means no vertical scroll bar.) `horizontal-type` is either `bottom` or `nil` (which means no horizontal scroll bar).

<!---->

*   Function: **frame-scroll-bar-width** *\&optional frame*

    This function returns the width of vertical scroll bars of `frame` in pixels.

<!---->

*   Function: **frame-scroll-bar-height** *\&optional frame*

    This function returns the height of horizontal scroll bars of `frame` in pixels.

You can override the frame specific settings for individual windows by using the following function:

*   Function: **set-window-scroll-bars** *window \&optional width vertical-type height horizontal-type persistent*

    This function sets the width and/or height and the types of scroll bars for window `window`. If `window` is `nil`, the selected window is used.

    `width` specifies the width of the vertical scroll bar in pixels (`nil` means use the width specified for the frame). `vertical-type` specifies whether to have a vertical scroll bar and, if so, where. The possible values are `left`, `right`, `t`, which means to use the frame’s default, and `nil` for no vertical scroll bar.

    `height` specifies the height of the horizontal scroll bar in pixels (`nil` means use the height specified for the frame). `horizontal-type` specifies whether to have a horizontal scroll bar. The possible values are `bottom`, `t`, which means to use the frame’s default, and `nil` for no horizontal scroll bar. Note that for a mini window the value `t` has the same meaning as `nil`, namely to not show a horizontal scroll bar. You have to explicitly specify `bottom` in order to show a horizontal scroll bar in a mini window.

    If `window` is not large enough to accommodate a scroll bar of the desired dimension, this leaves the corresponding scroll bar unchanged.

    The values specified here may be later overridden by invoking `set-window-buffer` (see [Buffers and Windows](Buffers-and-Windows.html)) on `window` with its `keep-margins` argument `nil` or omitted. However, if the optional fifth argument `persistent` is non-`nil` and the other arguments are processed successfully, the values specified here unconditionally survive subsequent invocations of `set-window-buffer`.

Using the `persistent` argument of `set-window-scroll-bars` and `set-window-fringes` (see [Fringe Size/Pos](Fringe-Size_002fPos.html)) you can reliably and permanently turn off scroll bars and/or fringes in any minibuffer window by adding the following snippet to your early init file (see [Init File](Init-File.html)).

    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-window-scroll-bars
                 (minibuffer-window frame) 0 nil 0 nil t)
                (set-window-fringes
                 (minibuffer-window frame) 0 0 nil t)))

The following four functions take as argument a live window which defaults to the selected one.

*   Function: **window-scroll-bars** *\&optional window*

    This function returns a list of the form `(width columns vertical-type height lines horizontal-type persistent)`.

    The value `width` is the value that was specified for the width of the vertical scroll bar (which may be `nil`); `columns` is the (possibly rounded) number of columns that the vertical scroll bar actually occupies.

    The value `height` is the value that was specified for the height of the horizontal scroll bar (which may be `nil`); `lines` is the (possibly rounded) number of lines that the horizontally scroll bar actually occupies.

    The value of `persistent` is the value specified for `window` with the last successful invocation of `set-window-scroll-bars`, `nil` if there never was one.

<!---->

*   Function: **window-current-scroll-bars** *\&optional window*

    This function reports the scroll bar type for window `window`. The value is a cons cell `(vertical-type . horizontal-type)`. Unlike `window-scroll-bars`, this reports the scroll bar type actually used, once frame defaults and `scroll-bar-mode` are taken into account.

<!---->

*   Function: **window-scroll-bar-width** *\&optional window*

    This function returns the width in pixels of `window`’s vertical scrollbar.

<!---->

*   Function: **window-scroll-bar-height** *\&optional window*

    This function returns the height in pixels of `window`’s horizontal scrollbar.

If you do not specify a window’s scroll bar settings via `set-window-scroll-bars`, the buffer-local variables `vertical-scroll-bar`, `horizontal-scroll-bar`, `scroll-bar-width` and `scroll-bar-height` in the buffer being displayed control the window’s scroll bars. The function `set-window-buffer` examines these variables. If you change them in a buffer that is already visible in a window, you can make the window take note of the new values by calling `set-window-buffer` specifying the same buffer that is already displayed.

You can control the appearance of scroll bars for a particular buffer by setting the following variables which automatically become buffer-local when set.

*   Variable: **vertical-scroll-bar**

    This variable specifies the location of the vertical scroll bar. The possible values are `left`, `right`, `t`, which means to use the frame’s default, and `nil` for no scroll bar.

<!---->

*   Variable: **horizontal-scroll-bar**

    This variable specifies the location of the horizontal scroll bar. The possible values are `bottom`, `t`, which means to use the frame’s default, and `nil` for no scroll bar.

<!---->

*   Variable: **scroll-bar-width**

    This variable specifies the width of the buffer’s vertical scroll bars, measured in pixels. A value of `nil` means to use the value specified by the frame.

<!---->

*   Variable: **scroll-bar-height**

    This variable specifies the height of the buffer’s horizontal scroll bar, measured in pixels. A value of `nil` means to use the value specified by the frame.

Finally you can toggle the display of scroll bars on all frames by customizing the variables `scroll-bar-mode` and `horizontal-scroll-bar-mode`.

*   User Option: **scroll-bar-mode**

    This variable controls whether and where to put vertical scroll bars in all frames. The possible values are `nil` for no scroll bars, `left` to put scroll bars on the left and `right` to put scroll bars on the right.

<!---->

*   User Option: **horizontal-scroll-bar-mode**

    This variable controls whether to display horizontal scroll bars on all frames.

Next: [Window Dividers](Window-Dividers.html), Previous: [Fringes](Fringes.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
