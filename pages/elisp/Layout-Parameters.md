

#### 29.4.3.4 Layout Parameters

These frame parameters enable or disable various parts of the frame, or control their sizes.

`border-width`

The width in pixels of the frame’s outer border (see [Frame Geometry](Frame-Geometry.html)).

`internal-border-width`

The width in pixels of the frame’s internal border (see [Frame Geometry](Frame-Geometry.html)).

`vertical-scroll-bars`

Whether the frame has scroll bars (see [Scroll Bars](Scroll-Bars.html)) for vertical scrolling, and which side of the frame they should be on. The possible values are `left`, `right`, and `nil` for no scroll bars.

`horizontal-scroll-bars`

Whether the frame has scroll bars for horizontal scrolling (`t` and `bottom` mean yes, `nil` means no).

`scroll-bar-width`

The width of vertical scroll bars, in pixels, or `nil` meaning to use the default width.

`scroll-bar-height`

The height of horizontal scroll bars, in pixels, or `nil` meaning to use the default height.

*   `left-fringe`
*   `right-fringe`

The default width of the left and right fringes of windows in this frame (see [Fringes](Fringes.html)). If either of these is zero, that effectively removes the corresponding fringe.

When you use `frame-parameter` to query the value of either of these two frame parameters, the return value is always an integer. When using `set-frame-parameter`, passing a `nil` value imposes an actual default value of 8 pixels.

`right-divider-width`

The width (thickness) reserved for the right divider (see [Window Dividers](Window-Dividers.html)) of any window on the frame, in pixels. A value of zero means to not draw right dividers.

`bottom-divider-width`

The width (thickness) reserved for the bottom divider (see [Window Dividers](Window-Dividers.html)) of any window on the frame, in pixels. A value of zero means to not draw bottom dividers.

`menu-bar-lines`

The number of lines to allocate at the top of the frame for a menu bar (see [Menu Bar](Menu-Bar.html)). The default is one if Menu Bar mode is enabled and zero otherwise. See [Menu Bars](https://www.gnu.org/software/emacs/manual/html_node/emacs/Menu-Bars.html#Menu-Bars) in The GNU Emacs Manual. For an external menu bar (see [Frame Layout](Frame-Layout.html)), this value remains unchanged even when the menu bar wraps to two or more lines. In that case, the `menu-bar-size` value returned by `frame-geometry` (see [Frame Geometry](Frame-Geometry.html)) allows to derive whether the menu bar actually occupies one or more lines.

`tool-bar-lines`

The number of lines to use for the tool bar (see [Tool Bar](Tool-Bar.html)). The default is one if Tool Bar mode is enabled and zero otherwise. See [Tool Bars](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tool-Bars.html#Tool-Bars) in The GNU Emacs Manual. This value may change whenever the tool bar wraps (see [Frame Layout](Frame-Layout.html)).

`tool-bar-position`

The position of the tool bar when Emacs was built with GTK+. Its value can be one of `top`, `bottom` `left`, `right`. The default is `top`.

`line-spacing`

Additional space to leave below each text line, in pixels (a positive integer). See [Line Height](Line-Height.html), for more information.

`no-special-glyphs`

If this is non-`nil`, it suppresses the display of any truncation and continuation glyphs (see [Truncation](Truncation.html)) for all buffers displayed by this frame. This is useful to eliminate such glyphs when fitting a frame to its buffer via `fit-frame-to-buffer` (see [Resizing Windows](Resizing-Windows.html)).
