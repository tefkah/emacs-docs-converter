

Next: [Window System Selections](Window-System-Selections.html), Previous: [Dialog Boxes](Dialog-Boxes.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 29.19 Pointer Shape

You can specify the mouse pointer style for particular text or images using the `pointer` text property, and for images with the `:pointer` and `:map` image properties. The values you can use in these properties are `text` (or `nil`), `arrow`, `hand`, `vdrag`, `hdrag`, `modeline`, and `hourglass`. `text` stands for the usual mouse pointer style used over text.

Over void parts of the window (parts that do not correspond to any of the buffer contents), the mouse pointer usually uses the `arrow` style, but you can specify a different style (one of those above) by setting `void-text-area-pointer`.

*   User Option: **void-text-area-pointer**

    This variable specifies the mouse pointer style for void text areas. These include the areas after the end of a line or below the last line in the buffer. The default is to use the `arrow` (non-text) pointer style.

When using X, you can specify what the `text` pointer style really looks like by setting the variable `x-pointer-shape`.

*   Variable: **x-pointer-shape**

    This variable specifies the pointer shape to use ordinarily in the Emacs frame, for the `text` pointer style.

<!---->

*   Variable: **x-sensitive-text-pointer-shape**

    This variable specifies the pointer shape to use when the mouse is over mouse-sensitive text.

These variables affect newly created frames. They do not normally affect existing frames; however, if you set the mouse color of a frame, that also installs the current value of those two variables. See [Font and Color Parameters](Font-and-Color-Parameters.html).

The values you can use, to specify either of these pointer shapes, are defined in the file `lisp/term/x-win.el`. Use `M-x apropos RET x-pointer RET` to see a list of them.

Next: [Window System Selections](Window-System-Selections.html), Previous: [Dialog Boxes](Dialog-Boxes.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
