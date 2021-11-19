

Next: [Glyphs](Glyphs.html), Previous: [Display Tables](Display-Tables.html), Up: [Character Display](Character-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.22.3 Active Display Table

Each window can specify a display table, and so can each buffer. The window’s display table, if there is one, takes precedence over the buffer’s display table. If neither exists, Emacs tries to use the standard display table; if that is `nil`, Emacs uses the usual character display conventions (see [Usual Display](Usual-Display.html)).

Note that display tables affect how the mode line is displayed, so if you want to force redisplay of the mode line using a new display table, call `force-mode-line-update` (see [Mode Line Format](Mode-Line-Format.html)).

*   Function: **window-display-table** *\&optional window*

    This function returns `window`’s display table, or `nil` if there is none. The default for `window` is the selected window.

<!---->

*   Function: **set-window-display-table** *window table*

    This function sets the display table of `window` to `table`. The argument `table` should be either a display table or `nil`.

<!---->

*   Variable: **buffer-display-table**

    This variable is automatically buffer-local in all buffers; its value specifies the buffer’s display table. If it is `nil`, there is no buffer display table.

<!---->

*   Variable: **standard-display-table**

    The value of this variable is the standard display table, which is used when Emacs is displaying a buffer in a window with neither a window display table nor a buffer display table defined, or when Emacs is outputting text to the standard output or error streams. Although its default is typically `nil`, in an interactive session if the terminal cannot display curved quotes, its default maps curved quotes to ASCII approximations. See [Text Quoting Style](Text-Quoting-Style.html).

The `disp-table` library defines several functions for changing the standard display table.

Next: [Glyphs](Glyphs.html), Previous: [Display Tables](Display-Tables.html), Up: [Character Display](Character-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
