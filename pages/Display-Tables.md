

Next: [Active Display Table](Active-Display-Table.html), Previous: [Usual Display](Usual-Display.html), Up: [Character Display](Character-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.22.2 Display Tables

A display table is a special-purpose char-table (see [Char-Tables](Char_002dTables.html)), with `display-table` as its subtype, which is used to override the usual character display conventions. This section describes how to make, inspect, and assign elements to a display table object.

*   Function: **make-display-table**

    This creates and returns a display table. The table initially has `nil` in all elements.

The ordinary elements of the display table are indexed by character codes; the element at index `c` says how to display the character code `c`. The value should be `nil` (which means to display the character `c` according to the usual display conventions; see [Usual Display](Usual-Display.html)), or a vector of glyph codes (which means to display the character `c` as those glyphs; see [Glyphs](Glyphs.html)).

**Warning:** if you use the display table to change the display of newline characters, the whole buffer will be displayed as one long line.

The display table also has six *extra slots* which serve special purposes. Here is a table of their meanings; `nil` in any slot means to use the default for that slot, as stated below.

*   0

    The glyph for the end of a truncated screen line (the default for this is ‘`$`’). See [Glyphs](Glyphs.html). On graphical terminals, Emacs by default uses arrows in the fringes to indicate truncation, so the display table has no effect, unless you disable the fringes (see [Window Fringes](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fringes.html#Fringes) in the GNU Emacs Manual).

*   1

    The glyph for the end of a continued line (the default is ‘`\`’). On graphical terminals, Emacs by default uses curved arrows in the fringes to indicate continuation, so the display table has no effect, unless you disable the fringes.

*   2

    The glyph for indicating a character displayed as an octal character code (the default is ‘`\`’).

*   3

    The glyph for indicating a control character (the default is ‘`^`’).

*   4

    A vector of glyphs for indicating the presence of invisible lines (the default is ‘`...`’). See [Selective Display](Selective-Display.html).

*   5

    The glyph used to draw the border between side-by-side windows (the default is ‘`|`’). See [Splitting Windows](Splitting-Windows.html). This currently has effect only on text terminals; on graphical terminals, if vertical scroll bars are supported and in use, a scroll bar separates the two windows, and if there are no vertical scroll bars and no dividers (see [Window Dividers](Window-Dividers.html)), Emacs uses a thin line to indicate the border.

For example, here is how to construct a display table that mimics the effect of setting `ctl-arrow` to a non-`nil` value (see [Glyphs](Glyphs.html), for the function `make-glyph-code`):

```lisp
(setq disptab (make-display-table))
(dotimes (i 32)
  (or (= i ?\t)
      (= i ?\n)
      (aset disptab i
            (vector (make-glyph-code ?^ 'escape-glyph)
                    (make-glyph-code (+ i 64) 'escape-glyph)))))
(aset disptab 127
      (vector (make-glyph-code ?^ 'escape-glyph)
              (make-glyph-code ?? 'escape-glyph)))))
```

*   Function: **display-table-slot** *display-table slot*

    This function returns the value of the extra slot `slot` of `display-table`. The argument `slot` may be a number from 0 to 5 inclusive, or a slot name (symbol). Valid symbols are `truncation`, `wrap`, `escape`, `control`, `selective-display`, and `vertical-border`.

<!---->

*   Function: **set-display-table-slot** *display-table slot value*

    This function stores `value` in the extra slot `slot` of `display-table`. The argument `slot` may be a number from 0 to 5 inclusive, or a slot name (symbol). Valid symbols are `truncation`, `wrap`, `escape`, `control`, `selective-display`, and `vertical-border`.

<!---->

*   Function: **describe-display-table** *display-table*

    This function displays a description of the display table `display-table` in a help buffer.

<!---->

*   Command: **describe-current-display-table**

    This command displays a description of the current display table in a help buffer.

Next: [Active Display Table](Active-Display-Table.html), Previous: [Usual Display](Usual-Display.html), Up: [Character Display](Character-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
