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

Next: [The Echo Area](The-Echo-Area.html), Previous: [Forcing Redisplay](Forcing-Redisplay.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 39.3 Truncation

When a line of text extends beyond the right edge of a window, Emacs can *continue* the line (make it wrap to the next screen line), or *truncate* the line (limit it to one screen line). The additional screen lines used to display a long text line are called *continuation* lines. Continuation is not the same as filling; continuation happens on the screen only, not in the buffer contents, and it breaks a line precisely at the right margin, not at a word boundary. See [Filling](Filling.html).

On a graphical display, tiny arrow images in the window fringes indicate truncated and continued lines (see [Fringes](Fringes.html)). On a text terminal, a ‘`$`’ in the rightmost column of the window indicates truncation; a ‘`\`’ on the rightmost column indicates a line that wraps. (The display table can specify alternate characters to use for this; see [Display Tables](Display-Tables.html)).

*   User Option: **truncate-lines**

    If this buffer-local variable is non-`nil`, lines that extend beyond the right edge of the window are truncated; otherwise, they are continued. As a special exception, the variable `truncate-partial-width-windows` takes precedence in *partial-width* windows (i.e., windows that do not occupy the entire frame width).

<!---->

*   User Option: **truncate-partial-width-windows**

    This variable controls line truncation in *partial-width* windows. A partial-width window is one that does not occupy the entire frame width (see [Splitting Windows](Splitting-Windows.html)). If the value is `nil`, line truncation is determined by the variable `truncate-lines` (see above). If the value is an integer `n`, lines are truncated if the partial-width window has fewer than `n` columns, regardless of the value of `truncate-lines`; if the partial-width window has `n` or more columns, line truncation is determined by `truncate-lines`. For any other non-`nil` value, lines are truncated in every partial-width window, regardless of the value of `truncate-lines`.

When horizontal scrolling (see [Horizontal Scrolling](Horizontal-Scrolling.html)) is in use in a window, that forces truncation.

*   Variable: **wrap-prefix**

    If this buffer-local variable is non-`nil`, it defines a *wrap prefix* which Emacs displays at the start of every continuation line. (If lines are truncated, `wrap-prefix` is never used.) Its value may be a string or an image (see [Other Display Specs](Other-Display-Specs.html)), or a stretch of whitespace such as specified by the `:width` or `:align-to` display properties (see [Specified Space](Specified-Space.html)). The value is interpreted in the same way as a `display` text property. See [Display Property](Display-Property.html).

    A wrap prefix may also be specified for regions of text, using the `wrap-prefix` text or overlay property. This takes precedence over the `wrap-prefix` variable. See [Special Properties](Special-Properties.html).

<!---->

*   Variable: **line-prefix**

    If this buffer-local variable is non-`nil`, it defines a *line prefix* which Emacs displays at the start of every non-continuation line. Its value may be a string or an image (see [Other Display Specs](Other-Display-Specs.html)), or a stretch of whitespace such as specified by the `:width` or `:align-to` display properties (see [Specified Space](Specified-Space.html)). The value is interpreted in the same way as a `display` text property. See [Display Property](Display-Property.html).

    A line prefix may also be specified for regions of text using the `line-prefix` text or overlay property. This takes precedence over the `line-prefix` variable. See [Special Properties](Special-Properties.html).

Next: [The Echo Area](The-Echo-Area.html), Previous: [Forcing Redisplay](Forcing-Redisplay.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
