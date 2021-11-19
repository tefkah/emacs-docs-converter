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

Previous: [Header Lines](Header-Lines.html), Up: [Mode Line Format](Mode-Line-Format.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.4.8 Emulating Mode Line Formatting

You can use the function `format-mode-line` to compute the text that would appear in a mode line or header line based on a certain mode line construct.

*   Function: **format-mode-line** *format \&optional face window buffer*

    This function formats a line of text according to `format` as if it were generating the mode line for `window`, but it also returns the text as a string. The argument `window` defaults to the selected window. If `buffer` is non-`nil`, all the information used is taken from `buffer`; by default, it comes from `window`’s buffer.

    The value string normally has text properties that correspond to the faces, keymaps, etc., that the mode line would have. Any character for which no `face` property is specified by `format` gets a default value determined by `face`. If `face` is `t`, that stands for either `mode-line` if `window` is selected, otherwise `mode-line-inactive`. If `face` is `nil` or omitted, that stands for the default face. If `face` is an integer, the value returned by this function will have no text properties.

    You can also specify other valid faces as the value of `face`. If specified, that face provides the `face` property for characters whose face is not specified by `format`.

    Note that using `mode-line`, `mode-line-inactive`, or `header-line` as `face` will actually redisplay the mode line or the header line, respectively, using the current definitions of the corresponding face, in addition to returning the formatted string. (Other faces do not cause redisplay.)

    For example, `(format-mode-line header-line-format)` returns the text that would appear in the selected window’s header line (`""` if it has no header line). `(format-mode-line header-line-format 'header-line)` returns the same text, with each character carrying the face that it will have in the header line itself, and also redraws the header line.

Previous: [Header Lines](Header-Lines.html), Up: [Mode Line Format](Mode-Line-Format.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
