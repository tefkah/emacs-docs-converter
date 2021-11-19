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

Next: [Header Lines](Header-Lines.html), Previous: [%-Constructs](_0025_002dConstructs.html), Up: [Mode Line Format](Mode-Line-Format.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.4.6 Properties in the Mode Line

Certain text properties are meaningful in the mode line. The `face` property affects the appearance of text; the `help-echo` property associates help strings with the text, and `keymap` can make the text mouse-sensitive.

There are four ways to specify text properties for text in the mode line:

1.  Put a string with a text property directly into the mode line data structure.
2.  Put a text property on a mode line %-construct such as ‘`%12b`’; then the expansion of the %-construct will have that same text property.
3.  Use a `(:propertize elt props…)` construct to give `elt` a text property specified by `props`.
4.  Use a list containing `:eval form` in the mode line data structure, and make `form` evaluate to a string that has a text property.

You can use the `keymap` property to specify a keymap. This keymap only takes real effect for mouse clicks; binding character keys and function keys to it has no effect, since it is impossible to move point into the mode line.

When the mode line refers to a variable which does not have a non-`nil` `risky-local-variable` property, any text properties given or specified within that variable’s values are ignored. This is because such properties could otherwise specify functions to be called, and those functions could come from file local variables.
