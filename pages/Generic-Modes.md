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

Next: [Example Major Modes](Example-Major-Modes.html), Previous: [Tabulated List Mode](Tabulated-List-Mode.html), Up: [Major Modes](Major-Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.2.8 Generic Modes

*Generic modes* are simple major modes with basic support for comment syntax and Font Lock mode. To define a generic mode, use the macro `define-generic-mode`. See the file `generic-x.el` for some examples of the use of `define-generic-mode`.

*   Macro: **define-generic-mode** *mode comment-list keyword-list font-lock-list auto-mode-list function-list \&optional docstring*

    This macro defines a generic mode command named `mode` (a symbol, not quoted). The optional argument `docstring` is the documentation for the mode command. If you do not supply it, `define-generic-mode` generates one by default.

    The argument `comment-list` is a list in which each element is either a character, a string of one or two characters, or a cons cell. A character or a string is set up in the mode’s syntax table as a comment starter. If the entry is a cons cell, the CAR is set up as a comment starter and the CDR as a comment ender. (Use `nil` for the latter if you want comments to end at the end of the line.) Note that the syntax table mechanism has limitations about what comment starters and enders are actually possible. See [Syntax Tables](Syntax-Tables.html).

    The argument `keyword-list` is a list of keywords to highlight with `font-lock-keyword-face`. Each keyword should be a string. Meanwhile, `font-lock-list` is a list of additional expressions to highlight. Each element of this list should have the same form as an element of `font-lock-keywords`. See [Search-based Fontification](Search_002dbased-Fontification.html).

    The argument `auto-mode-list` is a list of regular expressions to add to the variable `auto-mode-alist`. They are added by the execution of the `define-generic-mode` form, not by expanding the macro call.

    Finally, `function-list` is a list of functions for the mode command to call for additional setup. It calls these functions just before it runs the mode hook variable `mode-hook`.

Next: [Example Major Modes](Example-Major-Modes.html), Previous: [Tabulated List Mode](Tabulated-List-Mode.html), Up: [Major Modes](Major-Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]