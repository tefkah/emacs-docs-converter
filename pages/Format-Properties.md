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

Next: [Sticky Properties](Sticky-Properties.html), Previous: [Special Properties](Special-Properties.html), Up: [Text Properties](Text-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.19.5 Formatted Text Properties

These text properties affect the behavior of the fill commands. They are used for representing formatted text. See [Filling](Filling.html), and [Margins](Margins.html).

*   `hard`

    If a newline character has this property, it is a “hard” newline. The fill commands do not alter hard newlines and do not move words across them. However, this property takes effect only if the `use-hard-newlines` minor mode is enabled. See [Hard and Soft Newlines](https://www.gnu.org/software/emacs/manual/html_node/emacs/Hard-and-Soft-Newlines.html#Hard-and-Soft-Newlines) in The GNU Emacs Manual.

*   `right-margin`

    This property specifies an extra right margin for filling this part of the text.

*   `left-margin`

    This property specifies an extra left margin for filling this part of the text.

*   `justification`

    This property specifies the style of justification for filling this part of the text.
