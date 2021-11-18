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

Next: [Glyphless Chars](Glyphless-Chars.html), Previous: [Active Display Table](Active-Display-Table.html), Up: [Character Display](Character-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.22.4 Glyphs

A *glyph* is a graphical symbol which occupies a single character position on the screen. Each glyph is represented in Lisp as a *glyph code*, which specifies a character and optionally a face to display it in (see [Faces](Faces.html)). The main use of glyph codes is as the entries of display tables (see [Display Tables](Display-Tables.html)). The following functions are used to manipulate glyph codes:

*   Function: **make-glyph-code** *char \&optional face*

    This function returns a glyph code representing char `char` with face `face`. If `face` is omitted or `nil`, the glyph uses the default face; in that case, the glyph code is an integer. If `face` is non-`nil`, the glyph code is not necessarily an integer object.

<!---->

*   Function: **glyph-char** *glyph*

    This function returns the character of glyph code `glyph`.

<!---->

*   Function: **glyph-face** *glyph*

    This function returns face of glyph code `glyph`, or `nil` if `glyph` uses the default face.

You can set up a *glyph table* to change how glyph codes are actually displayed on text terminals. This feature is semi-obsolete; use `glyphless-char-display` instead (see [Glyphless Chars](Glyphless-Chars.html)).

*   Variable: **glyph-table**

    The value of this variable, if non-`nil`, is the current glyph table. It takes effect only on character terminals; on graphical displays, all glyphs are displayed literally. The glyph table should be a vector whose `g`th element specifies how to display glyph code `g`, where `g` is the glyph code for a glyph whose face is unspecified. Each element should be one of the following:

    *   `nil`

        Display this glyph literally.

    *   a string

        Display this glyph by sending the specified string to the terminal.

    *   a glyph code

        Display the specified glyph code instead.

    Any integer glyph code greater than or equal to the length of the glyph table is displayed literally.

Next: [Glyphless Chars](Glyphless-Chars.html), Previous: [Active Display Table](Active-Display-Table.html), Up: [Character Display](Character-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
