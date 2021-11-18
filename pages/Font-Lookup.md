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

Next: [Fontsets](Fontsets.html), Previous: [Font Selection](Font-Selection.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.12.10 Looking Up Fonts

*   Function: **x-list-fonts** *name \&optional reference-face frame maximum width*

    This function returns a list of available font names that match `name`. `name` should be a string containing a font name in either the Fontconfig, GTK+, or XLFD format (see [Fonts](https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html#Fonts) in The GNU Emacs Manual). Within an XLFD string, wildcard characters may be used: the ‘`*`’ character matches any substring, and the ‘`?`’ character matches any single character. Case is ignored when matching font names.

    If the optional arguments `reference-face` and `frame` are specified, the returned list includes only fonts that are the same size as `reference-face` (a face name) currently is on the frame `frame`.

    The optional argument `maximum` sets a limit on how many fonts to return. If it is non-`nil`, then the return value is truncated after the first `maximum` matching fonts. Specifying a small value for `maximum` can make this function much faster, in cases where many fonts match the pattern.

    The optional argument `width` specifies a desired font width. If it is non-`nil`, the function only returns those fonts whose characters are (on average) `width` times as wide as `reference-face`.

<!---->

*   Function: **x-family-fonts** *\&optional family frame*

    This function returns a list describing the available fonts for family `family` on `frame`. If `family` is omitted or `nil`, this list applies to all families, and therefore, it contains all available fonts. Otherwise, `family` must be a string; it may contain the wildcards ‘`?`’ and ‘`*`’.

    The list describes the display that `frame` is on; if `frame` is omitted or `nil`, it applies to the selected frame’s display (see [Input Focus](Input-Focus.html)).

    Each element in the list is a vector of the following form:

        [family width point-size weight slant
         fixed-p full registry-and-encoding]

    The first five elements correspond to face attributes; if you specify these attributes for a face, it will use this font.

    The last three elements give additional information about the font. `fixed-p` is non-`nil` if the font is fixed-pitch. `full` is the full name of the font, and `registry-and-encoding` is a string giving the registry and encoding of the font.

Next: [Fontsets](Fontsets.html), Previous: [Font Selection](Font-Selection.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
