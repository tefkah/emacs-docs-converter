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

Next: [Auto Faces](Auto-Faces.html), Previous: [Face Remapping](Face-Remapping.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.12.6 Functions for Working with Faces

Here are additional functions for creating and working with faces.

*   Function: **face-list**

    This function returns a list of all defined face names.

<!---->

*   Function: **face-id** *face*

    This function returns the *face number* of face `face`. This is a number that uniquely identifies a face at low levels within Emacs. It is seldom necessary to refer to a face by its face number. However, functions that manipulate glyphs, such as `make-glyph-code` and `glyph-face` (see [Glyphs](Glyphs.html)) access the face numbers internally. Note that the face number is stored as the value of the `face` property of the face symbol, so we recommend not to set that property of a face to any value of your own.

<!---->

*   Function: **face-documentation** *face*

    This function returns the documentation string of face `face`, or `nil` if none was specified for it.

<!---->

*   Function: **face-equal** *face1 face2 \&optional frame*

    This returns `t` if the faces `face1` and `face2` have the same attributes for display.

<!---->

*   Function: **face-differs-from-default-p** *face \&optional frame*

    This returns non-`nil` if the face `face` displays differently from the default face.

A *face alias* provides an equivalent name for a face. You can define a face alias by giving the alias symbol the `face-alias` property, with a value of the target face name. The following example makes `modeline` an alias for the `mode-line` face.

    (put 'modeline 'face-alias 'mode-line)

*   Macro: **define-obsolete-face-alias** *obsolete-face current-face when*

    This macro defines `obsolete-face` as an alias for `current-face`, and also marks it as obsolete, indicating that it may be removed in future. `when` should be a string indicating when `obsolete-face` was made obsolete (usually a version number string).

Next: [Auto Faces](Auto-Faces.html), Previous: [Face Remapping](Face-Remapping.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
