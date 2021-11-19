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

Next: [Font Selection](Font-Selection.html), Previous: [Auto Faces](Auto-Faces.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.12.8 Basic Faces

If your Emacs Lisp program needs to assign some faces to text, it is often a good idea to use certain existing faces or inherit from them, rather than defining entirely new faces. This way, if other users have customized the basic faces to give Emacs a certain look, your program will fit in without additional customization.

Some of the basic faces defined in Emacs are listed below. In addition to these, you might want to make use of the Font Lock faces for syntactic highlighting, if highlighting is not already handled by Font Lock mode, or if some Font Lock faces are not in use. See [Faces for Font Lock](Faces-for-Font-Lock.html).

*   `default`

    The default face, whose attributes are all specified. All other faces implicitly inherit from it: any unspecified attribute defaults to the attribute on this face (see [Face Attributes](Face-Attributes.html)).

*   *   `bold`
    *   `italic`
    *   `bold-italic`
    *   `underline`
    *   `fixed-pitch`
    *   `fixed-pitch-serif`
    *   `variable-pitch`

    These have the attributes indicated by their names (e.g., `bold` has a bold `:weight` attribute), with all other attributes unspecified (and so given by `default`).

*   `shadow`

    For dimmed-out text. For example, it is used for the ignored part of a filename in the minibuffer (see [Minibuffers for File Names](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-File.html#Minibuffer-File) in The GNU Emacs Manual).

*   *   `link`
    *   `link-visited`

    For clickable text buttons that send the user to a different buffer or location.

*   `highlight`

    For stretches of text that should temporarily stand out. For example, it is commonly assigned to the `mouse-face` property for cursor highlighting (see [Special Properties](Special-Properties.html)).

*   *   `match`
    *   `isearch`
    *   `lazy-highlight`

    For text matching (respectively) permanent search matches, interactive search matches, and lazy highlighting other matches than the current interactive one.

*   *   `error`
    *   `warning`
    *   `success`

    For text concerning errors, warnings, or successes. For example, these are used for messages in `*Compilation*` buffers.

Next: [Font Selection](Font-Selection.html), Previous: [Auto Faces](Auto-Faces.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
