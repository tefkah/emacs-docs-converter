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

Next: [Face Remapping](Face-Remapping.html), Previous: [Attribute Functions](Attribute-Functions.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.12.4 Displaying Faces

When Emacs displays a given piece of text, the visual appearance of the text may be determined by faces drawn from different sources. If these various sources together specify more than one face for a particular character, Emacs merges the attributes of the various faces. Here is the order in which Emacs merges the faces, from highest to lowest priority:

*   If the text consists of a special glyph, the glyph can specify a particular face. See [Glyphs](Glyphs.html).

*   If the text lies within an active region, Emacs highlights it using the `region` face. See [Standard Faces](https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html#Standard-Faces) in The GNU Emacs Manual.

*   If the text lies within an overlay with a non-`nil` `face` property, Emacs applies the face(s) specified by that property. If the overlay has a `mouse-face` property and the mouse is near enough to the overlay, Emacs applies the face or face attributes specified by the `mouse-face` property instead. See [Overlay Properties](Overlay-Properties.html).

    When multiple overlays cover one character, an overlay with higher priority overrides those with lower priority. See [Overlays](Overlays.html).

*   If the text contains a `face` or `mouse-face` property, Emacs applies the specified faces and face attributes. See [Special Properties](Special-Properties.html). (This is how Font Lock mode faces are applied. See [Font Lock Mode](Font-Lock-Mode.html).)

*   If the text lies within the mode line of the selected window, Emacs applies the `mode-line` face. For the mode line of a non-selected window, Emacs applies the `mode-line-inactive` face. For a header line, Emacs applies the `header-line` face. For a tab line, Emacs applies the `tab-line` face.

*   If the text comes from an overlay string via `before-string` or `after-string` properties (see [Overlay Properties](Overlay-Properties.html)), or from a display string (see [Other Display Specs](Other-Display-Specs.html)), and the string doesn’t contain a `face` or `mouse-face` property, or these properties leave some face attributes undefined, but the buffer text affected by the overlay/display property does define a face or those attributes, Emacs applies the face attributes of the “underlying” buffer text. Note that this is so even if the overlay or display string is displayed in the display margins (see [Display Margins](Display-Margins.html)).

*   If any given attribute has not been specified during the preceding steps, Emacs applies the attribute of the `default` face.

At each stage, if a face has a valid `:inherit` attribute, Emacs treats any attribute with an `unspecified` value as having the corresponding value drawn from the parent face(s). see [Face Attributes](Face-Attributes.html). Note that the parent face(s) may also leave the attribute unspecified; in that case, the attribute remains unspecified at the next level of face merging.

Next: [Face Remapping](Face-Remapping.html), Previous: [Attribute Functions](Attribute-Functions.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
