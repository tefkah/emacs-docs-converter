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

Next: [Displaying Faces](Displaying-Faces.html), Previous: [Defining Faces](Defining-Faces.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.12.3 Face Attribute Functions

This section describes functions for directly accessing and modifying the attributes of a named face.

*   Function: **face-attribute** *face attribute \&optional frame inherit*

    This function returns the value of the `attribute` attribute for `face` on `frame`.

    If `frame` is omitted or `nil`, that means the selected frame (see [Input Focus](Input-Focus.html)). If `frame` is `t`, this function returns the value of the specified attribute for newly-created frames (this is normally `unspecified`, unless you have specified some value using `set-face-attribute`; see below).

    If `inherit` is `nil`, only attributes directly defined by `face` are considered, so the return value may be `unspecified`, or a relative value. If `inherit` is non-`nil`, `face`’s definition of `attribute` is merged with the faces specified by its `:inherit` attribute; however the return value may still be `unspecified` or relative. If `inherit` is a face or a list of faces, then the result is further merged with that face (or faces), until it becomes specified and absolute.

    To ensure that the return value is always specified and absolute, use a value of `default` for `inherit`; this will resolve any unspecified or relative values by merging with the `default` face (which is always completely specified).

    For example,

        (face-attribute 'bold :weight)
             ⇒ bold

<!---->

*   Function: **face-attribute-relative-p** *attribute value*

    This function returns non-`nil` if `value`, when used as the value of the face attribute `attribute`, is relative. This means it would modify, rather than completely override, any value that comes from a subsequent face in the face list or that is inherited from another face.

    `unspecified` is a relative value for all attributes. For `:height`, floating point and function values are also relative.

    For example:

        (face-attribute-relative-p :height 2.0)
             ⇒ t

<!---->

*   Function: **face-all-attributes** *face \&optional frame*

    This function returns an alist of attributes of `face`. The elements of the result are name-value pairs of the form `(attr-name . attr-value)`<!-- /@w -->. Optional argument `frame` specifies the frame whose definition of `face` to return; if omitted or `nil`, the returned value describes the default attributes of `face` for newly created frames.

<!---->

*   Function: **merge-face-attribute** *attribute value1 value2*

    If `value1` is a relative value for the face attribute `attribute`, returns it merged with the underlying value `value2`; otherwise, if `value1` is an absolute value for the face attribute `attribute`, returns `value1` unchanged.

Normally, Emacs uses the face specs of each face to automatically calculate its attributes on each frame (see [Defining Faces](Defining-Faces.html)). The function `set-face-attribute` can override this calculation by directly assigning attributes to a face, either on a specific frame or for all frames. This function is mostly intended for internal usage.

*   Function: **set-face-attribute** *face frame \&rest arguments*

    This function sets one or more attributes of `face` for `frame`. The attributes specifies in this way override the face spec(s) belonging to `face`.

    The extra arguments `arguments` specify the attributes to set, and the values for them. They should consist of alternating attribute names (such as `:family` or `:underline`) and values. Thus,

        (set-face-attribute 'foo nil :weight 'bold :slant 'italic)

    sets the attribute `:weight` to `bold` and the attribute `:slant` to `italic`.

    If `frame` is `t`, this function sets the default attributes for newly created frames. If `frame` is `nil`, this function sets the attributes for all existing frames, as well as for newly created frames.

The following commands and functions mostly provide compatibility with old versions of Emacs. They work by calling `set-face-attribute`. Values of `t` and `nil` (or omitted) for their `frame` argument are handled just like `set-face-attribute` and `face-attribute`. The commands read their arguments using the minibuffer, if called interactively.

*   *   Command: **set-face-foreground** *face color \&optional frame*
    *   Command: **set-face-background** *face color \&optional frame*

    These set the `:foreground` attribute (or `:background` attribute, respectively) of `face` to `color`.

<!---->

*   Command: **set-face-stipple** *face pattern \&optional frame*

    This sets the `:stipple` attribute of `face` to `pattern`.

<!---->

*   Command: **set-face-font** *face font \&optional frame*

    Change the font-related attributes of `face` to those of `font` (a string or a font object). See [face-font-attribute](Face-Attributes.html#face_002dfont_002dattribute), for the supported formats of the `font` argument. This function sets the attribute `:font` of the face, and indirectly also the `:family`, `:foundry`, `:width`, `:height`, `:weight`, and `:slant` attributes, as defined by the font. If `frame` is non-`nil`, only change the attributes on the specified frame.

<!---->

*   Function: **set-face-bold** *face bold-p \&optional frame*

    This sets the `:weight` attribute of `face` to `normal` if `bold-p` is `nil`, and to `bold` otherwise.

<!---->

*   Function: **set-face-italic** *face italic-p \&optional frame*

    This sets the `:slant` attribute of `face` to `normal` if `italic-p` is `nil`, and to `italic` otherwise.

<!---->

*   Command: **set-face-underline** *face underline \&optional frame*

    This sets the `:underline` attribute of `face` to `underline`.

<!---->

*   Command: **set-face-inverse-video** *face inverse-video-p \&optional frame*

    This sets the `:inverse-video` attribute of `face` to `inverse-video-p`.

<!---->

*   Command: **invert-face** *face \&optional frame*

    This swaps the foreground and background colors of face `face`.

<!---->

*   Command: **set-face-extend** *face extend \&optional frame*

    This sets the `:extend` attribute of `face` to `extend`.

The following functions examine the attributes of a face. They mostly provide compatibility with old versions of Emacs. If you don’t specify `frame`, they refer to the selected frame; `t` refers to the default data for new frames. They return `unspecified` if the face doesn’t define any value for that attribute. If `inherit` is `nil`, only an attribute directly defined by the face is returned. If `inherit` is non-`nil`, any faces specified by its `:inherit` attribute are considered as well, and if `inherit` is a face or a list of faces, then they are also considered, until a specified attribute is found. To ensure that the return value is always specified, use a value of `default` for `inherit`.

*   Function: **face-font** *face \&optional frame character*

    This function returns the name of the font of face `face`.

    If the optional argument `frame` is specified, it returns the name of the font of `face` for that frame. If `frame` is omitted or `nil`, the selected frame is used. In the latter case, if the optional third argument `character` is supplied, it returns the font name used for `character`.

<!---->

*   *   Function: **face-foreground** *face \&optional frame inherit*
    *   Function: **face-background** *face \&optional frame inherit*

    These functions return the foreground color (or background color, respectively) of face `face`, as a string. If the color is unspecified, they return `nil`.

<!---->

*   Function: **face-stipple** *face \&optional frame inherit*

    This function returns the name of the background stipple pattern of face `face`, or `nil` if it doesn’t have one.

<!---->

*   Function: **face-bold-p** *face \&optional frame inherit*

    This function returns a non-`nil` value if the `:weight` attribute of `face` is bolder than normal (i.e., one of `semi-bold`, `bold`, `extra-bold`, or `ultra-bold`). Otherwise, it returns `nil`.

<!---->

*   Function: **face-italic-p** *face \&optional frame inherit*

    This function returns a non-`nil` value if the `:slant` attribute of `face` is `italic` or `oblique`, and `nil` otherwise.

<!---->

*   Function: **face-underline-p** *face \&optional frame inherit*

    This function returns non-`nil` if face `face` specifies a non-`nil` `:underline` attribute.

<!---->

*   Function: **face-inverse-video-p** *face \&optional frame inherit*

    This function returns non-`nil` if face `face` specifies a non-`nil` `:inverse-video` attribute.

<!---->

*   Function: **face-extend-p** *face \&optional frame*

    This function returns non-`nil` if face `face` specifies a non-`nil` `:extend` attribute.

Next: [Displaying Faces](Displaying-Faces.html), Previous: [Defining Faces](Defining-Faces.html), Up: [Faces](Faces.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
