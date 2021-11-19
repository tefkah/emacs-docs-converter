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

Next: [Buttons](Buttons.html), Previous: [Images](Images.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 39.18 Embedded Native Widgets

Emacs is able to display native widgets, such as GTK+ WebKit widgets, in Emacs buffers when it was built with the necessary support libraries and is running on a graphical terminal. To test whether Emacs supports display of embedded widgets, check that the `xwidget-internal` feature is available (see [Named Features](Named-Features.html)).

To display an embedded widget in a buffer, you must first create an xwidget object, and then use that object as the display specifier in a `display` text or overlay property (see [Display Property](Display-Property.html)).

*   Function: **make-xwidget** *type title width height arguments \&optional buffer*

    This creates and returns an xwidget object. If `buffer` is omitted or `nil`, it defaults to the current buffer. If `buffer` names a buffer that doesn’t exist, it will be created. The `type` identifies the type of the xwidget component, it can be one of the following:

    *   `webkit`

        The WebKit component.

    The `width` and `height` arguments specify the widget size in pixels, and `title`, a string, specifies its title.

<!---->

*   Function: **xwidgetp** *object*

    This function returns `t` if `object` is an xwidget, `nil` otherwise.

<!---->

*   Function: **xwidget-plist** *xwidget*

    This function returns the property list of `xwidget`.

<!---->

*   Function: **set-xwidget-plist** *xwidget plist*

    This function replaces the property list of `xwidget` with a new property list given by `plist`.

<!---->

*   Function: **xwidget-buffer** *xwidget*

    This function returns the buffer of `xwidget`.

<!---->

*   Function: **get-buffer-xwidgets** *buffer*

    This function returns a list of xwidget objects associated with the `buffer`, which can be specified as a buffer object or a name of an existing buffer, a string. The value is `nil` if `buffer` contains no xwidgets.

<!---->

*   Function: **xwidget-webkit-goto-uri** *xwidget uri*

    This function browses the specified `uri` in the given `xwidget`. The `uri` is a string that specifies the name of a file or a URL.

<!---->

*   Function: **xwidget-webkit-execute-script** *xwidget script*

    This function causes the browser widget specified by `xwidget` to execute the specified JavaScript `script`.

<!---->

*   Function: **xwidget-webkit-execute-script-rv** *xwidget script \&optional default*

    This function executes the specified `script` like `xwidget-webkit-execute-script` does, but it also returns the script’s return value as a string. If `script` doesn’t return a value, this function returns `default`, or `nil` if `default` was omitted.

<!---->

*   Function: **xwidget-webkit-get-title** *xwidget*

    This function returns the title of `xwidget` as a string.

<!---->

*   Function: **xwidget-resize** *xwidget width height*

    This function resizes the specified `xwidget` to the size `width`x`height` pixels.

<!---->

*   Function: **xwidget-size-request** *xwidget*

    This function returns the desired size of `xwidget` as a list of the form `(width height)`. The dimensions are in pixels.

<!---->

*   Function: **xwidget-info** *xwidget*

    This function returns the attributes of `xwidget` as a vector of the form `[type title width height]`. The attributes are usually determined by `make-xwidget` when the xwidget is created.

<!---->

*   Function: **set-xwidget-query-on-exit-flag** *xwidget flag*

    This function allows you to arrange that Emacs will ask the user for confirmation before exiting or before killing a buffer that has `xwidget` associated with it. If `flag` is non-`nil`, Emacs will query the user, otherwise it will not.

<!---->

*   Function: **xwidget-query-on-exit-flag** *xwidget*

    This function returns the current setting of `xwidget`s query-on-exit flag, either `t` or `nil`.

Next: [Buttons](Buttons.html), Previous: [Images](Images.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
