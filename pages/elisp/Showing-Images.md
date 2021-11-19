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

Next: [Multi-Frame Images](Multi_002dFrame-Images.html), Previous: [Defining Images](Defining-Images.html), Up: [Images](Images.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.17.9 Showing Images

You can use an image descriptor by setting up the `display` property yourself, but it is easier to use the functions in this section.

*   Function: **insert-image** *image \&optional string area slice*

    This function inserts `image` in the current buffer at point. The value `image` should be an image descriptor; it could be a value returned by `create-image`, or the value of a symbol defined with `defimage`. The argument `string` specifies the text to put in the buffer to hold the image. If it is omitted or `nil`, `insert-image` uses `" "` by default.

    The argument `area` specifies whether to put the image in a margin. If it is `left-margin`, the image appears in the left margin; `right-margin` specifies the right margin. If `area` is `nil` or omitted, the image is displayed at point within the buffer’s text.

    The argument `slice` specifies a slice of the image to insert. If `slice` is `nil` or omitted the whole image is inserted. Otherwise, `slice` is a list `(x y width height)` which specifies the `x` and `y` positions and `width` and `height` of the image area to insert. Integer values are in units of pixels. A floating-point number in the range 0.0–1.0 stands for that fraction of the width or height of the entire image.

    Internally, this function inserts `string` in the buffer, and gives it a `display` property which specifies `image`. See [Display Property](Display-Property.html).

<!---->

*   Function: **insert-sliced-image** *image \&optional string area rows cols*

    This function inserts `image` in the current buffer at point, like `insert-image`, but splits the image into `rows`x`cols` equally sized slices.

    Emacs displays each slice as a separate image, and allows more intuitive scrolling up/down, instead of jumping up/down the entire image when paging through a buffer that displays (large) images.

<!---->

*   Function: **put-image** *image pos \&optional string area*

    This function puts image `image` in front of `pos` in the current buffer. The argument `pos` should be an integer or a marker. It specifies the buffer position where the image should appear. The argument `string` specifies the text that should hold the image as an alternative to the default.

    The argument `image` must be an image descriptor, perhaps returned by `create-image` or stored by `defimage`.

    The argument `area` specifies whether to put the image in a margin. If it is `left-margin`, the image appears in the left margin; `right-margin` specifies the right margin. If `area` is `nil` or omitted, the image is displayed at point within the buffer’s text.

    Internally, this function creates an overlay, and gives it a `before-string` property containing text that has a `display` property whose value is the image. (Whew!)

<!---->

*   Function: **remove-images** *start end \&optional buffer*

    This function removes images in `buffer` between positions `start` and `end`. If `buffer` is omitted or `nil`, images are removed from the current buffer.

    This removes only images that were put into `buffer` the way `put-image` does it, not images that were inserted with `insert-image` or in other ways.

<!---->

*   Function: **image-size** *spec \&optional pixels frame*

    This function returns the size of an image as a pair `(width . height)`<!-- /@w -->. `spec` is an image specification. `pixels` non-`nil` means return sizes measured in pixels, otherwise return sizes measured in the default character size of `frame` (see [Frame Font](Frame-Font.html)). `frame` is the frame on which the image will be displayed. `frame` `nil` or omitted means use the selected frame (see [Input Focus](Input-Focus.html)).

<!---->

*   Variable: **max-image-size**

    This variable is used to define the maximum size of image that Emacs will load. Emacs will refuse to load (and display) any image that is larger than this limit.

    If the value is an integer, it directly specifies the maximum image height and width, measured in pixels. If it is floating point, it specifies the maximum image height and width as a ratio to the frame height and width. If the value is non-numeric, there is no explicit limit on the size of images.

    The purpose of this variable is to prevent unreasonably large images from accidentally being loaded into Emacs. It only takes effect the first time an image is loaded. Once an image is placed in the image cache, it can always be displayed, even if the value of `max-image-size` is subsequently changed (see [Image Cache](Image-Cache.html)).

Images inserted with the insertion functions above also get a local keymap installed in the text properties (or overlays) that span the displayed image. This keymap defines the following commands:

*   `+`

    Increase the image size (`image-increase-size`). A prefix value of ‘`4`’ means to increase the size by 40%. The default is 20%.

*   `-`

    Decrease the image size (`image-increase-size`). A prefix value of ‘`4`’ means to decrease the size by 40%. The default is 20%.

*   `r`

    Rotate the image by 90 degrees clockwise (`image-rotate`). A prefix means to rotate by 90 degrees counter-clockwise instead.

*   `o`

    Save the image to a file (`image-save`).

Next: [Multi-Frame Images](Multi_002dFrame-Images.html), Previous: [Defining Images](Defining-Images.html), Up: [Images](Images.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
