

### 39.17 Images

To display an image in an Emacs buffer, you must first create an image descriptor, then use it as a display specifier in the `display` property of text that is displayed (see [Display Property](Display-Property.html)).

Emacs is usually able to display images when it is run on a graphical terminal. Images cannot be displayed in a text terminal, on certain graphical terminals that lack the support for this, or if Emacs is compiled without image support. You can use the function `display-images-p` to determine if images can in principle be displayed (see [Display Feature Testing](Display-Feature-Testing.html)).

|                                                     |    |                                                         |
| :-------------------------------------------------- | -- | :------------------------------------------------------ |
| • [Image Formats](Image-Formats.html)               |    | Supported image formats.                                |
| • [Image Descriptors](Image-Descriptors.html)       |    | How to specify an image for use in `:display`.          |
| • [XBM Images](XBM-Images.html)                     |    | Special features for XBM format.                        |
| • [XPM Images](XPM-Images.html)                     |    | Special features for XPM format.                        |
| • [ImageMagick Images](ImageMagick-Images.html)     |    | Special features available through ImageMagick.         |
| • [SVG Images](SVG-Images.html)                     |    | Creating and manipulating SVG images.                   |
| • [Other Image Types](Other-Image-Types.html)       |    | Various other formats are supported.                    |
| • [Defining Images](Defining-Images.html)           |    | Convenient ways to define an image for later use.       |
| • [Showing Images](Showing-Images.html)             |    | Convenient ways to display an image once it is defined. |
| • [Multi-Frame Images](Multi_002dFrame-Images.html) |    | Some images contain more than one frame.                |
| • [Image Cache](Image-Cache.html)                   |    | Internal mechanisms of image display.                   |
