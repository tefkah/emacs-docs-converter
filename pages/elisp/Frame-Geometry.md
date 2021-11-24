

### 29.3 Frame Geometry

The geometry of a frame depends on the toolkit that was used to build this instance of Emacs and the terminal that displays the frame. This chapter describes these dependencies and some of the functions to deal with them. Note that the `frame` argument of all of these functions has to specify a live frame (see [Deleting Frames](Deleting-Frames.html)). If omitted or `nil`, it specifies the selected frame (see [Input Focus](Input-Focus.html)).

|                                                         |    |                                                   |
| :------------------------------------------------------ | -- | :------------------------------------------------ |
| • [Frame Layout](Frame-Layout.html)                     |    | Basic layout of frames.                           |
| • [Frame Font](Frame-Font.html)                         |    | The default font of a frame and how to set it.    |
| • [Frame Position](Frame-Position.html)                 |    | The position of a frame on its display.           |
| • [Frame Size](Frame-Size.html)                         |    | Specifying and retrieving a frame’s size.         |
| • [Implied Frame Resizing](Implied-Frame-Resizing.html) |    | Implied resizing of frames and how to prevent it. |
