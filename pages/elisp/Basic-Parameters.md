

#### 29.4.3.1 Basic Parameters

These frame parameters give the most basic information about the frame. `title` and `name` are meaningful on all terminals.

`display`

The display on which to open this frame. It should be a string of the form ‘`host:dpy.screen`’, just like the `DISPLAY` environment variable. See [Multiple Terminals](Multiple-Terminals.html), for more details about display names.

`display-type`

This parameter describes the range of possible colors that can be used in this frame. Its value is `color`, `grayscale` or `mono`.

`title`

If a frame has a non-`nil` title, it appears in the window system’s title bar at the top of the frame, and also in the mode line of windows in that frame if `mode-line-frame-identification` uses ‘`%F`’ (see [%-Constructs](_0025_002dConstructs.html)). This is normally the case when Emacs is not using a window system, and can only display one frame at a time. See [Frame Titles](Frame-Titles.html).

`name`

The name of the frame. The frame name serves as a default for the frame title, if the `title` parameter is unspecified or `nil`. If you don’t specify a name, Emacs sets the frame name automatically (see [Frame Titles](Frame-Titles.html)).

If you specify the frame name explicitly when you create the frame, the name is also used (instead of the name of the Emacs executable) when looking up X resources for the frame.

`explicit-name`

If the frame name was specified explicitly when the frame was created, this parameter will be that name. If the frame wasn’t explicitly named, this parameter will be `nil`.
