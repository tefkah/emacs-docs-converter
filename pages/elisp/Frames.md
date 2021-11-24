

## 29 Frames

A *frame* is a screen object that contains one or more Emacs windows (see [Windows](Windows.html)). It is the kind of object called a “window” in the terminology of graphical environments; but we can’t call it a “window” here, because Emacs uses that word in a different way. In Emacs Lisp, a *frame object* is a Lisp object that represents a frame on the screen. See [Frame Type](Frame-Type.html).

A frame initially contains a single main window and/or a minibuffer window; you can subdivide the main window vertically or horizontally into smaller windows. See [Splitting Windows](Splitting-Windows.html).

A *terminal* is a display device capable of displaying one or more Emacs frames. In Emacs Lisp, a *terminal object* is a Lisp object that represents a terminal. See [Terminal Type](Terminal-Type.html).

There are two classes of terminals: *text terminals* and *graphical terminals*. Text terminals are non-graphics-capable displays, including `xterm` and other terminal emulators. On a text terminal, each Emacs frame occupies the terminal’s entire screen; although you can create additional frames and switch between them, the terminal only shows one frame at a time. Graphical terminals, on the other hand, are managed by graphical display systems such as the X Window System, which allow Emacs to show multiple frames simultaneously on the same display.

On GNU and Unix systems, you can create additional frames on any available terminal, within a single Emacs session, regardless of whether Emacs was started on a text or graphical terminal. Emacs can display on both graphical and text terminals simultaneously. This comes in handy, for instance, when you connect to the same session from several remote locations. See [Multiple Terminals](Multiple-Terminals.html).

### Function: **framep** *object*

This predicate returns a non-`nil` value if `object` is a frame, and `nil` otherwise. For a frame, the value indicates which kind of display the frame uses:

*   `t`

    The frame is displayed on a text terminal.

*   `x`

    The frame is displayed on an X graphical terminal.

*   `w32`

    The frame is displayed on a MS-Windows graphical terminal.

*   `ns`

    The frame is displayed on a GNUstep or Macintosh Cocoa graphical terminal.

*   `pc`

    The frame is displayed on an MS-DOS terminal.

### Function: **frame-terminal** *\&optional frame*

This function returns the terminal object that displays `frame`. If `frame` is `nil` or unspecified, it defaults to the selected frame.

### Function: **terminal-live-p** *object*

This predicate returns a non-`nil` value if `object` is a terminal that is live (i.e., not deleted), and `nil` otherwise. For live terminals, the return value indicates what kind of frames are displayed on that terminal; the list of possible values is the same as for `framep` above.

On a graphical terminal we distinguish two types of frames: A normal *top-level frame* is a frame whose window-system window is a child of the window-system’s root window for that terminal. A child frame is a frame whose window-system window is the child of the window-system window of another Emacs frame. See [Child Frames](Child-Frames.html).

|                                                             |    |                                                |
| :---------------------------------------------------------- | -- | :--------------------------------------------- |
| • [Creating Frames](Creating-Frames.html)                   |    | Creating additional frames.                    |
| • [Multiple Terminals](Multiple-Terminals.html)             |    | Displaying on several different devices.       |
| • [Frame Geometry](Frame-Geometry.html)                     |    | Geometric properties of frames.                |
| • [Frame Parameters](Frame-Parameters.html)                 |    | Controlling frame size, position, font, etc.   |
| • [Terminal Parameters](Terminal-Parameters.html)           |    | Parameters common for all frames on terminal.  |
| • [Frame Titles](Frame-Titles.html)                         |    | Automatic updating of frame titles.            |
| • [Deleting Frames](Deleting-Frames.html)                   |    | Frames last until explicitly deleted.          |
| • [Finding All Frames](Finding-All-Frames.html)             |    | How to examine all existing frames.            |
| • [Minibuffers and Frames](Minibuffers-and-Frames.html)     |    | How a frame finds the minibuffer to use.       |
| • [Input Focus](Input-Focus.html)                           |    | Specifying the selected frame.                 |
| • [Visibility of Frames](Visibility-of-Frames.html)         |    | Frames may be visible or invisible, or icons.  |
| • [Raising and Lowering](Raising-and-Lowering.html)         |    | Raising, Lowering and Restacking Frames.       |
| • [Frame Configurations](Frame-Configurations.html)         |    | Saving the state of all frames.                |
| • [Child Frames](Child-Frames.html)                         |    | Making a frame the child of another.           |
| • [Mouse Tracking](Mouse-Tracking.html)                     |    | Getting events that say when the mouse moves.  |
| • [Mouse Position](Mouse-Position.html)                     |    | Asking where the mouse is, or moving it.       |
| • [Pop-Up Menus](Pop_002dUp-Menus.html)                     |    | Displaying a menu for the user to select from. |
| • [Dialog Boxes](Dialog-Boxes.html)                         |    | Displaying a box to ask yes or no.             |
| • [Pointer Shape](Pointer-Shape.html)                       |    | Specifying the shape of the mouse pointer.     |
| • [Window System Selections](Window-System-Selections.html) |    | Transferring text to and from other X clients. |
| • [Drag and Drop](Drag-and-Drop.html)                       |    | Internals of Drag-and-Drop implementation.     |
| • [Color Names](Color-Names.html)                           |    | Getting the definitions of color names.        |
| • [Text Terminal Colors](Text-Terminal-Colors.html)         |    | Defining colors for text terminals.            |
| • [Resources](Resources.html)                               |    | Getting resource values from the server.       |
| • [Display Feature Testing](Display-Feature-Testing.html)   |    | Determining the features of a terminal.        |
