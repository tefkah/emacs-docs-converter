

Next: [Mouse Dragging Parameters](Mouse-Dragging-Parameters.html), Previous: [Buffer Parameters](Buffer-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.4.3.6 Frame Interaction Parameters

These parameters supply forms of interactions between different frames.

*   `parent-frame`

    If non-`nil`, this means that this frame is a child frame (see [Child Frames](Child-Frames.html)), and this parameter specifies its parent frame. If `nil`, this means that this frame is a normal, top-level frame.

*   `delete-before`

    If non-`nil`, this parameter specifies another frame whose deletion will automatically trigger the deletion of this frame. See [Deleting Frames](Deleting-Frames.html).

*   `mouse-wheel-frame`

    If non-`nil`, this parameter specifies the frame whose windows will be scrolled whenever the mouse wheel is scrolled with the mouse pointer hovering over this frame, see [Mouse Commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Mouse-Commands.html#Mouse-Commands) in The GNU Emacs Manual.

*   `no-other-frame`

    If this is non-`nil`, then this frame is not eligible as candidate for the functions `next-frame`, `previous-frame` (see [Finding All Frames](Finding-All-Frames.html)) and `other-frame`, see [Frame Commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Commands.html#Frame-Commands) in The GNU Emacs Manual.

*   `auto-hide-function`

    When this parameter specifies a function, that function will be called instead of the function specified by the variable `frame-auto-hide-function` when quitting the frame’s only window (see [Quitting Windows](Quitting-Windows.html)) and there are other frames left.

*   `minibuffer-exit`

    When this parameter is non-`nil`, Emacs will by default make this frame invisible whenever the minibuffer (see [Minibuffers](Minibuffers.html)) is exited. Alternatively, it can specify the functions `iconify-frame` and `delete-frame`. This parameter is useful to make a child frame disappear automatically (similar to how Emacs deals with a window) when exiting the minibuffer.

*   `keep-ratio`

    This parameter is currently meaningful for child frames (see [Child Frames](Child-Frames.html)) only. If it is non-`nil`, then Emacs will try to keep the frame’s size (width and height) ratios (see [Size Parameters](Size-Parameters.html)) as well as its left and right position ratios (see [Position Parameters](Position-Parameters.html)) unaltered whenever its parent frame is resized.

    If the value of this parameter is `nil`, the frame’s position and size remain unaltered when the parent frame is resized, so the position and size ratios may change. If the value of this parameter is `t`, Emacs will try to preserve the frame’s size and position ratios, hence the frame’s size and position relative to its parent frame may change.

    More individual control is possible by using a cons cell: In that case the frame’s width ratio is preserved if the CAR of the cell is either `t` or `width-only`. The height ratio is preserved if the CAR of the cell is either `t` or `height-only`. The left position ratio is preserved if the CDR of the cell is either `t` or `left-only`. The top position ratio is preserved if the CDR of the cell is either `t` or `top-only`.

Next: [Mouse Dragging Parameters](Mouse-Dragging-Parameters.html), Previous: [Buffer Parameters](Buffer-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
