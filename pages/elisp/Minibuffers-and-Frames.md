

### 29.9 Minibuffers and Frames

Normally, each frame has its own minibuffer window at the bottom, which is used whenever that frame is selected. You can get that window with the function `minibuffer-window` (see [Minibuffer Windows](Minibuffer-Windows.html)).

However, you can also create a frame without a minibuffer. Such a frame must use the minibuffer window of some other frame. That other frame will serve as *surrogate minibuffer frame* for this frame and cannot be deleted via `delete-frame` (see [Deleting Frames](Deleting-Frames.html)) as long as this frame is live.

When you create the frame, you can explicitly specify its minibuffer window (in some other frame) with the `minibuffer` frame parameter (see [Buffer Parameters](Buffer-Parameters.html)). If you don’t, then the minibuffer is found in the frame which is the value of the variable `default-minibuffer-frame`. Its value should be a frame that does have a minibuffer.

If you use a minibuffer-only frame, you might want that frame to raise when you enter the minibuffer. If so, set the variable `minibuffer-auto-raise` to `t`. See [Raising and Lowering](Raising-and-Lowering.html).

### Variable: **default-minibuffer-frame**

This variable specifies the frame to use for the minibuffer window, by default. It does not affect existing frames. It is always local to the current terminal and cannot be buffer-local. See [Multiple Terminals](Multiple-Terminals.html).
