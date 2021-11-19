

Next: [Circular Objects](Circular-Objects.html), Previous: [Programming Types](Programming-Types.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 2.5 Editing Types

The types in the previous section are used for general programming purposes, and most of them are common to most Lisp dialects. Emacs Lisp provides several additional data types for purposes connected with editing.

|                                                               |    |                                                     |
| :------------------------------------------------------------ | -- | :-------------------------------------------------- |
| • [Buffer Type](Buffer-Type.html)                             |    | The basic object of editing.                        |
| • [Marker Type](Marker-Type.html)                             |    | A position in a buffer.                             |
| • [Window Type](Window-Type.html)                             |    | Buffers are displayed in windows.                   |
| • [Frame Type](Frame-Type.html)                               |    | Windows subdivide frames.                           |
| • [Terminal Type](Terminal-Type.html)                         |    | A terminal device displays frames.                  |
| • [Window Configuration Type](Window-Configuration-Type.html) |    | Recording the way a frame is subdivided.            |
| • [Frame Configuration Type](Frame-Configuration-Type.html)   |    | Recording the status of all frames.                 |
| • [Process Type](Process-Type.html)                           |    | A subprocess of Emacs running on the underlying OS. |
| • [Thread Type](Thread-Type.html)                             |    | A thread of Emacs Lisp execution.                   |
| • [Mutex Type](Mutex-Type.html)                               |    | An exclusive lock for thread synchronization.       |
| • [Condition Variable Type](Condition-Variable-Type.html)     |    | Condition variable for thread synchronization.      |
| • [Stream Type](Stream-Type.html)                             |    | Receive or send characters.                         |
| • [Keymap Type](Keymap-Type.html)                             |    | What function a keystroke invokes.                  |
| • [Overlay Type](Overlay-Type.html)                           |    | How an overlay is represented.                      |
| • [Font Type](Font-Type.html)                                 |    | Fonts for displaying text.                          |
