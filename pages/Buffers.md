

Next: [Windows](Windows.html), Previous: [Backups and Auto-Saving](Backups-and-Auto_002dSaving.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 27 Buffers

A *buffer* is a Lisp object containing text to be edited. Buffers are used to hold the contents of files that are being visited; there may also be buffers that are not visiting files. While several buffers may exist at one time, only one buffer is designated the *current buffer* at any time. Most editing commands act on the contents of the current buffer. Each buffer, including the current buffer, may or may not be displayed in any windows.

|                                                   |    |                                                                              |
| :------------------------------------------------ | -- | :--------------------------------------------------------------------------- |
| • [Buffer Basics](Buffer-Basics.html)             |    | What is a buffer?                                                            |
| • [Current Buffer](Current-Buffer.html)           |    | Designating a buffer as current so that primitives will access its contents. |
| • [Buffer Names](Buffer-Names.html)               |    | Accessing and changing buffer names.                                         |
| • [Buffer File Name](Buffer-File-Name.html)       |    | The buffer file name indicates which file is visited.                        |
| • [Buffer Modification](Buffer-Modification.html) |    | A buffer is *modified* if it needs to be saved.                              |
| • [Modification Time](Modification-Time.html)     |    | Determining whether the visited file was changed behind Emacs’s back.        |
| • [Read Only Buffers](Read-Only-Buffers.html)     |    | Modifying text is not allowed in a read-only buffer.                         |
| • [Buffer List](Buffer-List.html)                 |    | How to look at all the existing buffers.                                     |
| • [Creating Buffers](Creating-Buffers.html)       |    | Functions that create buffers.                                               |
| • [Killing Buffers](Killing-Buffers.html)         |    | Buffers exist until explicitly killed.                                       |
| • [Indirect Buffers](Indirect-Buffers.html)       |    | An indirect buffer shares text with some other buffer.                       |
| • [Swapping Text](Swapping-Text.html)             |    | Swapping text between two buffers.                                           |
| • [Buffer Gap](Buffer-Gap.html)                   |    | The gap in the buffer.                                                       |

Next: [Windows](Windows.html), Previous: [Backups and Auto-Saving](Backups-and-Auto_002dSaving.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
