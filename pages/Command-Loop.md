

Next: [Keymaps](Keymaps.html), Previous: [Minibuffers](Minibuffers.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 21 Command Loop

When you run Emacs, it enters the *editor command loop* almost immediately. This loop reads key sequences, executes their definitions, and displays the results. In this chapter, we describe how these things are done, and the subroutines that allow Lisp programs to do them.

|                                                             |    |                                                           |
| :---------------------------------------------------------- | -- | :-------------------------------------------------------- |
| • [Command Overview](Command-Overview.html)                 |    | How the command loop reads commands.                      |
| • [Defining Commands](Defining-Commands.html)               |    | Specifying how a function should read arguments.          |
| • [Interactive Call](Interactive-Call.html)                 |    | Calling a command, so that it will read arguments.        |
| • [Distinguish Interactive](Distinguish-Interactive.html)   |    | Making a command distinguish interactive calls.           |
| • [Command Loop Info](Command-Loop-Info.html)               |    | Variables set by the command loop for you to examine.     |
| • [Adjusting Point](Adjusting-Point.html)                   |    | Adjustment of point after a command.                      |
| • [Input Events](Input-Events.html)                         |    | What input looks like when you read it.                   |
| • [Reading Input](Reading-Input.html)                       |    | How to read input events from the keyboard or mouse.      |
| • [Special Events](Special-Events.html)                     |    | Events processed immediately and individually.            |
| • [Waiting](Waiting.html)                                   |    | Waiting for user input or elapsed time.                   |
| • [Quitting](Quitting.html)                                 |    | How `C-g` works. How to catch or defer quitting.          |
| • [Prefix Command Arguments](Prefix-Command-Arguments.html) |    | How the commands to set prefix args work.                 |
| • [Recursive Editing](Recursive-Editing.html)               |    | Entering a recursive edit, and why you usually shouldn’t. |
| • [Disabling Commands](Disabling-Commands.html)             |    | How the command loop handles disabled commands.           |
| • [Command History](Command-History.html)                   |    | How the command history is set up, and how accessed.      |
| • [Keyboard Macros](Keyboard-Macros.html)                   |    | How keyboard macros are implemented.                      |

Next: [Keymaps](Keymaps.html), Previous: [Minibuffers](Minibuffers.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
