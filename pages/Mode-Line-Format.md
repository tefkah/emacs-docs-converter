

Next: [Imenu](Imenu.html), Previous: [Minor Modes](Minor-Modes.html), Up: [Modes](Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 23.4 Mode Line Format

Each Emacs window (aside from minibuffer windows) typically has a mode line at the bottom, which displays status information about the buffer displayed in the window. The mode line contains information about the buffer, such as its name, associated file, depth of recursive editing, and major and minor modes. A window can also have a *header line*, which is much like the mode line but appears at the top of the window.

This section describes how to control the contents of the mode line and header line. We include it in this chapter because much of the information displayed in the mode line relates to the enabled major and minor modes.

|                                                   |    |                                                 |
| :------------------------------------------------ | -- | :---------------------------------------------- |
| • [Base](Mode-Line-Basics.html)                   |    | Basic ideas of mode line control.               |
| • [Data](Mode-Line-Data.html)                     |    | The data structure that controls the mode line. |
| • [Top](Mode-Line-Top.html)                       |    | The top level variable, mode-line-format.       |
| • [Mode Line Variables](Mode-Line-Variables.html) |    | Variables used in that data structure.          |
| • [%-Constructs](_0025_002dConstructs.html)       |    | Putting information into a mode line.           |
| • [Properties in Mode](Properties-in-Mode.html)   |    | Using text properties in the mode line.         |
| • [Header Lines](Header-Lines.html)               |    | Like a mode line, but at the top.               |
| • [Emulating Mode Line](Emulating-Mode-Line.html) |    | Formatting text as the mode line would.         |
