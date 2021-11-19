

Next: [Documentation](Documentation.html), Previous: [Keymaps](Keymaps.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 23 Major and Minor Modes

A *mode* is a set of definitions that customize Emacs behavior in useful ways. There are two varieties of modes: *minor modes*, which provide features that users can turn on and off while editing; and *major modes*, which are used for editing or interacting with a particular kind of text. Each buffer has exactly one *major mode* at a time.

This chapter describes how to write both major and minor modes, how to indicate them in the mode line, and how they run hooks supplied by the user. For related topics such as keymaps and syntax tables, see [Keymaps](Keymaps.html), and [Syntax Tables](Syntax-Tables.html).

|                                                 |    |                                                               |
| :---------------------------------------------- | -- | :------------------------------------------------------------ |
| • [Hooks](Hooks.html)                           |    | How to use hooks; how to write code that provides hooks.      |
| • [Major Modes](Major-Modes.html)               |    | Defining major modes.                                         |
| • [Minor Modes](Minor-Modes.html)               |    | Defining minor modes.                                         |
| • [Mode Line Format](Mode-Line-Format.html)     |    | Customizing the text that appears in the mode line.           |
| • [Imenu](Imenu.html)                           |    | Providing a menu of definitions made in a buffer.             |
| • [Font Lock Mode](Font-Lock-Mode.html)         |    | How modes can highlight text according to syntax.             |
| • [Auto-Indentation](Auto_002dIndentation.html) |    | How to teach Emacs to indent for a major mode.                |
| • [Desktop Save Mode](Desktop-Save-Mode.html)   |    | How modes can have buffer state saved between Emacs sessions. |
