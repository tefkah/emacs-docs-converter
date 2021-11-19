

Next: [Symbol Type](Symbol-Type.html), Previous: [Floating-Point Type](Floating_002dPoint-Type.html), Up: [Programming Types](Programming-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.3 Character Type

A *character* in Emacs Lisp is nothing more than an integer. In other words, characters are represented by their character codes. For example, the character `A` is represented as the integer 65.

Individual characters are used occasionally in programs, but it is more common to work with *strings*, which are sequences composed of characters. See [String Type](String-Type.html).

Characters in strings and buffers are currently limited to the range of 0 to 4194303—twenty two bits (see [Character Codes](Character-Codes.html)). Codes 0 through 127 are ASCII codes; the rest are non-ASCII (see [Non-ASCII Characters](Non_002dASCII-Characters.html)). Characters that represent keyboard input have a much wider range, to encode modifier keys such as Control, Meta and Shift.

There are special functions for producing a human-readable textual description of a character for the sake of messages. See [Describing Characters](Describing-Characters.html).

|                                                       |    |                                                |
| :---------------------------------------------------- | -- | :--------------------------------------------- |
| • [Basic Char Syntax](Basic-Char-Syntax.html)         |    | Syntax for regular characters.                 |
| • [General Escape Syntax](General-Escape-Syntax.html) |    | How to specify characters by their codes.      |
| • [Ctl-Char Syntax](Ctl_002dChar-Syntax.html)         |    | Syntax for control characters.                 |
| • [Meta-Char Syntax](Meta_002dChar-Syntax.html)       |    | Syntax for meta-characters.                    |
| • [Other Char Bits](Other-Char-Bits.html)             |    | Syntax for hyper-, super-, and alt-characters. |
