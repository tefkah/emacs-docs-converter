

Next: [Modes](Modes.html), Previous: [Command Loop](Command-Loop.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 22 Keymaps

The command bindings of input events are recorded in data structures called *keymaps*. Each entry in a keymap associates (or *binds*) an individual event type, either to another keymap or to a command. When an event type is bound to a keymap, that keymap is used to look up the next input event; this continues until a command is found. The whole process is called *key lookup*.

|                                                             |    |                                                                                                                 |
| :---------------------------------------------------------- | -- | :-------------------------------------------------------------------------------------------------------------- |
| • [Key Sequences](Key-Sequences.html)                       |    | Key sequences as Lisp objects.                                                                                  |
| • [Keymap Basics](Keymap-Basics.html)                       |    | Basic concepts of keymaps.                                                                                      |
| • [Format of Keymaps](Format-of-Keymaps.html)               |    | What a keymap looks like as a Lisp object.                                                                      |
| • [Creating Keymaps](Creating-Keymaps.html)                 |    | Functions to create and copy keymaps.                                                                           |
| • [Inheritance and Keymaps](Inheritance-and-Keymaps.html)   |    | How one keymap can inherit the bindings of another keymap.                                                      |
| • [Prefix Keys](Prefix-Keys.html)                           |    | Defining a key with a keymap as its definition.                                                                 |
| • [Active Keymaps](Active-Keymaps.html)                     |    | How Emacs searches the active keymaps for a key binding.                                                        |
| • [Searching Keymaps](Searching-Keymaps.html)               |    | A pseudo-Lisp summary of searching active maps.                                                                 |
| • [Controlling Active Maps](Controlling-Active-Maps.html)   |    | Each buffer has a local keymap to override the standard (global) bindings. A minor mode can also override them. |
| • [Key Lookup](Key-Lookup.html)                             |    | Finding a key’s binding in one keymap.                                                                          |
| • [Functions for Key Lookup](Functions-for-Key-Lookup.html) |    | How to request key lookup.                                                                                      |
| • [Changing Key Bindings](Changing-Key-Bindings.html)       |    | Redefining a key in a keymap.                                                                                   |
| • [Remapping Commands](Remapping-Commands.html)             |    | A keymap can translate one command to another.                                                                  |
| • [Translation Keymaps](Translation-Keymaps.html)           |    | Keymaps for translating sequences of events.                                                                    |
| • [Key Binding Commands](Key-Binding-Commands.html)         |    | Interactive interfaces for redefining keys.                                                                     |
| • [Scanning Keymaps](Scanning-Keymaps.html)                 |    | Looking through all keymaps, for printing help.                                                                 |
| • [Menu Keymaps](Menu-Keymaps.html)                         |    | Defining a menu as a keymap.                                                                                    |

Next: [Modes](Modes.html), Previous: [Command Loop](Command-Loop.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
