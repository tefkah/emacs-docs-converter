

Next: [Yes-or-No Queries](Yes_002dor_002dNo-Queries.html), Previous: [Initial Input](Initial-Input.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.6 Completion

*Completion* is a feature that fills in the rest of a name starting from an abbreviation for it. Completion works by comparing the user’s input against a list of valid names and determining how much of the name is determined uniquely by what the user has typed. For example, when you type `C-x b` (`switch-to-buffer`) and then type the first few letters of the name of the buffer to which you wish to switch, and then type `TAB` (`minibuffer-complete`), Emacs extends the name as far as it can.

Standard Emacs commands offer completion for names of symbols, files, buffers, and processes; with the functions in this section, you can implement completion for other kinds of names.

The `try-completion` function is the basic primitive for completion: it returns the longest determined completion of a given initial string, with a given set of strings to match against.

The function `completing-read` provides a higher-level interface for completion. A call to `completing-read` specifies how to determine the list of valid names. The function then activates the minibuffer with a local keymap that binds a few keys to commands useful for completion. Other functions provide convenient simple interfaces for reading certain kinds of names with completion.

|                                                           |    |                                                                                      |
| :-------------------------------------------------------- | -- | :----------------------------------------------------------------------------------- |
| • [Basic Completion](Basic-Completion.html)               |    | Low-level functions for completing strings.                                          |
| • [Minibuffer Completion](Minibuffer-Completion.html)     |    | Invoking the minibuffer with completion.                                             |
| • [Completion Commands](Completion-Commands.html)         |    | Minibuffer commands that do completion.                                              |
| • [High-Level Completion](High_002dLevel-Completion.html) |    | Convenient special cases of completion (reading buffer names, variable names, etc.). |
| • [Reading File Names](Reading-File-Names.html)           |    | Using completion to read file names and shell commands.                              |
| • [Completion Variables](Completion-Variables.html)       |    | Variables controlling completion behavior.                                           |
| • [Programmed Completion](Programmed-Completion.html)     |    | Writing your own completion function.                                                |
| • [Completion in Buffers](Completion-in-Buffers.html)     |    | Completing text in ordinary buffers.                                                 |

Next: [Yes-or-No Queries](Yes_002dor_002dNo-Queries.html), Previous: [Initial Input](Initial-Input.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
