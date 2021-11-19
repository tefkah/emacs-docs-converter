

Next: [Files](Files.html), Previous: [Modes](Modes.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 24 Documentation

GNU Emacs has convenient built-in help facilities, most of which derive their information from documentation strings associated with functions and variables. This chapter describes how to access documentation strings in Lisp programs.

The contents of a documentation string should follow certain conventions. In particular, its first line should be a complete sentence (or two complete sentences) that briefly describes what the function or variable does. See [Documentation Tips](Documentation-Tips.html), for how to write good documentation strings.

Note that the documentation strings for Emacs are not the same thing as the Emacs manual. Manuals have their own source files, written in the Texinfo language; documentation strings are specified in the definitions of the functions and variables they apply to. A collection of documentation strings is not sufficient as a manual because a good manual is not organized in that fashion; it is organized in terms of topics of discussion.

For commands to display documentation strings, see [Help](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help.html#Help) in The GNU Emacs Manual.

|                                                           |    |                                                                             |
| :-------------------------------------------------------- | -- | :-------------------------------------------------------------------------- |
| • [Documentation Basics](Documentation-Basics.html)       |    | Where doc strings are defined and stored.                                   |
| • [Accessing Documentation](Accessing-Documentation.html) |    | How Lisp programs can access doc strings.                                   |
| • [Keys in Documentation](Keys-in-Documentation.html)     |    | Substituting current key bindings.                                          |
| • [Text Quoting Style](Text-Quoting-Style.html)           |    | Quotation marks in doc strings and messages.                                |
| • [Describing Characters](Describing-Characters.html)     |    | Making printable descriptions of non-printing characters and key sequences. |
| • [Help Functions](Help-Functions.html)                   |    | Subroutines used by Emacs help facilities.                                  |

Next: [Files](Files.html), Previous: [Modes](Modes.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
