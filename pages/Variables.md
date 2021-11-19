

Next: [Functions](Functions.html), Previous: [Control Structures](Control-Structures.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 12 Variables

A *variable* is a name used in a program to stand for a value. In Lisp, each variable is represented by a Lisp symbol (see [Symbols](Symbols.html)). The variable name is simply the symbol’s name, and the variable’s value is stored in the symbol’s value cell[8](#FOOT8). See [Symbol Components](Symbol-Components.html). In Emacs Lisp, the use of a symbol as a variable is independent of its use as a function name.

As previously noted in this manual, a Lisp program is represented primarily by Lisp objects, and only secondarily as text. The textual form of a Lisp program is given by the read syntax of the Lisp objects that constitute the program. Hence, the textual form of a variable in a Lisp program is written using the read syntax for the symbol representing the variable.

|                                                                             |    |                                                                           |
| :-------------------------------------------------------------------------- | -- | :------------------------------------------------------------------------ |
| • [Global Variables](Global-Variables.html)                                 |    | Variable values that exist permanently, everywhere.                       |
| • [Constant Variables](Constant-Variables.html)                             |    | Variables that never change.                                              |
| • [Local Variables](Local-Variables.html)                                   |    | Variable values that exist only temporarily.                              |
| • [Void Variables](Void-Variables.html)                                     |    | Symbols that lack values.                                                 |
| • [Defining Variables](Defining-Variables.html)                             |    | A definition says a symbol is used as a variable.                         |
| • [Tips for Defining](Tips-for-Defining.html)                               |    | Things you should think about when you define a variable.                 |
| • [Accessing Variables](Accessing-Variables.html)                           |    | Examining values of variables whose names are known only at run time.     |
| • [Setting Variables](Setting-Variables.html)                               |    | Storing new values in variables.                                          |
| • [Watching Variables](Watching-Variables.html)                             |    | Running a function when a variable is changed.                            |
| • [Variable Scoping](Variable-Scoping.html)                                 |    | How Lisp chooses among local and global values.                           |
| • [Buffer-Local Variables](Buffer_002dLocal-Variables.html)                 |    | Variable values in effect only in one buffer.                             |
| • [File Local Variables](File-Local-Variables.html)                         |    | Handling local variable lists in files.                                   |
| • [Directory Local Variables](Directory-Local-Variables.html)               |    | Local variables common to all files in a directory.                       |
| • [Connection Local Variables](Connection-Local-Variables.html)             |    | Local variables common for remote connections.                            |
| • [Variable Aliases](Variable-Aliases.html)                                 |    | Variables that are aliases for other variables.                           |
| • [Variables with Restricted Values](Variables-with-Restricted-Values.html) |    | Non-constant variables whose value can *not* be an arbitrary Lisp object. |
| • [Generalized Variables](Generalized-Variables.html)                       |    | Extending the concept of variables.                                       |

***

#### Footnotes

##### [(8)](#DOCF8)

To be precise, under the default *dynamic scoping* rule, the value cell always holds the variable’s current value, but this is not the case under the *lexical scoping* rule. See [Variable Scoping](Variable-Scoping.html), for details.

Next: [Functions](Functions.html), Previous: [Control Structures](Control-Structures.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
