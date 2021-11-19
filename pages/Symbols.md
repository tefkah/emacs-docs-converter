

Next: [Evaluation](Evaluation.html), Previous: [Hash Tables](Hash-Tables.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 9 Symbols

A *symbol* is an object with a unique name. This chapter describes symbols, their components, their property lists, and how they are created and interned. Separate chapters describe the use of symbols as variables and as function names; see [Variables](Variables.html), and [Functions](Functions.html). For the precise read syntax for symbols, see [Symbol Type](Symbol-Type.html).

You can test whether an arbitrary Lisp object is a symbol with `symbolp`:

*   Function: **symbolp** *object*

    This function returns `t` if `object` is a symbol, `nil` otherwise.

|                                               |    |                                                                          |
| :-------------------------------------------- | -- | :----------------------------------------------------------------------- |
| • [Symbol Components](Symbol-Components.html) |    | Symbols have names, values, function definitions and property lists.     |
| • [Definitions](Definitions.html)             |    | A definition says how a symbol will be used.                             |
| • [Creating Symbols](Creating-Symbols.html)   |    | How symbols are kept unique.                                             |
| • [Symbol Properties](Symbol-Properties.html) |    | Each symbol has a property list for recording miscellaneous information. |
