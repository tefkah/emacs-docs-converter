

Next: [Edebug and Macros](Edebug-and-Macros.html), Previous: [Coverage Testing](Coverage-Testing.html), Up: [Edebug](Edebug.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.14 The Outside Context

Edebug tries to be transparent to the program you are debugging, but it does not succeed completely. Edebug also tries to be transparent when you evaluate expressions with `e` or with the evaluation list buffer, by temporarily restoring the outside context. This section explains precisely what context Edebug restores, and how Edebug fails to be completely transparent.

|                                                             |    |                                  |
| :---------------------------------------------------------- | -- | :------------------------------- |
| • [Checking Whether to Stop](Checking-Whether-to-Stop.html) |    | When Edebug decides what to do.  |
| • [Edebug Display Update](Edebug-Display-Update.html)       |    | When Edebug updates the display. |
| • [Edebug Recursive Edit](Edebug-Recursive-Edit.html)       |    | When Edebug stops execution.     |
