

Next: [Edebug](Edebug.html), Up: [Debugging](Debugging.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 18.1 The Lisp Debugger

The ordinary *Lisp debugger* provides the ability to suspend evaluation of a form. While evaluation is suspended (a state that is commonly known as a *break*), you may examine the run time stack, examine the values of local or global variables, or change those values. Since a break is a recursive edit, all the usual editing facilities of Emacs are available; you can even run programs that will enter the debugger recursively. See [Recursive Editing](Recursive-Editing.html).

|                                                       |    |                                                     |
| :---------------------------------------------------- | -- | :-------------------------------------------------- |
| • [Error Debugging](Error-Debugging.html)             |    | Entering the debugger when an error happens.        |
| • [Infinite Loops](Infinite-Loops.html)               |    | Stopping and debugging a program that doesn’t exit. |
| • [Function Debugging](Function-Debugging.html)       |    | Entering it when a certain function is called.      |
| • [Variable Debugging](Variable-Debugging.html)       |    | Entering it when a variable is modified.            |
| • [Explicit Debug](Explicit-Debug.html)               |    | Entering it at a certain point in the program.      |
| • [Using Debugger](Using-Debugger.html)               |    | What the debugger does.                             |
| • [Backtraces](Backtraces.html)                       |    | What you see while in the debugger.                 |
| • [Debugger Commands](Debugger-Commands.html)         |    | Commands used while in the debugger.                |
| • [Invoking the Debugger](Invoking-the-Debugger.html) |    | How to call the function `debug`.                   |
| • [Internals of Debugger](Internals-of-Debugger.html) |    | Subroutines of the debugger, and global variables.  |
