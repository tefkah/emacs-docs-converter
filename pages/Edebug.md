

Next: [Syntax Errors](Syntax-Errors.html), Previous: [Debugger](Debugger.html), Up: [Debugging](Debugging.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 18.2 Edebug

Edebug is a source-level debugger for Emacs Lisp programs, with which you can:

*   Step through evaluation, stopping before and after each expression.
*   Set conditional or unconditional breakpoints.
*   Stop when a specified condition is true (the global break event).
*   Trace slow or fast, stopping briefly at each stop point, or at each breakpoint.
*   Display expression results and evaluate expressions as if outside of Edebug.
*   Automatically re-evaluate a list of expressions and display their results each time Edebug updates the display.
*   Output trace information on function calls and returns.
*   Stop when an error occurs.
*   Display a backtrace, omitting Edebug’s own frames.
*   Specify argument evaluation for macros and defining forms.
*   Obtain rudimentary coverage testing and frequency counts.

The first three sections below should tell you enough about Edebug to start using it.

|                                                   |    |                                                                    |
| :------------------------------------------------ | -- | :----------------------------------------------------------------- |
| • [Using Edebug](Using-Edebug.html)               |    | Introduction to use of Edebug.                                     |
| • [Instrumenting](Instrumenting.html)             |    | You must instrument your code in order to debug it with Edebug.    |
| • [Modes](Edebug-Execution-Modes.html)            |    | Execution modes, stopping more or less often.                      |
| • [Jumping](Jumping.html)                         |    | Commands to jump to a specified place.                             |
| • [Misc](Edebug-Misc.html)                        |    | Miscellaneous commands.                                            |
| • [Breaks](Breaks.html)                           |    | Setting breakpoints to make the program stop.                      |
| • [Trapping Errors](Trapping-Errors.html)         |    | Trapping errors with Edebug.                                       |
| • [Views](Edebug-Views.html)                      |    | Views inside and outside of Edebug.                                |
| • [Eval](Edebug-Eval.html)                        |    | Evaluating expressions within Edebug.                              |
| • [Eval List](Eval-List.html)                     |    | Expressions whose values are displayed each time you enter Edebug. |
| • [Printing in Edebug](Printing-in-Edebug.html)   |    | Customization of printing.                                         |
| • [Trace Buffer](Trace-Buffer.html)               |    | How to produce trace output in a buffer.                           |
| • [Coverage Testing](Coverage-Testing.html)       |    | How to test evaluation coverage.                                   |
| • [The Outside Context](The-Outside-Context.html) |    | Data that Edebug saves and restores.                               |
| • [Edebug and Macros](Edebug-and-Macros.html)     |    | Specifying how to handle macro calls.                              |
| • [Options](Edebug-Options.html)                  |    | Option variables for customizing Edebug.                           |

Next: [Syntax Errors](Syntax-Errors.html), Previous: [Debugger](Debugger.html), Up: [Debugging](Debugging.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
