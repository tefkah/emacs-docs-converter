

Next: [Cleanups](Cleanups.html), Previous: [Examples of Catch](Examples-of-Catch.html), Up: [Nonlocal Exits](Nonlocal-Exits.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 11.7.3 Errors

When Emacs Lisp attempts to evaluate a form that, for some reason, cannot be evaluated, it *signals* an *error*.

When an error is signaled, Emacs’s default reaction is to print an error message and terminate execution of the current command. This is the right thing to do in most cases, such as if you type `C-f` at the end of the buffer.

In complicated programs, simple termination may not be what you want. For example, the program may have made temporary changes in data structures, or created temporary buffers that should be deleted before the program is finished. In such cases, you would use `unwind-protect` to establish *cleanup expressions* to be evaluated in case of error. (See [Cleanups](Cleanups.html).) Occasionally, you may wish the program to continue execution despite an error in a subroutine. In these cases, you would use `condition-case` to establish *error handlers* to recover control in case of error.

Resist the temptation to use error handling to transfer control from one part of the program to another; use `catch` and `throw` instead. See [Catch and Throw](Catch-and-Throw.html).

|                                                     |    |                                                 |
| :-------------------------------------------------- | -- | :---------------------------------------------- |
| • [Signaling Errors](Signaling-Errors.html)         |    | How to report an error.                         |
| • [Processing of Errors](Processing-of-Errors.html) |    | What Emacs does when you report an error.       |
| • [Handling Errors](Handling-Errors.html)           |    | How you can trap errors and continue execution. |
| • [Error Symbols](Error-Symbols.html)               |    | How errors are classified for trapping them.    |
