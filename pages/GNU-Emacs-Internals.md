

Next: [Standard Errors](Standard-Errors.html), Previous: [Tips](Tips.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## Appendix E GNU Emacs Internals

This chapter describes how the runnable Emacs executable is dumped with the preloaded Lisp libraries in it, how storage is allocated, and some internal aspects of GNU Emacs that may be of interest to C programmers.

|                                                               |    |                                                    |
| :------------------------------------------------------------ | -- | :------------------------------------------------- |
| • [Building Emacs](Building-Emacs.html)                       |    | How the dumped Emacs is made.                      |
| • [Pure Storage](Pure-Storage.html)                           |    | Kludge to make preloaded Lisp functions shareable. |
| • [Garbage Collection](Garbage-Collection.html)               |    | Reclaiming space for Lisp objects no longer used.  |
| • [Stack-allocated Objects](Stack_002dallocated-Objects.html) |    | Temporary conses and strings on C stack.           |
| • [Memory Usage](Memory-Usage.html)                           |    | Info about total size of Lisp objects made so far. |
| • [C Dialect](C-Dialect.html)                                 |    | What C variant Emacs is written in.                |
| • [Writing Emacs Primitives](Writing-Emacs-Primitives.html)   |    | Writing C code for Emacs.                          |
| • [Writing Dynamic Modules](Writing-Dynamic-Modules.html)     |    | Writing loadable modules for Emacs.                |
| • [Object Internals](Object-Internals.html)                   |    | Data formats of buffers, windows, processes.       |
| • [C Integer Types](C-Integer-Types.html)                     |    | How C integer types are used inside Emacs.         |
