

Next: [Primitive Function Type](Primitive-Function-Type.html), Previous: [Function Type](Function-Type.html), Up: [Programming Types](Programming-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.14 Macro Type

A *Lisp macro* is a user-defined construct that extends the Lisp language. It is represented as an object much like a function, but with different argument-passing semantics. A Lisp macro has the form of a list whose first element is the symbol `macro` and whose CDR is a Lisp function object, including the `lambda` symbol.

Lisp macro objects are usually defined with the built-in `defmacro` macro, but any list that begins with `macro` is a macro as far as Emacs is concerned. See [Macros](Macros.html), for an explanation of how to write a macro.

**Warning**: Lisp macros and keyboard macros (see [Keyboard Macros](Keyboard-Macros.html)) are entirely different things. When we use the word “macro” without qualification, we mean a Lisp macro, not a keyboard macro.
