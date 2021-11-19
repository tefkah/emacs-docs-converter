

Next: [Control Structures](Control-Structures.html), Previous: [Symbols](Symbols.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 10 Evaluation

The *evaluation* of expressions in Emacs Lisp is performed by the *Lisp interpreter*—a program that receives a Lisp object as input and computes its *value as an expression*. How it does this depends on the data type of the object, according to rules described in this chapter. The interpreter runs automatically to evaluate portions of your program, but can also be called explicitly via the Lisp primitive function `eval`.

|                                       |    |                                                        |
| :------------------------------------ | -- | :----------------------------------------------------- |
| • [Intro Eval](Intro-Eval.html)       |    | Evaluation in the scheme of things.                    |
| • [Forms](Forms.html)                 |    | How various sorts of objects are evaluated.            |
| • [Quoting](Quoting.html)             |    | Avoiding evaluation (to put constants in the program). |
| • [Backquote](Backquote.html)         |    | Easier construction of list structure.                 |
| • [Eval](Eval.html)                   |    | How to invoke the Lisp interpreter explicitly.         |
| • [Deferred Eval](Deferred-Eval.html) |    | Deferred and lazy evaluation of forms.                 |
