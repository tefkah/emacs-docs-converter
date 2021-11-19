

Next: [Quoting](Quoting.html), Previous: [Intro Eval](Intro-Eval.html), Up: [Evaluation](Evaluation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 10.2 Kinds of Forms

A Lisp object that is intended to be evaluated is called a *form* (or an *expression*). How Emacs evaluates a form depends on its data type. Emacs has three different kinds of form that are evaluated differently: symbols, lists, and all other types. This section describes all three kinds, one by one, starting with the other types, which are self-evaluating forms.

|                                                           |    |                                                                                       |
| :-------------------------------------------------------- | -- | :------------------------------------------------------------------------------------ |
| • [Self-Evaluating Forms](Self_002dEvaluating-Forms.html) |    | Forms that evaluate to themselves.                                                    |
| • [Symbol Forms](Symbol-Forms.html)                       |    | Symbols evaluate as variables.                                                        |
| • [Classifying Lists](Classifying-Lists.html)             |    | How to distinguish various sorts of list forms.                                       |
| • [Function Indirection](Function-Indirection.html)       |    | When a symbol appears as the car of a list, we find the real function via the symbol. |
| • [Function Forms](Function-Forms.html)                   |    | Forms that call functions.                                                            |
| • [Macro Forms](Macro-Forms.html)                         |    | Forms that call macros.                                                               |
| • [Special Forms](Special-Forms.html)                     |    | Special forms are idiosyncratic primitives, most of them extremely important.         |
| • [Autoloading](Autoloading.html)                         |    | Functions set up to load files containing their real definitions.                     |
