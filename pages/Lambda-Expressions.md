

Next: [Function Names](Function-Names.html), Previous: [What Is a Function](What-Is-a-Function.html), Up: [Functions](Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 13.2 Lambda Expressions

A lambda expression is a function object written in Lisp. Here is an example:

```lisp
(lambda (x)
  "Return the hyperbolic cosine of X."
  (* 0.5 (+ (exp x) (exp (- x)))))
```

In Emacs Lisp, such a list is a valid expression which evaluates to a function object.

A lambda expression, by itself, has no name; it is an *anonymous function*. Although lambda expressions can be used this way (see [Anonymous Functions](Anonymous-Functions.html)), they are more commonly associated with symbols to make *named functions* (see [Function Names](Function-Names.html)). Before going into these details, the following subsections describe the components of a lambda expression and what they do.

|                                                         |    |                                                 |
| :------------------------------------------------------ | -- | :---------------------------------------------- |
| • [Lambda Components](Lambda-Components.html)           |    | The parts of a lambda expression.               |
| • [Simple Lambda](Simple-Lambda.html)                   |    | A simple example.                               |
| • [Argument List](Argument-List.html)                   |    | Details and special features of argument lists. |
| • [Function Documentation](Function-Documentation.html) |    | How to put documentation in a function.         |
