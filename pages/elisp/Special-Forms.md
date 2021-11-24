

#### 10.2.7 Special Forms

A *special form* is a primitive function specially marked so that its arguments are not all evaluated. Most special forms define control structures or perform variable bindings—things which functions cannot do.

Each special form has its own rules for which arguments are evaluated and which are used without evaluation. Whether a particular argument is evaluated may depend on the results of evaluating other arguments.

If an expression’s first symbol is that of a special form, the expression should follow the rules of that special form; otherwise, Emacs’s behavior is not well-defined (though it will not crash). For example, `((lambda (x) x . 3) 4)` contains a subexpression that begins with `lambda` but is not a well-formed `lambda` expression, so Emacs may signal an error, or may return 3 or 4 or `nil`, or may behave in other ways.

### Function: **special-form-p** *object*

This predicate tests whether its argument is a special form, and returns `t` if so, `nil` otherwise.

Here is a list, in alphabetical order, of all of the special forms in Emacs Lisp with a reference to where each is described.

`and`

see [Combining Conditions](Combining-Conditions.html)

`catch`

see [Catch and Throw](Catch-and-Throw.html)

`cond`

see [Conditionals](Conditionals.html)

`condition-case`

see [Handling Errors](Handling-Errors.html)

`defconst`

see [Defining Variables](Defining-Variables.html)

`defvar`

see [Defining Variables](Defining-Variables.html)

`function`

see [Anonymous Functions](Anonymous-Functions.html)

`if`

see [Conditionals](Conditionals.html)

`interactive`

see [Interactive Call](Interactive-Call.html)

`lambda`

see [Lambda Expressions](Lambda-Expressions.html)

*   `let`
*   `let*`

see [Local Variables](Local-Variables.html)

`or`

see [Combining Conditions](Combining-Conditions.html)

*   `prog1`
*   `prog2`
*   `progn`

see [Sequencing](Sequencing.html)

`quote`

see [Quoting](Quoting.html)

`save-current-buffer`

see [Current Buffer](Current-Buffer.html)

`save-excursion`

see [Excursions](Excursions.html)

`save-restriction`

see [Narrowing](Narrowing.html)

`setq`

see [Setting Variables](Setting-Variables.html)

`setq-default`

see [Creating Buffer-Local](Creating-Buffer_002dLocal.html)

`unwind-protect`

see [Nonlocal Exits](Nonlocal-Exits.html)

`while`

see [Iteration](Iteration.html)

> **Common Lisp note:** Here are some comparisons of special forms in GNU Emacs Lisp and Common Lisp. `setq`, `if`, and `catch` are special forms in both Emacs Lisp and Common Lisp. `save-excursion` is a special form in Emacs Lisp, but doesn’t exist in Common Lisp. `throw` is a special form in Common Lisp (because it must be able to throw multiple values), but it is a function in Emacs Lisp (which doesn’t have multiple values).
