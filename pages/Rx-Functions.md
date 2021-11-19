

Next: [Extending Rx](Extending-Rx.html), Previous: [Rx Constructs](Rx-Constructs.html), Up: [Rx Notation](Rx-Notation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.3.3.2 Functions and macros using `rx` regexps

*   Macro: **rx** *rx-expr…*

    Translate the `rx-expr`s to a string regexp, as if they were the body of a `(seq …)` form. The `rx` macro expands to a string constant, or, if `literal` or `regexp` forms are used, a Lisp expression that evaluates to a string.

<!---->

*   Function: **rx-to-string** *rx-expr \&optional no-group*

    Translate `rx-expr` to a string regexp which is returned. If `no-group` is absent or nil, bracket the result in a non-capturing group, ‘`\(?:…\)`’, if necessary to ensure that a postfix operator appended to it will apply to the whole expression.

    Arguments to `literal` and `regexp` forms in `rx-expr` must be string literals.

The `pcase` macro can use `rx` expressions as patterns directly; see [rx in pcase](pcase-Macro.html#rx-in-pcase).

For mechanisms to add user-defined extensions to the `rx` notation, see [Extending Rx](Extending-Rx.html).
