

Previous: [Low-Level Parsing](Low_002dLevel-Parsing.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 35.6.5 Parameters to Control Parsing

*   Variable: **multibyte-syntax-as-symbol**

    If this variable is non-`nil`, `scan-sexps` treats all non-ASCII characters as symbol constituents regardless of what the syntax table says about them. (However, `syntax-table `text properties can still override the syntax.)

<!---->

*   User Option: **parse-sexp-ignore-comments**

    If the value is non-`nil`, then comments are treated as whitespace by the functions in this section and by `forward-sexp`, `scan-lists` and `scan-sexps`.

The behavior of `parse-partial-sexp` is also affected by `parse-sexp-lookup-properties` (see [Syntax Properties](Syntax-Properties.html)).

*   Variable: **comment-end-can-be-escaped**

    If this buffer local variable is non-`nil`, a single character which usually terminates a comment doesn’t do so when that character is escaped. This is used in C and C++ Modes, where line comments starting with ‘`//`’ can be continued onto the next line by escaping the newline with ‘`\`’.

You can use `forward-comment` to move forward or backward over one comment or several comments.
