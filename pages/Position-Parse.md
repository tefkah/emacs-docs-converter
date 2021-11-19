

Next: [Parser State](Parser-State.html), Previous: [Motion via Parsing](Motion-via-Parsing.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 35.6.2 Finding the Parse State for a Position

For syntactic analysis, such as in indentation, often the useful thing is to compute the syntactic state corresponding to a given buffer position. This function does that conveniently.

*   Function: **syntax-ppss** *\&optional pos*

    This function returns the parser state that the parser would reach at position `pos` starting from the beginning of the visible portion of the buffer. See [Parser State](Parser-State.html), for a description of the parser state.

    The return value is the same as if you call the low-level parsing function `parse-partial-sexp` to parse from the beginning of the visible portion of the buffer to `pos` (see [Low-Level Parsing](Low_002dLevel-Parsing.html)). However, `syntax-ppss` uses caches to speed up the computation. Due to this optimization, the second value (previous complete subexpression) and sixth value (minimum parenthesis depth) in the returned parser state are not meaningful.

    This function has a side effect: it adds a buffer-local entry to `before-change-functions` (see [Change Hooks](Change-Hooks.html)) for `syntax-ppss-flush-cache` (see below). This entry keeps the cache consistent as the buffer is modified. However, the cache might not be updated if `syntax-ppss` is called while `before-change-functions` is temporarily let-bound, or if the buffer is modified without running the hook, such as when using `inhibit-modification-hooks`. In those cases, it is necessary to call `syntax-ppss-flush-cache` explicitly.

<!---->

*   Function: **syntax-ppss-flush-cache** *beg \&rest ignored-args*

    This function flushes the cache used by `syntax-ppss`, starting at position `beg`. The remaining arguments, `ignored-args`, are ignored; this function accepts them so that it can be directly used on hooks such as `before-change-functions` (see [Change Hooks](Change-Hooks.html)).

Next: [Parser State](Parser-State.html), Previous: [Motion via Parsing](Motion-via-Parsing.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
