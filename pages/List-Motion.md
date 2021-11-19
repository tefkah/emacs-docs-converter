

Next: [Skipping Characters](Skipping-Characters.html), Previous: [Screen Lines](Screen-Lines.html), Up: [Motion](Motion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 30.2.6 Moving over Balanced Expressions

Here are several functions concerned with balanced-parenthesis expressions (also called *sexps* in connection with moving across them in Emacs). The syntax table controls how these functions interpret various characters; see [Syntax Tables](Syntax-Tables.html). See [Parsing Expressions](Parsing-Expressions.html), for lower-level primitives for scanning sexps or parts of sexps. For user-level commands, see [Commands for Editing with Parentheses](https://www.gnu.org/software/emacs/manual/html_node/emacs/Parentheses.html#Parentheses) in The GNU Emacs Manual.

*   Command: **forward-list** *\&optional arg*

    This function moves forward across `arg` (default 1) balanced groups of parentheses. (Other syntactic entities such as words or paired string quotes are ignored.)

<!---->

*   Command: **backward-list** *\&optional arg*

    This function moves backward across `arg` (default 1) balanced groups of parentheses. (Other syntactic entities such as words or paired string quotes are ignored.)

<!---->

*   Command: **up-list** *\&optional arg escape-strings no-syntax-crossing*

    This function moves forward out of `arg` (default 1) levels of parentheses. A negative argument means move backward but still to a less deep spot. If `escape-strings` is non-`nil` (as it is interactively), move out of enclosing strings as well. If `no-syntax-crossing` is non-`nil` (as it is interactively), prefer to break out of any enclosing string instead of moving to the start of a list broken across multiple strings. On error, location of point is unspecified.

<!---->

*   Command: **backward-up-list** *\&optional arg escape-strings no-syntax-crossing*

    This function is just like `up-list`, but with a negated argument.

<!---->

*   Command: **down-list** *\&optional arg*

    This function moves forward into `arg` (default 1) levels of parentheses. A negative argument means move backward but still go deeper in parentheses (-`arg` levels).

<!---->

*   Command: **forward-sexp** *\&optional arg*

    This function moves forward across `arg` (default 1) balanced expressions. Balanced expressions include both those delimited by parentheses and other kinds, such as words and string constants. See [Parsing Expressions](Parsing-Expressions.html). For example,

    ```lisp
    ---------- Buffer: foo ----------
    (concat∗ "foo " (car x) y z)
    ---------- Buffer: foo ----------
    ```

    ```lisp
    ```

    ```lisp
    (forward-sexp 3)
         ⇒ nil

    ---------- Buffer: foo ----------
    (concat "foo " (car x) y∗ z)
    ---------- Buffer: foo ----------
    ```

<!---->

*   Command: **backward-sexp** *\&optional arg*

    This function moves backward across `arg` (default 1) balanced expressions.

<!---->

*   Command: **beginning-of-defun** *\&optional arg*

    This function moves back to the `arg`th beginning of a defun. If `arg` is negative, this actually moves forward, but it still moves to the beginning of a defun, not to the end of one. `arg` defaults to 1.

<!---->

*   Command: **end-of-defun** *\&optional arg*

    This function moves forward to the `arg`th end of a defun. If `arg` is negative, this actually moves backward, but it still moves to the end of a defun, not to the beginning of one. `arg` defaults to 1.

<!---->

*   User Option: **defun-prompt-regexp**

    If non-`nil`, this buffer-local variable holds a regular expression that specifies what text can appear before the open-parenthesis that starts a defun. That is to say, a defun begins on a line that starts with a match for this regular expression, followed by a character with open-parenthesis syntax.

<!---->

*   User Option: **open-paren-in-column-0-is-defun-start**

    If this variable’s value is non-`nil`, an open parenthesis in column 0 is considered to be the start of a defun. If it is `nil`, an open parenthesis in column 0 has no special meaning. The default is `t`. If a string literal happens to have a parenthesis in column 0, escape it with a backslash to avoid a false positive.

<!---->

*   Variable: **beginning-of-defun-function**

    If non-`nil`, this variable holds a function for finding the beginning of a defun. The function `beginning-of-defun` calls this function instead of using its normal method, passing it its optional argument. If the argument is non-`nil`, the function should move back by that many functions, like `beginning-of-defun` does.

<!---->

*   Variable: **end-of-defun-function**

    If non-`nil`, this variable holds a function for finding the end of a defun. The function `end-of-defun` calls this function instead of using its normal method.

Next: [Skipping Characters](Skipping-Characters.html), Previous: [Screen Lines](Screen-Lines.html), Up: [Motion](Motion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
