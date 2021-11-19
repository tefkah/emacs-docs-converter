

Next: [Edebug Display Update](Edebug-Display-Update.html), Up: [The Outside Context](The-Outside-Context.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.14.1 Checking Whether to Stop

Whenever Edebug is entered, it needs to save and restore certain data before even deciding whether to make trace information or stop the program.

*   `max-lisp-eval-depth` (see [Eval](Eval.html)) and `max-specpdl-size` (see [Local Variables](Local-Variables.html)) are both increased to reduce Edebug’s impact on the stack. You could, however, still run out of stack space when using Edebug. You can also enlarge the value of `edebug-max-depth` if Edebug reaches the limit of recursion depth instrumenting code that contains very large quoted lists.
*   The state of keyboard macro execution is saved and restored. While Edebug is active, `executing-kbd-macro` is bound to `nil` unless `edebug-continue-kbd-macro` is non-`nil`.
