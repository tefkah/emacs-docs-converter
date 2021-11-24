

### 14.5 Common Problems Using Macros

Macro expansion can have counterintuitive consequences. This section describes some important consequences that can lead to trouble, and rules to follow to avoid trouble.

|                                                       |    |                                                                |
| :---------------------------------------------------- | -- | :------------------------------------------------------------- |
| • [Wrong Time](Wrong-Time.html)                       |    | Do the work in the expansion, not in the macro.                |
| • [Argument Evaluation](Argument-Evaluation.html)     |    | The expansion should evaluate each macro arg once.             |
| • [Surprising Local Vars](Surprising-Local-Vars.html) |    | Local variable bindings in the expansion require special care. |
| • [Eval During Expansion](Eval-During-Expansion.html) |    | Don’t evaluate them; put them in the expansion.                |
| • [Repeated Expansion](Repeated-Expansion.html)       |    | Avoid depending on how many times expansion is done.           |
