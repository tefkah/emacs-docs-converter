

### 6.4 Vectors

A *vector* is a general-purpose array whose elements can be any Lisp objects. (By contrast, the elements of a string can only be characters. See [Strings and Characters](Strings-and-Characters.html).) Vectors are used in Emacs for many purposes: as key sequences (see [Key Sequences](Key-Sequences.html)), as symbol-lookup tables (see [Creating Symbols](Creating-Symbols.html)), as part of the representation of a byte-compiled function (see [Byte Compilation](Byte-Compilation.html)), and more.

Like other arrays, vectors use zero-origin indexing: the first element has index 0.

Vectors are printed with square brackets surrounding the elements. Thus, a vector whose elements are the symbols `a`, `b` and `a` is printed as `[a b a]`. You can write vectors in the same way in Lisp input.

A vector, like a string or a number, is considered a constant for evaluation: the result of evaluating it is the same vector. This does not evaluate or even examine the elements of the vector. See [Self-Evaluating Forms](Self_002dEvaluating-Forms.html). Vectors written with square brackets should not be modified via `aset` or other destructive operations. See [Mutability](Mutability.html).

Here are examples illustrating these principles:

```lisp
(setq avector [1 two '(three) "four" [five]])
     ⇒ [1 two '(three) "four" [five]]
(eval avector)
     ⇒ [1 two '(three) "four" [five]]
(eq avector (eval avector))
     ⇒ t
```
