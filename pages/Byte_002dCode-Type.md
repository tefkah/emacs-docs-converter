

Next: [Record Type](Record-Type.html), Previous: [Primitive Function Type](Primitive-Function-Type.html), Up: [Programming Types](Programming-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.16 Byte-Code Function Type

*Byte-code function objects* are produced by byte-compiling Lisp code (see [Byte Compilation](Byte-Compilation.html)). Internally, a byte-code function object is much like a vector; however, the evaluator handles this data type specially when it appears in a function call. See [Byte-Code Objects](Byte_002dCode-Objects.html).

The printed representation and read syntax for a byte-code function object is like that for a vector, with an additional ‘`#`’ before the opening ‘`[`’.
