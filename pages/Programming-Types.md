

Next: [Editing Types](Editing-Types.html), Previous: [Comments](Comments.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 2.4 Programming Types

There are two general categories of types in Emacs Lisp: those having to do with Lisp programming, and those having to do with editing. The former exist in many Lisp implementations, in one form or another. The latter are unique to Emacs Lisp.

|                                                           |    |                                                                                                      |
| :-------------------------------------------------------- | -- | :--------------------------------------------------------------------------------------------------- |
| • [Integer Type](Integer-Type.html)                       |    | Numbers without fractional parts.                                                                    |
| • [Floating-Point Type](Floating_002dPoint-Type.html)     |    | Numbers with fractional parts and with a large range.                                                |
| • [Character Type](Character-Type.html)                   |    | The representation of letters, numbers and control characters.                                       |
| • [Symbol Type](Symbol-Type.html)                         |    | A multi-use object that refers to a function, variable, or property list, and has a unique identity. |
| • [Sequence Type](Sequence-Type.html)                     |    | Both lists and arrays are classified as sequences.                                                   |
| • [Cons Cell Type](Cons-Cell-Type.html)                   |    | Cons cells, and lists (which are made from cons cells).                                              |
| • [Array Type](Array-Type.html)                           |    | Arrays include strings and vectors.                                                                  |
| • [String Type](String-Type.html)                         |    | An (efficient) array of characters.                                                                  |
| • [Vector Type](Vector-Type.html)                         |    | One-dimensional arrays.                                                                              |
| • [Char-Table Type](Char_002dTable-Type.html)             |    | One-dimensional sparse arrays indexed by characters.                                                 |
| • [Bool-Vector Type](Bool_002dVector-Type.html)           |    | One-dimensional arrays of `t` or `nil`.                                                              |
| • [Hash Table Type](Hash-Table-Type.html)                 |    | Super-fast lookup tables.                                                                            |
| • [Function Type](Function-Type.html)                     |    | A piece of executable code you can call from elsewhere.                                              |
| • [Macro Type](Macro-Type.html)                           |    | A method of expanding an expression into another expression, more fundamental but less pretty.       |
| • [Primitive Function Type](Primitive-Function-Type.html) |    | A function written in C, callable from Lisp.                                                         |
| • [Byte-Code Type](Byte_002dCode-Type.html)               |    | A function written in Lisp, then compiled.                                                           |
| • [Record Type](Record-Type.html)                         |    | Compound objects with programmer-defined types.                                                      |
| • [Type Descriptors](Type-Descriptors.html)               |    | Objects holding information about types.                                                             |
| • [Autoload Type](Autoload-Type.html)                     |    | A type used for automatically loading seldom-used functions.                                         |
| • [Finalizer Type](Finalizer-Type.html)                   |    | Runs code when no longer reachable.                                                                  |
| ```lisp
 
```                                             |    |                                                                                                      |

Next: [Editing Types](Editing-Types.html), Previous: [Comments](Comments.html), Up: [Lisp Data Types](Lisp-Data-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
