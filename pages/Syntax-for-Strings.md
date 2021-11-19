

Next: [Non-ASCII in Strings](Non_002dASCII-in-Strings.html), Up: [String Type](String-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.8.1 Syntax for Strings

The read syntax for a string is a double-quote, an arbitrary number of characters, and another double-quote, `"like this"`. To include a double-quote in a string, precede it with a backslash; thus, `"\""` is a string containing just one double-quote character. Likewise, you can include a backslash by preceding it with another backslash, like this: `"this \\ is a single embedded backslash"`.

The newline character is not special in the read syntax for strings; if you write a new line between the double-quotes, it becomes a character in the string. But an escaped newline—one that is preceded by ‘`\`’—does not become part of the string; i.e., the Lisp reader ignores an escaped newline while reading a string. An escaped space ‘`\ `’ is likewise ignored.

```lisp
"It is useful to include newlines
in documentation strings,
but the newline is \
ignored if escaped."
     ⇒ "It is useful to include newlines
in documentation strings,
but the newline is ignored if escaped."
```
