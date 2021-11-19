

Next: [Creating Strings](Creating-Strings.html), Previous: [String Basics](String-Basics.html), Up: [Strings and Characters](Strings-and-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 4.2 Predicates for Strings

For more information about general sequence and array predicates, see [Sequences Arrays Vectors](Sequences-Arrays-Vectors.html), and [Arrays](Arrays.html).

*   Function: **stringp** *object*

    This function returns `t` if `object` is a string, `nil` otherwise.

<!---->

*   Function: **string-or-null-p** *object*

    This function returns `t` if `object` is a string or `nil`. It returns `nil` otherwise.

<!---->

*   Function: **char-or-string-p** *object*

    This function returns `t` if `object` is a string or a character (i.e., an integer), `nil` otherwise.
