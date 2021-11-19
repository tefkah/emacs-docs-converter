

Next: [Regexp Example](Regexp-Example.html), Up: [Regular Expressions](Regular-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.3.1 Syntax of Regular Expressions

Regular expressions have a syntax in which a few characters are special constructs and the rest are *ordinary*. An ordinary character is a simple regular expression that matches that character and nothing else. The special characters are ‘`.`’, ‘`*`’, ‘`+`’, ‘`?`’, ‘`[`’, ‘`^`’, ‘`$`’, and ‘`\`’; no new special characters will be defined in the future. The character ‘`]`’ is special if it ends a character alternative (see later). The character ‘`-`’ is special inside a character alternative. A ‘`[:`’ and balancing ‘`:]`’ enclose a character class inside a character alternative. Any other character appearing in a regular expression is ordinary, unless a ‘`\`’ precedes it.

For example, ‘`f`’ is not a special character, so it is ordinary, and therefore ‘`f`’ is a regular expression that matches the string ‘`f`’ and no other string. (It does *not* match the string ‘`fg`’, but it does match a *part* of that string.) Likewise, ‘`o`’ is a regular expression that matches only ‘`o`’.

Any two regular expressions `a` and `b` can be concatenated. The result is a regular expression that matches a string if `a` matches some amount of the beginning of that string and `b` matches the rest of the string.

As a simple example, we can concatenate the regular expressions ‘`f`’ and ‘`o`’ to get the regular expression ‘`fo`’, which matches only the string ‘`fo`’. Still trivial. To do something more powerful, you need to use one of the special regular expression constructs.

|                                             |    |                                                |
| :------------------------------------------ | -- | :--------------------------------------------- |
| • [Regexp Special](Regexp-Special.html)     |    | Special characters in regular expressions.     |
| • [Char Classes](Char-Classes.html)         |    | Character classes used in regular expressions. |
| • [Regexp Backslash](Regexp-Backslash.html) |    | Backslash-sequences in regular expressions.    |

Next: [Regexp Example](Regexp-Example.html), Up: [Regular Expressions](Regular-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
