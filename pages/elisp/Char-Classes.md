

#### 34.3.1.2 Character Classes

Below is a table of the classes you can use in a character alternative, and what they mean. Note that the ‘`[`’ and ‘`]`’ characters that enclose the class name are part of the name, so a regular expression using these classes needs one more pair of brackets. For example, a regular expression matching a sequence of one or more letters and digits would be ‘`[[:alnum:]]+`’, not ‘`[:alnum:]+`’.

‘`[:ascii:]`’

This matches any ASCII character (codes 0–127).

‘`[:alnum:]`’

This matches any letter or digit. For multibyte characters, it matches characters whose Unicode ‘`general-category`’ property (see [Character Properties](Character-Properties.html)) indicates they are alphabetic or decimal number characters.

‘`[:alpha:]`’

This matches any letter. For multibyte characters, it matches characters whose Unicode ‘`general-category`’ property (see [Character Properties](Character-Properties.html)) indicates they are alphabetic characters.

‘`[:blank:]`’

This matches horizontal whitespace, as defined by Annex C of the Unicode Technical Standard #18. In particular, it matches spaces, tabs, and other characters whose Unicode ‘`general-category`’ property (see [Character Properties](Character-Properties.html)) indicates they are spacing separators.

‘`[:cntrl:]`’

This matches any character whose code is in the range 0–31.

‘`[:digit:]`’

This matches ‘`0`’ through ‘`9`’. Thus, ‘`[-+[:digit:]]`’ matches any digit, as well as ‘`+`’ and ‘`-`’.

‘`[:graph:]`’

This matches graphic characters—everything except whitespace, ASCII and non-ASCII control characters, surrogates, and codepoints unassigned by Unicode, as indicated by the Unicode ‘`general-category`’ property (see [Character Properties](Character-Properties.html)).

‘`[:lower:]`’

This matches any lower-case letter, as determined by the current case table (see [Case Tables](Case-Tables.html)). If `case-fold-search` is non-`nil`, this also matches any upper-case letter.

‘`[:multibyte:]`’

This matches any multibyte character (see [Text Representations](Text-Representations.html)).

‘`[:nonascii:]`’

This matches any non-ASCII character.

‘`[:print:]`’

This matches any printing character—either whitespace, or a graphic character matched by ‘`[:graph:]`’.

‘`[:punct:]`’

This matches any punctuation character. (At present, for multibyte characters, it matches anything that has non-word syntax.)

‘`[:space:]`’

This matches any character that has whitespace syntax (see [Syntax Class Table](Syntax-Class-Table.html)).

‘`[:unibyte:]`’

This matches any unibyte character (see [Text Representations](Text-Representations.html)).

‘`[:upper:]`’

This matches any upper-case letter, as determined by the current case table (see [Case Tables](Case-Tables.html)). If `case-fold-search` is non-`nil`, this also matches any lower-case letter.

‘`[:word:]`’

This matches any character that has word syntax (see [Syntax Class Table](Syntax-Class-Table.html)).

‘`[:xdigit:]`’

This matches the hexadecimal digits: ‘`0`’ through ‘`9`’, ‘`a`’ through ‘`f`’ and ‘`A`’ through ‘`F`’.
