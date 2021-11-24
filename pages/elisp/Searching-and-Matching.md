

## 34 Searching and Matching

GNU Emacs provides two ways to search through a buffer for specified text: exact string searches and regular expression searches. After a regular expression search, you can examine the *match data* to determine which text matched the whole regular expression or various portions of it.

|                                                   |    |                                                                              |
| :------------------------------------------------ | -- | :--------------------------------------------------------------------------- |
| • [String Search](String-Search.html)             |    | Search for an exact match.                                                   |
| • [Searching and Case](Searching-and-Case.html)   |    | Case-independent or case-significant searching.                              |
| • [Regular Expressions](Regular-Expressions.html) |    | Describing classes of strings.                                               |
| • [Regexp Search](Regexp-Search.html)             |    | Searching for a match for a regexp.                                          |
| • [POSIX Regexps](POSIX-Regexps.html)             |    | Searching POSIX-style for the longest match.                                 |
| • [Match Data](Match-Data.html)                   |    | Finding out which part of the text matched, after a string or regexp search. |
| • [Search and Replace](Search-and-Replace.html)   |    | Commands that loop, searching and replacing.                                 |
| • [Standard Regexps](Standard-Regexps.html)       |    | Useful regexps for finding sentences, pages,...                              |

The ‘`skip-chars…`’ functions also perform a kind of searching. See [Skipping Characters](Skipping-Characters.html). To search for changes in character properties, see [Property Search](Property-Search.html).
