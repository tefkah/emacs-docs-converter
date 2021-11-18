<!-- This is the GNU Emacs Lisp Reference Manual
corresponding to Emacs version 27.2.

Copyright (C) 1990-1996, 1998-2021 Free Software Foundation,
Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being "GNU General Public License," with the
Front-Cover Texts being "A GNU Manual," and with the Back-Cover
Texts as in (a) below.  A copy of the license is included in the
section entitled "GNU Free Documentation License."

(a) The FSF's Back-Cover Text is: "You have the freedom to copy and
modify this GNU manual.  Buying copies from the FSF supports it in
developing GNU and promoting software freedom." -->

<!-- Created by GNU Texinfo 6.7, http://www.gnu.org/software/texinfo/ -->

Previous: [Char Classes](Char-Classes.html), Up: [Syntax of Regexps](Syntax-of-Regexps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.3.1.3 Backslash Constructs in Regular Expressions

For the most part, ‘`\`’ followed by any character matches only that character. However, there are several exceptions: certain sequences starting with ‘`\`’ that have special meanings. Here is a table of the special ‘`\`’ constructs.

*   ‘`\|`’

    specifies an alternative. Two regular expressions `a` and `b` with ‘`\|`’ in between form an expression that matches anything that either `a` or `b` matches.

    Thus, ‘`foo\|bar`’ matches either ‘`foo`’ or ‘`bar`’ but no other string.

    ‘`\|`’ applies to the largest possible surrounding expressions. Only a surrounding ‘`\( … \)`’ grouping can limit the grouping power of ‘`\|`’.

    If you need full backtracking capability to handle multiple uses of ‘`\|`’, use the POSIX regular expression functions (see [POSIX Regexps](POSIX-Regexps.html)).

*   ‘`\{m\}`’

    is a postfix operator that repeats the previous pattern exactly `m` times. Thus, ‘`x\{5\}`’ matches the string ‘`xxxxx`’ and nothing else. ‘`c[ad]\{3\}r`’ matches string such as ‘`caaar`’, ‘`cdddr`’, ‘`cadar`’, and so on.

*   ‘`\{m,n\}`’

    is a more general postfix operator that specifies repetition with a minimum of `m` repeats and a maximum of `n` repeats. If `m` is omitted, the minimum is 0; if `n` is omitted, there is no maximum. For both forms, `m` and `n`, if specified, may be no larger than 2\*\*16 - 1 .

    For example, ‘`c[ad]\{1,2\}r`’ matches the strings ‘`car`’, ‘`cdr`’, ‘`caar`’, ‘`cadr`’, ‘`cdar`’, and ‘`cddr`’, and nothing else.\
    ‘`\{0,1\}`’ or ‘`\{,1\}`’ is equivalent to ‘`?`’.\
    ‘`\{0,\}`’ or ‘`\{,\}`’ is equivalent to ‘`*`’.\
    ‘`\{1,\}`’ is equivalent to ‘`+`’.

*   ‘`\( … \)`’

    is a grouping construct that serves three purposes:

    1.  To enclose a set of ‘`\|`’ alternatives for other operations. Thus, the regular expression ‘`\(foo\|bar\)x`’ matches either ‘`foox`’ or ‘`barx`’.
    2.  To enclose a complicated expression for the postfix operators ‘`*`’, ‘`+`’ and ‘`?`’ to operate on. Thus, ‘`ba\(na\)*`’ matches ‘`ba`’, ‘`bana`’, ‘`banana`’, ‘`bananana`’, etc., with any number (zero or more) of ‘`na`’ strings.
    3.  To record a matched substring for future reference with ‘`\digit`’ (see below).

    This last application is not a consequence of the idea of a parenthetical grouping; it is a separate feature that was assigned as a second meaning to the same ‘`\( … \)`’ construct because, in practice, there was usually no conflict between the two meanings. But occasionally there is a conflict, and that led to the introduction of shy groups.

*   ‘`\(?: … \)`’

    is the *shy group* construct. A shy group serves the first two purposes of an ordinary group (controlling the nesting of other operators), but it does not get a number, so you cannot refer back to its value with ‘`\digit`’. Shy groups are particularly useful for mechanically-constructed regular expressions, because they can be added automatically without altering the numbering of ordinary, non-shy groups.

    Shy groups are also called *non-capturing* or *unnumbered groups*.

*   ‘`\(?num: … \)`’

    is the *explicitly numbered group* construct. Normal groups get their number implicitly, based on their position, which can be inconvenient. This construct allows you to force a particular group number. There is no particular restriction on the numbering, e.g., you can have several groups with the same number in which case the last one to match (i.e., the rightmost match) will win. Implicitly numbered groups always get the smallest integer larger than the one of any previous group.

*   ‘`\digit`’

    matches the same text that matched the `digit`th occurrence of a grouping (‘`\( … \)`’) construct.

    In other words, after the end of a group, the matcher remembers the beginning and end of the text matched by that group. Later on in the regular expression you can use ‘`\`’ followed by `digit` to match that same text, whatever it may have been.

    The strings matching the first nine grouping constructs appearing in the entire regular expression passed to a search or matching function are assigned numbers 1 through 9 in the order that the open parentheses appear in the regular expression. So you can use ‘`\1`’ through ‘`\9`’ to refer to the text matched by the corresponding grouping constructs.

    For example, ‘`\(.*\)\1`’ matches any newline-free string that is composed of two identical halves. The ‘`\(.*\)`’ matches the first half, which may be anything, but the ‘`\1`’ that follows must match the same exact text.

    If a ‘`\( … \)`’ construct matches more than once (which can happen, for instance, if it is followed by ‘`*`’), only the last match is recorded.

    If a particular grouping construct in the regular expression was never matched—for instance, if it appears inside of an alternative that wasn’t used, or inside of a repetition that repeated zero times—then the corresponding ‘`\digit`’ construct never matches anything. To use an artificial example, ‘`\(foo\(b*\)\|lose\)\2`’ cannot match ‘`lose`’: the second alternative inside the larger group matches it, but then ‘`\2`’ is undefined and can’t match anything. But it can match ‘`foobb`’, because the first alternative matches ‘`foob`’ and ‘`\2`’ matches ‘`b`’.

*   ‘`\w`’

    matches any word-constituent character. The editor syntax table determines which characters these are. See [Syntax Tables](Syntax-Tables.html).

*   ‘`\W`’

    matches any character that is not a word constituent.

*   ‘`\scode`’

    matches any character whose syntax is `code`. Here `code` is a character that represents a syntax code: thus, ‘`w`’ for word constituent, ‘`-`’ for whitespace, ‘`(`’ for open parenthesis, etc. To represent whitespace syntax, use either ‘`-`’ or a space character. See [Syntax Class Table](Syntax-Class-Table.html), for a list of syntax codes and the characters that stand for them.

*   ‘`\Scode`’

    matches any character whose syntax is not `code`.

*   ‘`\cc`’

    matches any character whose category is `c`. Here `c` is a character that represents a category: thus, ‘`c`’ for Chinese characters or ‘`g`’ for Greek characters in the standard category table. You can see the list of all the currently defined categories with `M-x describe-categories RET`. You can also define your own categories in addition to the standard ones using the `define-category` function (see [Categories](Categories.html)).

*   ‘`\Cc`’

    matches any character whose category is not `c`.

The following regular expression constructs match the empty string—that is, they don’t use up any characters—but whether they match depends on the context. For all, the beginning and end of the accessible portion of the buffer are treated as if they were the actual beginning and end of the buffer.

*   ‘`` \` ``’

    matches the empty string, but only at the beginning of the buffer or string being matched against.

*   ‘`\'`’

    matches the empty string, but only at the end of the buffer or string being matched against.

*   ‘`\=`’

    matches the empty string, but only at point. (This construct is not defined when matching against a string.)

*   ‘`\b`’

    matches the empty string, but only at the beginning or end of a word. Thus, ‘`\bfoo\b`’ matches any occurrence of ‘`foo`’ as a separate word. ‘`\bballs?\b`’ matches ‘`ball`’ or ‘`balls`’ as a separate word.

    ‘`\b`’ matches at the beginning or end of the buffer (or string) regardless of what text appears next to it.

*   ‘`\B`’

    matches the empty string, but *not* at the beginning or end of a word, nor at the beginning or end of the buffer (or string).

*   ‘`\<`’

    matches the empty string, but only at the beginning of a word. ‘`\<`’ matches at the beginning of the buffer (or string) only if a word-constituent character follows.

*   ‘`\>`’

    matches the empty string, but only at the end of a word. ‘`\>`’ matches at the end of the buffer (or string) only if the contents end with a word-constituent character.

*   ‘`\_<`’

    matches the empty string, but only at the beginning of a symbol. A symbol is a sequence of one or more word or symbol constituent characters. ‘`\_<`’ matches at the beginning of the buffer (or string) only if a symbol-constituent character follows.

*   ‘`\_>`’

    matches the empty string, but only at the end of a symbol. ‘`\_>`’ matches at the end of the buffer (or string) only if the contents end with a symbol-constituent character.

Not every string is a valid regular expression. For example, a string that ends inside a character alternative without a terminating ‘`]`’ is invalid, and so is a string that ends with a single ‘`\`’. If an invalid regular expression is passed to any of the search functions, an `invalid-regexp` error is signaled.

Previous: [Char Classes](Char-Classes.html), Up: [Syntax of Regexps](Syntax-of-Regexps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
