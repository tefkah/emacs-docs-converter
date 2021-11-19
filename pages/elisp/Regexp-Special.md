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

Next: [Char Classes](Char-Classes.html), Up: [Syntax of Regexps](Syntax-of-Regexps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.3.1.1 Special Characters in Regular Expressions

Here is a list of the characters that are special in a regular expression.

*   ‘`.`’ (Period)

    is a special character that matches any single character except a newline. Using concatenation, we can make regular expressions like ‘`a.b`’, which matches any three-character string that begins with ‘`a`’ and ends with ‘`b`’.

*   ‘`*`’

    is not a construct by itself; it is a postfix operator that means to match the preceding regular expression repetitively as many times as possible. Thus, ‘`o*`’ matches any number of ‘`o`’s (including no ‘`o`’s).

    ‘`*`’ always applies to the *smallest* possible preceding expression. Thus, ‘`fo*`’ has a repeating ‘`o`’, not a repeating ‘`fo`’. It matches ‘`f`’, ‘`fo`’, ‘`foo`’, and so on.

    The matcher processes a ‘`*`’ construct by matching, immediately, as many repetitions as can be found. Then it continues with the rest of the pattern. If that fails, backtracking occurs, discarding some of the matches of the ‘`*`’-modified construct in the hope that this will make it possible to match the rest of the pattern. For example, in matching ‘`ca*ar`’ against the string ‘`caaar`’, the ‘`a*`’ first tries to match all three ‘`a`’s; but the rest of the pattern is ‘`ar`’ and there is only ‘`r`’ left to match, so this try fails. The next alternative is for ‘`a*`’ to match only two ‘`a`’s. With this choice, the rest of the regexp matches successfully.

    **Warning:** Nested repetition operators can run for a very long time, if they lead to ambiguous matching. For example, trying to match the regular expression ‘`\(x+y*\)*a`’ against the string ‘`xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxz`’ could take hours before it ultimately fails. Emacs must try each way of grouping the ‘`x`’s before concluding that none of them can work. In general, avoid expressions that can match the same string in multiple ways.

*   ‘`+`’

    is a postfix operator, similar to ‘`*`’ except that it must match the preceding expression at least once. So, for example, ‘`ca+r`’ matches the strings ‘`car`’ and ‘`caaaar`’ but not the string ‘`cr`’, whereas ‘`ca*r`’ matches all three strings.

*   ‘`?`’

    is a postfix operator, similar to ‘`*`’ except that it must match the preceding expression either once or not at all. For example, ‘`ca?r`’ matches ‘`car`’ or ‘`cr`’; nothing else.

*   ‘`*?`’, ‘`+?`’, ‘`??`’

    These are *non-greedy* variants of the operators ‘`*`’, ‘`+`’ and ‘`?`’. Where those operators match the largest possible substring (consistent with matching the entire containing expression), the non-greedy variants match the smallest possible substring (consistent with matching the entire containing expression).

    For example, the regular expression ‘`c[ad]*a`’ when applied to the string ‘`cdaaada`’ matches the whole string; but the regular expression ‘`c[ad]*?a`’, applied to that same string, matches just ‘`cda`’. (The smallest possible match here for ‘`[ad]*?`’ that permits the whole expression to match is ‘`d`’.)

*   ‘`[ … ]`’

    is a *character alternative*, which begins with ‘`[`’ and is terminated by ‘`]`’. In the simplest case, the characters between the two brackets are what this character alternative can match.

    Thus, ‘`[ad]`’ matches either one ‘`a`’ or one ‘`d`’, and ‘`[ad]*`’ matches any string composed of just ‘`a`’s and ‘`d`’s (including the empty string). It follows that ‘`c[ad]*r`’ matches ‘`cr`’, ‘`car`’, ‘`cdr`’, ‘`caddaar`’, etc.

    You can also include character ranges in a character alternative, by writing the starting and ending characters with a ‘`-`’ between them. Thus, ‘`[a-z]`’ matches any lower-case ASCII letter. Ranges may be intermixed freely with individual characters, as in ‘`[a-z$%.]`’, which matches any lower case ASCII letter or ‘`$`’, ‘`%`’ or period. However, the ending character of one range should not be the starting point of another one; for example, ‘`[a-m-z]`’ should be avoided.

    A character alternative can also specify named character classes (see [Char Classes](Char-Classes.html)). This is a POSIX feature. For example, ‘`[[:ascii:]]`’ matches any ASCII character. Using a character class is equivalent to mentioning each of the characters in that class; but the latter is not feasible in practice, since some classes include thousands of different characters. A character class should not appear as the lower or upper bound of a range.

    The usual regexp special characters are not special inside a character alternative. A completely different set of characters is special: ‘`]`’, ‘`-`’ and ‘`^`’. To include ‘`]`’ in a character alternative, put it at the beginning. To include ‘`^`’, put it anywhere but at the beginning. To include ‘`-`’, put it at the end. Thus, ‘`[]^-]`’ matches all three of these special characters. You cannot use ‘`\`’ to escape these three characters, since ‘`\`’ is not special here.

    The following aspects of ranges are specific to Emacs, in that POSIX allows but does not require this behavior and programs other than Emacs may behave differently:

    1.  If `case-fold-search` is non-`nil`, ‘`[a-z]`’ also matches upper-case letters.
    2.  A range is not affected by the locale’s collation sequence: it always represents the set of characters with codepoints ranging between those of its bounds, so that ‘`[a-z]`’ matches only ASCII letters, even outside the C or POSIX locale.
    3.  If the lower bound of a range is greater than its upper bound, the range is empty and represents no characters. Thus, ‘`[z-a]`’ always fails to match, and ‘`[^z-a]`’ matches any character, including newline. However, a reversed range should always be from the letter ‘`z`’ to the letter ‘`a`’ to make it clear that it is not a typo; for example, ‘`[+-*/]`’ should be avoided, because it matches only ‘`/`’ rather than the likely-intended four characters.

    Some kinds of character alternatives are not the best style even though they have a well-defined meaning in Emacs. They include:

    1.  Although a range’s bound can be almost any character, it is better style to stay within natural sequences of ASCII letters and digits because most people have not memorized character code tables. For example, ‘`[.-9]`’ is less clear than ‘`[./0-9]`’, and ‘``[`-~]``’ is less clear than ‘``[`a-z{|}~]``’. Unicode character escapes can help here; for example, for most programmers ‘`[ก-ฺ฿-๛]`’ is less clear than ‘`[\u0E01-\u0E3A\u0E3F-\u0E5B]`’.
    2.  Although a character alternative can include duplicates, it is better style to avoid them. For example, ‘`[XYa-yYb-zX]`’ is less clear than ‘`[XYa-z]`’.
    3.  Although a range can denote just one, two, or three characters, it is simpler to list the characters. For example, ‘`[a-a0]`’ is less clear than ‘`[a0]`’, ‘`[i-j]`’ is less clear than ‘`[ij]`’, and ‘`[i-k]`’ is less clear than ‘`[ijk]`’.
    4.  Although a ‘`-`’ can appear at the beginning of a character alternative or as the upper bound of a range, it is better style to put ‘`-`’ by itself at the end of a character alternative. For example, although ‘`[-a-z]`’ is valid, ‘`[a-z-]`’ is better style; and although ‘`[*--]`’ is valid, ‘`[*+,-]`’ is clearer.

*   ‘`[^ … ]`’

    ‘`[^`’ begins a *complemented character alternative*. This matches any character except the ones specified. Thus, ‘`[^a-z0-9A-Z]`’ matches all characters *except* ASCII letters and digits.

    ‘`^`’ is not special in a character alternative unless it is the first character. The character following the ‘`^`’ is treated as if it were first (in other words, ‘`-`’ and ‘`]`’ are not special there).

    A complemented character alternative can match a newline, unless newline is mentioned as one of the characters not to match. This is in contrast to the handling of regexps in programs such as `grep`.

    You can specify named character classes, just like in character alternatives. For instance, ‘`[^[:ascii:]]`’ matches any non-ASCII character. See [Char Classes](Char-Classes.html).

*   ‘`^`’

    When matching a buffer, ‘`^`’ matches the empty string, but only at the beginning of a line in the text being matched (or the beginning of the accessible portion of the buffer). Otherwise it fails to match anything. Thus, ‘`^foo`’ matches a ‘`foo`’ that occurs at the beginning of a line.

    When matching a string instead of a buffer, ‘`^`’ matches at the beginning of the string or after a newline character.

    For historical compatibility reasons, ‘`^`’ can be used only at the beginning of the regular expression, or after ‘`\(`’, ‘`\(?:`’ or ‘`\|`’.

*   ‘`$`’

    is similar to ‘`^`’ but matches only at the end of a line (or the end of the accessible portion of the buffer). Thus, ‘`x+$`’ matches a string of one ‘`x`’ or more at the end of a line.

    When matching a string instead of a buffer, ‘`$`’ matches at the end of the string or before a newline character.

    For historical compatibility reasons, ‘`$`’ can be used only at the end of the regular expression, or before ‘`\)`’ or ‘`\|`’.

*   ‘`\`’

    has two functions: it quotes the special characters (including ‘`\`’), and it introduces additional special constructs.

    Because ‘`\`’ quotes special characters, ‘`\$`’ is a regular expression that matches only ‘`$`’, and ‘`\[`’ is a regular expression that matches only ‘`[`’, and so on.

    Note that ‘`\`’ also has special meaning in the read syntax of Lisp strings (see [String Type](String-Type.html)), and must be quoted with ‘`\`’. For example, the regular expression that matches the ‘`\`’ character is ‘`\\`’. To write a Lisp string that contains the characters ‘`\\`’, Lisp syntax requires you to quote each ‘`\`’ with another ‘`\`’. Therefore, the read syntax for a regular expression matching ‘`\`’ is `"\\\\"`.

**Please note:** For historical compatibility, special characters are treated as ordinary ones if they are in contexts where their special meanings make no sense. For example, ‘`*foo`’ treats ‘`*`’ as ordinary since there is no preceding expression on which the ‘`*`’ can act. It is poor practice to depend on this behavior; quote the special character anyway, regardless of where it appears.

As a ‘`\`’ is not special inside a character alternative, it can never remove the special meaning of ‘`-`’ or ‘`]`’. So you should not quote these characters when they have no special meaning either. This would not clarify anything, since backslashes can legitimately precede these characters where they *have* special meaning, as in ‘`[^\]`’ (`"[^\\]"` for Lisp string syntax), which matches any single character except a backslash.

In practice, most ‘`]`’ that occur in regular expressions close a character alternative and hence are special. However, occasionally a regular expression may try to match a complex pattern of literal ‘`[`’ and ‘`]`’. In such situations, it sometimes may be necessary to carefully parse the regexp from the start to determine which square brackets enclose a character alternative. For example, ‘`[^][]]`’ consists of the complemented character alternative ‘`[^][]`’ (which matches any single character that is not a square bracket), followed by a literal ‘`]`’.

The exact rules are that at the beginning of a regexp, ‘`[`’ is special and ‘`]`’ not. This lasts until the first unquoted ‘`[`’, after which we are in a character alternative; ‘`[`’ is no longer special (except when it starts a character class) but ‘`]`’ is special, unless it immediately follows the special ‘`[`’ or that ‘`[`’ followed by a ‘`^`’. This lasts until the next special ‘`]`’ that does not end a character class. This ends the character alternative and restores the ordinary syntax of regular expressions; an unquoted ‘`[`’ is special again and a ‘`]`’ not.

Next: [Char Classes](Char-Classes.html), Up: [Syntax of Regexps](Syntax-of-Regexps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
