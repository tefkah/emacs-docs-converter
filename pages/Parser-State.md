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

Next: [Low-Level Parsing](Low_002dLevel-Parsing.html), Previous: [Position Parse](Position-Parse.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 35.6.3 Parser State

A *parser state* is a list of (currently) eleven elements describing the state of the syntactic parser, after it parses the text between a specified starting point and a specified end point in the buffer using `parse-partial-sexp` (see [Low-Level Parsing](Low_002dLevel-Parsing.html)). Parsing functions such as `syntax-ppss` (see [Position Parse](Position-Parse.html)) also return a parser state as the value. `parse-partial-sexp` can accept a parser state as an argument, for resuming parsing.

Here are the meanings of the elements of the parser state:

0.  The depth in parentheses, counting from 0. **Warning:** this can be negative if there are more close parens than open parens between the parser’s starting point and end point.
1.  The character position of the start of the innermost parenthetical grouping containing the stopping point; `nil` if none.
2.  The character position of the start of the last complete subexpression terminated; `nil` if none.
3.  Non-`nil` if inside a string. More precisely, this is the character that will terminate the string, or `t` if a generic string delimiter character should terminate it.
4.  `t` if inside a non-nestable comment (of any comment style; see [Syntax Flags](Syntax-Flags.html)); or the comment nesting level if inside a comment that can be nested.
5.  `t` if the end point is just after a quote character.
6.  The minimum parenthesis depth encountered during this scan.
7.  What kind of comment is active: `nil` if not in a comment or in a comment of style ‘`a`’; 1 for a comment of style ‘`b`’; 2 for a comment of style ‘`c`’; and `syntax-table` for a comment that should be ended by a generic comment delimiter character.
8.  The string or comment start position. While inside a comment, this is the position where the comment began; while inside a string, this is the position where the string began. When outside of strings and comments, this element is `nil`.
9.  The list of the positions of the currently open parentheses, starting with the outermost.
10. When the last buffer position scanned was the (potential) first character of a two character construct (comment delimiter or escaped/char-quoted character pair), the `syntax-code` (see [Syntax Table Internals](Syntax-Table-Internals.html)) of that position. Otherwise `nil`.

Elements 1, 2, and 6 are ignored in a state which you pass as an argument to `parse-partial-sexp` to continue parsing. Elements 9 and 10 are mainly used internally by the parser code.

Some additional useful information is available from a parser state using these functions:

*   Function: **syntax-ppss-toplevel-pos** *state*

    This function extracts, from parser state `state`, the last position scanned in the parse which was at top level in grammatical structure. “At top level” means outside of any parentheses, comments, or strings.

    The value is `nil` if `state` represents a parse which has arrived at a top level position.

<!---->

*   Function: **syntax-ppss-context** *state*

    Return `string` if the end position of the scan returning `state` is in a string, and `comment` if it’s in a comment.

Next: [Low-Level Parsing](Low_002dLevel-Parsing.html), Previous: [Position Parse](Position-Parse.html), Up: [Parsing Expressions](Parsing-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
