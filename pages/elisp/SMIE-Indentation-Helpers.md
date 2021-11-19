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

Next: [SMIE Indentation Example](SMIE-Indentation-Example.html), Previous: [SMIE Indentation](SMIE-Indentation.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.7.1.7 Helper Functions for Indentation Rules

SMIE provides various functions designed specifically for use in the indentation rules function (several of those functions break if used in another context). These functions all start with the prefix `smie-rule-`.

*   Function: **smie-rule-bolp**

    Return non-`nil` if the current token is the first on the line.

<!---->

*   Function: **smie-rule-hanging-p**

    Return non-`nil` if the current token is *hanging*. A token is *hanging* if it is the last token on the line and if it is preceded by other tokens: a lone token on a line is not hanging.

<!---->

*   Function: **smie-rule-next-p** *\&rest tokens*

    Return non-`nil` if the next token is among `tokens`.

<!---->

*   Function: **smie-rule-prev-p** *\&rest tokens*

    Return non-`nil` if the previous token is among `tokens`.

<!---->

*   Function: **smie-rule-parent-p** *\&rest parents*

    Return non-`nil` if the current token’s parent is among `parents`.

<!---->

*   Function: **smie-rule-sibling-p**

    Return non-`nil` if the current token’s parent is actually a sibling. This is the case for example when the parent of a `","` is just the previous `","`.

<!---->

*   Function: **smie-rule-parent** *\&optional offset*

    Return the proper offset to align the current token with the parent. If non-`nil`, `offset` should be an integer giving an additional offset to apply.

<!---->

*   Function: **smie-rule-separator** *method*

    Indent current token as a *separator*.

    By *separator*, we mean here a token whose sole purpose is to separate various elements within some enclosing syntactic construct, and which does not have any semantic significance in itself (i.e., it would typically not exist as a node in an abstract syntax tree).

    Such a token is expected to have an associative syntax and be closely tied to its syntactic parent. Typical examples are `","` in lists of arguments (enclosed inside parentheses), or `";"` in sequences of instructions (enclosed in a `{...}` or `begin...end` block).

    `method` should be the method name that was passed to `smie-rules-function`.

Next: [SMIE Indentation Example](SMIE-Indentation-Example.html), Previous: [SMIE Indentation](SMIE-Indentation.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
