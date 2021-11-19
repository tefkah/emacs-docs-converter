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

Next: [SMIE Grammar](SMIE-Grammar.html), Previous: [SMIE setup](SMIE-setup.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.7.1.2 Operator Precedence Grammars

SMIE’s precedence grammars simply give to each token a pair of precedences: the left-precedence and the right-precedence. We say `T1 < T2` if the right-precedence of token `T1` is less than the left-precedence of token `T2`. A good way to read this `<` is as a kind of parenthesis: if we find `... T1 something T2 ...` then that should be parsed as `... T1 (something T2 ...` rather than as `... T1 something) T2 ...`. The latter interpretation would be the case if we had `T1 > T2`. If we have `T1 = T2`, it means that token T2 follows token T1 in the same syntactic construction, so typically we have `"begin" = "end"`. Such pairs of precedences are sufficient to express left-associativity or right-associativity of infix operators, nesting of tokens like parentheses and many other cases.

*   Function: **smie-prec2->grammar** *table*

    This function takes a *prec2* grammar `table` and returns an alist suitable for use in `smie-setup`. The *prec2* `table` is itself meant to be built by one of the functions below.

<!---->

*   Function: **smie-merge-prec2s** *\&rest tables*

    This function takes several *prec2* `tables` and merges them into a new *prec2* table.

<!---->

*   Function: **smie-precs->prec2** *precs*

    This function builds a *prec2* table from a table of precedences `precs`. `precs` should be a list, sorted by precedence (for example `"+"` will come before `"*"`), of elements of the form `(assoc op ...)`, where each `op` is a token that acts as an operator; `assoc` is their associativity, which can be either `left`, `right`, `assoc`, or `nonassoc`. All operators in a given element share the same precedence level and associativity.

<!---->

*   Function: **smie-bnf->prec2** *bnf \&rest resolvers*

    This function lets you specify the grammar using a BNF notation. It accepts a `bnf` description of the grammar along with a set of conflict resolution rules `resolvers`, and returns a *prec2* table.

    `bnf` is a list of nonterminal definitions of the form `(nonterm rhs1 rhs2 ...)` where each `rhs` is a (non-empty) list of terminals (aka tokens) or non-terminals.

    Not all grammars are accepted:

    *   An `rhs` cannot be an empty list (an empty list is never needed, since SMIE allows all non-terminals to match the empty string anyway).
    *   An `rhs` cannot have 2 consecutive non-terminals: each pair of non-terminals needs to be separated by a terminal (aka token). This is a fundamental limitation of operator precedence grammars.

    Additionally, conflicts can occur:

    *   The returned *prec2* table holds constraints between pairs of tokens, and for any given pair only one constraint can be present: T1 < T2, T1 = T2, or T1 > T2.
    *   A token can be an `opener` (something similar to an open-paren), a `closer` (like a close-paren), or `neither` of the two (e.g., an infix operator, or an inner token like `"else"`).

    Precedence conflicts can be resolved via `resolvers`, which is a list of *precs* tables (see `smie-precs->prec2`): for each precedence conflict, if those `precs` tables specify a particular constraint, then the conflict is resolved by using this constraint instead, else a conflict is reported and one of the conflicting constraints is picked arbitrarily and the others are simply ignored.

Next: [SMIE Grammar](SMIE-Grammar.html), Previous: [SMIE setup](SMIE-setup.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]