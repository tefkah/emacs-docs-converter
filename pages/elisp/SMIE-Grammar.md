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

Next: [SMIE Lexer](SMIE-Lexer.html), Previous: [Operator Precedence Grammars](Operator-Precedence-Grammars.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.7.1.3 Defining the Grammar of a Language

The usual way to define the SMIE grammar of a language is by defining a new global variable that holds the precedence table by giving a set of BNF rules. For example, the grammar definition for a small Pascal-like language could look like:

    (require 'smie)
    (defvar sample-smie-grammar
      (smie-prec2->grammar
       (smie-bnf->prec2

<!---->

        '((id)
          (inst ("begin" insts "end")
                ("if" exp "then" inst "else" inst)
                (id ":=" exp)
                (exp))
          (insts (insts ";" insts) (inst))
          (exp (exp "+" exp)
               (exp "*" exp)
               ("(" exps ")"))
          (exps (exps "," exps) (exp)))

<!---->

        '((assoc ";"))
        '((assoc ","))
        '((assoc "+") (assoc "*")))))

A few things to note:

*   The above grammar does not explicitly mention the syntax of function calls: SMIE will automatically allow any sequence of sexps, such as identifiers, balanced parentheses, or `begin ... end` blocks to appear anywhere anyway.
*   The grammar category `id` has no right hand side: this does not mean that it can match only the empty string, since as mentioned any sequence of sexps can appear anywhere anyway.
*   Because non terminals cannot appear consecutively in the BNF grammar, it is difficult to correctly handle tokens that act as terminators, so the above grammar treats `";"` as a statement *separator* instead, which SMIE can handle very well.
*   Separators used in sequences (such as `","` and `";"` above) are best defined with BNF rules such as `(foo (foo "separator" foo) ...)` which generate precedence conflicts which are then resolved by giving them an explicit `(assoc "separator")`.
*   The `("(" exps ")")` rule was not needed to pair up parens, since SMIE will pair up any characters that are marked as having paren syntax in the syntax table. What this rule does instead (together with the definition of `exps`) is to make it clear that `","` should not appear outside of parentheses.
*   Rather than have a single *precs* table to resolve conflicts, it is preferable to have several tables, so as to let the BNF part of the grammar specify relative precedences where possible.
*   Unless there is a very good reason to prefer `left` or `right`, it is usually preferable to mark operators as associative, using `assoc`. For that reason `"+"` and `"*"` are defined above as `assoc`, although the language defines them formally as left associative.

Next: [SMIE Lexer](SMIE-Lexer.html), Previous: [Operator Precedence Grammars](Operator-Precedence-Grammars.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]