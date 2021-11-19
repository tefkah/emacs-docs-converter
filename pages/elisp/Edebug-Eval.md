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

Next: [Eval List](Eval-List.html), Previous: [Edebug Views](Edebug-Views.html), Up: [Edebug](Edebug.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.9 Evaluation

While within Edebug, you can evaluate expressions as if Edebug were not running. Edebug tries to be invisible to the expression’s evaluation and printing. Evaluation of expressions that cause side effects will work as expected, except for changes to data that Edebug explicitly saves and restores. See [The Outside Context](The-Outside-Context.html), for details on this process.

*   `e exp RET`

    Evaluate expression `exp` in the context outside of Edebug (`edebug-eval-expression`). That is, Edebug tries to minimize its interference with the evaluation.

*   `M-: exp RET`

    Evaluate expression `exp` in the context of Edebug itself (`eval-expression`).

*   `C-x C-e`

    Evaluate the expression before point, in the context outside of Edebug (`edebug-eval-last-sexp`). With the prefix argument of zero (`C-u 0 C-x C-e`), don’t shorten long items (like strings and lists).

Edebug supports evaluation of expressions containing references to lexically bound symbols created by the following constructs in `cl.el`: `lexical-let`, `macrolet`, and `symbol-macrolet`.
