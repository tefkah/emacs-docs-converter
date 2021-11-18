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

Next: [Watching Variables](Watching-Variables.html), Previous: [Accessing Variables](Accessing-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 12.8 Setting Variable Values

The usual way to change the value of a variable is with the special form `setq`. When you need to compute the choice of variable at run time, use the function `set`.

*   Special Form: **setq** *\[symbol form]…*

    This special form is the most common method of changing a variable’s value. Each `symbol` is given a new value, which is the result of evaluating the corresponding `form`. The current binding of the symbol is changed.

    `setq` does not evaluate `symbol`; it sets the symbol that you write. We say that this argument is *automatically quoted*. The ‘`q`’ in `setq` stands for “quoted”.

    The value of the `setq` form is the value of the last `form`.

        (setq x (1+ 2))
             ⇒ 3

    <!---->

        x                   ; x now has a global value.
             ⇒ 3

    <!---->

        (let ((x 5))
          (setq x 6)        ; The local binding of x is set.
          x)
             ⇒ 6

    <!---->

        x                   ; The global value is unchanged.
             ⇒ 3

    Note that the first `form` is evaluated, then the first `symbol` is set, then the second `form` is evaluated, then the second `symbol` is set, and so on:

        (setq x 10          ; Notice that x is set before
              y (1+ x))     ;   the value of y is computed.
             ⇒ 11

<!---->

*   Function: **set** *symbol value*

    This function puts `value` in the value cell of `symbol`. Since it is a function rather than a special form, the expression written for `symbol` is evaluated to obtain the symbol to set. The return value is `value`.

    When dynamic variable binding is in effect (the default), `set` has the same effect as `setq`, apart from the fact that `set` evaluates its `symbol` argument whereas `setq` does not. But when a variable is lexically bound, `set` affects its *dynamic* value, whereas `setq` affects its current (lexical) value. See [Variable Scoping](Variable-Scoping.html).

        (set one 1)
        error→ Symbol's value as variable is void: one

    <!---->

        (set 'one 1)
             ⇒ 1

    <!---->

        (set 'two 'one)
             ⇒ one

    <!---->

        (set two 2)         ; two evaluates to symbol one.
             ⇒ 2

    <!---->

        one                 ; So it is one that was set.
             ⇒ 2
        (let ((one 1))      ; This binding of one is set,
          (set 'one 3)      ;   not the global value.
          one)
             ⇒ 3

    <!---->

        one
             ⇒ 2

    If `symbol` is not actually a symbol, a `wrong-type-argument` error is signaled.

        (set '(x y) 'z)
        error→ Wrong type argument: symbolp, (x y)

Next: [Watching Variables](Watching-Variables.html), Previous: [Accessing Variables](Accessing-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
