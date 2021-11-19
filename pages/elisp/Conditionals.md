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

Next: [Combining Conditions](Combining-Conditions.html), Previous: [Sequencing](Sequencing.html), Up: [Control Structures](Control-Structures.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 11.2 Conditionals

Conditional control structures choose among alternatives. Emacs Lisp has five conditional forms: `if`, which is much the same as in other languages; `when` and `unless`, which are variants of `if`; `cond`, which is a generalized case statement; and `pcase`, which is a generalization of `cond` (see [Pattern-Matching Conditional](Pattern_002dMatching-Conditional.html)).

*   Special Form: **if** *condition then-form else-forms…*

    `if` chooses between the `then-form` and the `else-forms` based on the value of `condition`. If the evaluated `condition` is non-`nil`, `then-form` is evaluated and the result returned. Otherwise, the `else-forms` are evaluated in textual order, and the value of the last one is returned. (The `else` part of `if` is an example of an implicit `progn`. See [Sequencing](Sequencing.html).)

    If `condition` has the value `nil`, and no `else-forms` are given, `if` returns `nil`.

    `if` is a special form because the branch that is not selected is never evaluated—it is ignored. Thus, in this example, `true` is not printed because `print` is never called:

        (if nil
            (print 'true)
          'very-false)
        ⇒ very-false

<!---->

*   Macro: **when** *condition then-forms…*

    This is a variant of `if` where there are no `else-forms`, and possibly several `then-forms`. In particular,

        (when condition a b c)

    is entirely equivalent to

        (if condition (progn a b c) nil)

<!---->

*   Macro: **unless** *condition forms…*

    This is a variant of `if` where there is no `then-form`:

        (unless condition a b c)

    is entirely equivalent to

        (if condition nil
           a b c)

<!---->

*   Special Form: **cond** *clause…*

    `cond` chooses among an arbitrary number of alternatives. Each `clause` in the `cond` must be a list. The CAR of this list is the `condition`; the remaining elements, if any, the `body-forms`. Thus, a clause looks like this:

        (condition body-forms…)

    `cond` tries the clauses in textual order, by evaluating the `condition` of each clause. If the value of `condition` is non-`nil`, the clause succeeds; then `cond` evaluates its `body-forms`, and returns the value of the last of `body-forms`. Any remaining clauses are ignored.

    If the value of `condition` is `nil`, the clause fails, so the `cond` moves on to the following clause, trying its `condition`.

    A clause may also look like this:

        (condition)

    Then, if `condition` is non-`nil` when tested, the `cond` form returns the value of `condition`.

    If every `condition` evaluates to `nil`, so that every clause fails, `cond` returns `nil`.

    The following example has four clauses, which test for the cases where the value of `x` is a number, string, buffer and symbol, respectively:

        (cond ((numberp x) x)
              ((stringp x) x)
              ((bufferp x)
               (setq temporary-hack x) ; multiple body-forms
               (buffer-name x))        ; in one clause
              ((symbolp x) (symbol-value x)))

    Often we want to execute the last clause whenever none of the previous clauses was successful. To do this, we use `t` as the `condition` of the last clause, like this: `(t body-forms)`. The form `t` evaluates to `t`, which is never `nil`, so this clause never fails, provided the `cond` gets to it at all. For example:

        (setq a 5)
        (cond ((eq a 'hack) 'foo)
              (t "default"))
        ⇒ "default"

    This `cond` expression returns `foo` if the value of `a` is `hack`, and returns the string `"default"` otherwise.

Any conditional construct can be expressed with `cond` or with `if`. Therefore, the choice between them is a matter of style. For example:

    (if a b c)
    ≡
    (cond (a b) (t c))

Next: [Combining Conditions](Combining-Conditions.html), Previous: [Sequencing](Sequencing.html), Up: [Control Structures](Control-Structures.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
