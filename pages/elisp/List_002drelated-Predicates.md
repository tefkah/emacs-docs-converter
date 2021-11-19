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

Next: [List Elements](List-Elements.html), Previous: [Cons Cells](Cons-Cells.html), Up: [Lists](Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 5.2 Predicates on Lists

The following predicates test whether a Lisp object is an atom, whether it is a cons cell or is a list, or whether it is the distinguished object `nil`. (Many of these predicates can be defined in terms of the others, but they are used so often that it is worth having them.)

*   Function: **consp** *object*

    This function returns `t` if `object` is a cons cell, `nil` otherwise. `nil` is not a cons cell, although it *is* a list.

<!---->

*   Function: **atom** *object*

    This function returns `t` if `object` is an atom, `nil` otherwise. All objects except cons cells are atoms. The symbol `nil` is an atom and is also a list; it is the only Lisp object that is both.

        (atom object) ≡ (not (consp object))

<!---->

*   Function: **listp** *object*

    This function returns `t` if `object` is a cons cell or `nil`. Otherwise, it returns `nil`.

        (listp '(1))
             ⇒ t

    <!---->

        (listp '())
             ⇒ t

<!---->

*   Function: **nlistp** *object*

    This function is the opposite of `listp`: it returns `t` if `object` is not a list. Otherwise, it returns `nil`.

        (listp object) ≡ (not (nlistp object))

<!---->

*   Function: **null** *object*

    This function returns `t` if `object` is `nil`, and returns `nil` otherwise. This function is identical to `not`, but as a matter of clarity we use `null` when `object` is considered a list and `not` when it is considered a truth value (see `not` in [Combining Conditions](Combining-Conditions.html)).

        (null '(1))
             ⇒ nil

    <!---->

        (null '())
             ⇒ t

<!---->

*   Function: **proper-list-p** *object*

    This function returns the length of `object` if it is a proper list, `nil` otherwise (see [Cons Cells](Cons-Cells.html)). In addition to satisfying `listp`, a proper list is neither circular nor dotted.

        (proper-list-p '(a b c))
            ⇒ 3

    <!---->

        (proper-list-p '(a b . c))
            ⇒ nil

Next: [List Elements](List-Elements.html), Previous: [Cons Cells](Cons-Cells.html), Up: [Lists](Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
