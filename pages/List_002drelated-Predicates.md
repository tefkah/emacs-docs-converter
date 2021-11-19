

Next: [List Elements](List-Elements.html), Previous: [Cons Cells](Cons-Cells.html), Up: [Lists](Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 5.2 Predicates on Lists

The following predicates test whether a Lisp object is an atom, whether it is a cons cell or is a list, or whether it is the distinguished object `nil`. (Many of these predicates can be defined in terms of the others, but they are used so often that it is worth having them.)

*   Function: **consp** *object*

    This function returns `t` if `object` is a cons cell, `nil` otherwise. `nil` is not a cons cell, although it *is* a list.

<!---->

*   Function: **atom** *object*

    This function returns `t` if `object` is an atom, `nil` otherwise. All objects except cons cells are atoms. The symbol `nil` is an atom and is also a list; it is the only Lisp object that is both.

    ```lisp
    (atom object) ≡ (not (consp object))
    ```

<!---->

*   Function: **listp** *object*

    This function returns `t` if `object` is a cons cell or `nil`. Otherwise, it returns `nil`.

    ```lisp
    (listp '(1))
         ⇒ t
    ```

    ```lisp
    (listp '())
         ⇒ t
    ```

<!---->

*   Function: **nlistp** *object*

    This function is the opposite of `listp`: it returns `t` if `object` is not a list. Otherwise, it returns `nil`.

    ```lisp
    (listp object) ≡ (not (nlistp object))
    ```

<!---->

*   Function: **null** *object*

    This function returns `t` if `object` is `nil`, and returns `nil` otherwise. This function is identical to `not`, but as a matter of clarity we use `null` when `object` is considered a list and `not` when it is considered a truth value (see `not` in [Combining Conditions](Combining-Conditions.html)).

    ```lisp
    (null '(1))
         ⇒ nil
    ```

    ```lisp
    (null '())
         ⇒ t
    ```

<!---->

*   Function: **proper-list-p** *object*

    This function returns the length of `object` if it is a proper list, `nil` otherwise (see [Cons Cells](Cons-Cells.html)). In addition to satisfying `listp`, a proper list is neither circular nor dotted.

    ```lisp
    (proper-list-p '(a b c))
        ⇒ 3
    ```

    ```lisp
    (proper-list-p '(a b . c))
        ⇒ nil
    ```

Next: [List Elements](List-Elements.html), Previous: [Cons Cells](Cons-Cells.html), Up: [Lists](Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
