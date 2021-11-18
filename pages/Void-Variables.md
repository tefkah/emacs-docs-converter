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

Next: [Defining Variables](Defining-Variables.html), Previous: [Local Variables](Local-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 12.4 When a Variable is Void

We say that a variable is void if its symbol has an unassigned value cell (see [Symbol Components](Symbol-Components.html)).

Under Emacs Lisp’s default dynamic scoping rule (see [Variable Scoping](Variable-Scoping.html)), the value cell stores the variable’s current (local or global) value. Note that an unassigned value cell is *not* the same as having `nil` in the value cell. The symbol `nil` is a Lisp object and can be the value of a variable, just as any other object can be; but it is still a value. If a variable is void, trying to evaluate the variable signals a `void-variable` error, instead of returning a value.

Under the optional lexical scoping rule, the value cell only holds the variable’s global value—the value outside of any lexical binding construct. When a variable is lexically bound, the local value is determined by the lexical environment; hence, variables can have local values even if their symbols’ value cells are unassigned.

*   Function: **makunbound** *symbol*

    This function empties out the value cell of `symbol`, making the variable void. It returns `symbol`.

    If `symbol` has a dynamic local binding, `makunbound` voids the current binding, and this voidness lasts only as long as the local binding is in effect. Afterwards, the previously shadowed local or global binding is reexposed; then the variable will no longer be void, unless the reexposed binding is void too.

    Here are some examples (assuming dynamic binding is in effect):

        (setq x 1)               ; Put a value in the global binding.
             ⇒ 1
        (let ((x 2))             ; Locally bind it.
          (makunbound 'x)        ; Void the local binding.
          x)
        error→ Symbol's value as variable is void: x

    <!---->

        x                        ; The global binding is unchanged.
             ⇒ 1

        (let ((x 2))             ; Locally bind it.
          (let ((x 3))           ; And again.
            (makunbound 'x)      ; Void the innermost-local binding.
            x))                  ; And refer: it’s void.
        error→ Symbol's value as variable is void: x

    ```
    ```

        (let ((x 2))
          (let ((x 3))
            (makunbound 'x))     ; Void inner binding, then remove it.
          x)                     ; Now outer let binding is visible.
             ⇒ 2

<!---->

*   Function: **boundp** *variable*

    This function returns `t` if `variable` (a symbol) is not void, and `nil` if it is void.

    Here are some examples (assuming dynamic binding is in effect):

        (boundp 'abracadabra)          ; Starts out void.
             ⇒ nil

    <!---->

        (let ((abracadabra 5))         ; Locally bind it.
          (boundp 'abracadabra))
             ⇒ t

    <!---->

        (boundp 'abracadabra)          ; Still globally void.
             ⇒ nil

    <!---->

        (setq abracadabra 5)           ; Make it globally nonvoid.
             ⇒ 5

    <!---->

        (boundp 'abracadabra)
             ⇒ t

Next: [Defining Variables](Defining-Variables.html), Previous: [Local Variables](Local-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
