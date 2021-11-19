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

Previous: [Creating Buffer-Local](Creating-Buffer_002dLocal.html), Up: [Buffer-Local Variables](Buffer_002dLocal-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 12.11.3 The Default Value of a Buffer-Local Variable

The global value of a variable with buffer-local bindings is also called the *default* value, because it is the value that is in effect whenever neither the current buffer nor the selected frame has its own binding for the variable.

The functions `default-value` and `setq-default` access and change a variable’s default value regardless of whether the current buffer has a buffer-local binding. For example, you could use `setq-default` to change the default setting of `paragraph-start` for most buffers; and this would work even when you are in a C or Lisp mode buffer that has a buffer-local value for this variable.

The special forms `defvar` and `defconst` also set the default value (if they set the variable at all), rather than any buffer-local value.

*   Function: **default-value** *symbol*

    This function returns `symbol`’s default value. This is the value that is seen in buffers and frames that do not have their own values for this variable. If `symbol` is not buffer-local, this is equivalent to `symbol-value` (see [Accessing Variables](Accessing-Variables.html)).

<!---->

*   Function: **default-boundp** *symbol*

    The function `default-boundp` tells you whether `symbol`’s default value is nonvoid. If `(default-boundp 'foo)` returns `nil`, then `(default-value 'foo)` would get an error.

    `default-boundp` is to `default-value` as `boundp` is to `symbol-value`.

<!---->

*   Special Form: **setq-default** *\[symbol form]…*

    This special form gives each `symbol` a new default value, which is the result of evaluating the corresponding `form`. It does not evaluate `symbol`, but does evaluate `form`. The value of the `setq-default` form is the value of the last `form`.

    If a `symbol` is not buffer-local for the current buffer, and is not marked automatically buffer-local, `setq-default` has the same effect as `setq`. If `symbol` is buffer-local for the current buffer, then this changes the value that other buffers will see (as long as they don’t have a buffer-local value), but not the value that the current buffer sees.

        ;; In buffer ‘foo’:
        (make-local-variable 'buffer-local)
             ⇒ buffer-local

    <!---->

        (setq buffer-local 'value-in-foo)
             ⇒ value-in-foo

    <!---->

        (setq-default buffer-local 'new-default)
             ⇒ new-default

    <!---->

        buffer-local
             ⇒ value-in-foo

    <!---->

        (default-value 'buffer-local)
             ⇒ new-default

    ```
    ```

        ;; In (the new) buffer ‘bar’:
        buffer-local
             ⇒ new-default

    <!---->

        (default-value 'buffer-local)
             ⇒ new-default

    <!---->

        (setq buffer-local 'another-default)
             ⇒ another-default

    <!---->

        (default-value 'buffer-local)
             ⇒ another-default

    ```
    ```

        ;; Back in buffer ‘foo’:
        buffer-local
             ⇒ value-in-foo
        (default-value 'buffer-local)
             ⇒ another-default

<!---->

*   Function: **set-default** *symbol value*

    This function is like `setq-default`, except that `symbol` is an ordinary evaluated argument.

        (set-default (car '(a b c)) 23)
             ⇒ 23

    <!---->

        (default-value 'a)
             ⇒ 23

A variable can be let-bound (see [Local Variables](Local-Variables.html)) to a value. This makes its global value shadowed by the binding; `default-value` will then return the value from that binding, not the global value, and `set-default` will be prevented from setting the global value (it will change the let-bound value instead). The following two functions allow to reference the global value even if it’s shadowed by a let-binding.

*   Function: **default-toplevel-value** *symbol*

    This function returns the *top-level* default value of `symbol`, which is its value outside of any let-binding.

<!---->

    (defvar variable 'global-value)
        ⇒ variable

<!---->

    (let ((variable 'let-binding))
      (default-value 'variable))
        ⇒ let-binding

<!---->

    (let ((variable 'let-binding))
      (default-toplevel-value 'variable))
        ⇒ global-value

*   Function: **set-default-toplevel-value** *symbol value*

    This function sets the top-level default value of `symbol` to the specified `value`. This comes in handy when you want to set the global value of `symbol` regardless of whether your code runs in the context of `symbol`’s let-binding.

Previous: [Creating Buffer-Local](Creating-Buffer_002dLocal.html), Up: [Buffer-Local Variables](Buffer_002dLocal-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
