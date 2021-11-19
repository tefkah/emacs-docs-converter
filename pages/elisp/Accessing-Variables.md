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

Next: [Setting Variables](Setting-Variables.html), Previous: [Tips for Defining](Tips-for-Defining.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 12.7 Accessing Variable Values

The usual way to reference a variable is to write the symbol which names it. See [Symbol Forms](Symbol-Forms.html).

Occasionally, you may want to reference a variable which is only determined at run time. In that case, you cannot specify the variable name in the text of the program. You can use the `symbol-value` function to extract the value.

*   Function: **symbol-value** *symbol*

    This function returns the value stored in `symbol`’s value cell. This is where the variable’s current (dynamic) value is stored. If the variable has no local binding, this is simply its global value. If the variable is void, a `void-variable` error is signaled.

    If the variable is lexically bound, the value reported by `symbol-value` is not necessarily the same as the variable’s lexical value, which is determined by the lexical environment rather than the symbol’s value cell. See [Variable Scoping](Variable-Scoping.html).

        (setq abracadabra 5)
             ⇒ 5

    <!---->

        (setq foo 9)
             ⇒ 9

    ```
    ```

        ;; Here the symbol abracadabra
        ;;   is the symbol whose value is examined.
        (let ((abracadabra 'foo))
          (symbol-value 'abracadabra))
             ⇒ foo

    ```
    ```

        ;; Here, the value of abracadabra,
        ;;   which is foo,
        ;;   is the symbol whose value is examined.
        (let ((abracadabra 'foo))
          (symbol-value abracadabra))
             ⇒ 9

    ```
    ```

        (symbol-value 'abracadabra)
             ⇒ 5
