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

Previous: [Output Functions](Output-Functions.html), Up: [Read and Print](Read-and-Print.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 19.6 Variables Affecting Output

*   Variable: **standard-output**

    The value of this variable is the default output stream—the stream that print functions use when the `stream` argument is `nil`. The default is `t`, meaning display in the echo area.

<!---->

*   Variable: **print-quoted**

    If this is non-`nil`, that means to print quoted forms using abbreviated reader syntax, e.g., `(quote foo)` prints as `'foo`, and `(function foo)` as `#'foo`. The default is `t`.

<!---->

*   Variable: **print-escape-newlines**

    If this variable is non-`nil`, then newline characters in strings are printed as ‘`\n`’ and formfeeds are printed as ‘`\f`’. Normally these characters are printed as actual newlines and formfeeds.

    This variable affects the print functions `prin1` and `print` that print with quoting. It does not affect `princ`. Here is an example using `prin1`:

        (prin1 "a\nb")
             -| "a
             -| b"
             ⇒ "a
        b"

    ```
    ```

        (let ((print-escape-newlines t))
          (prin1 "a\nb"))
             -| "a\nb"
             ⇒ "a
        b"

    In the second expression, the local binding of `print-escape-newlines` is in effect during the call to `prin1`, but not during the printing of the result.

<!---->

*   Variable: **print-escape-control-characters**

    If this variable is non-`nil`, control characters in strings are printed as backslash sequences by the print functions `prin1` and `print` that print with quoting. If this variable and `print-escape-newlines` are both non-`nil`, the latter takes precedences for newlines and formfeeds.

<!---->

*   Variable: **print-escape-nonascii**

    If this variable is non-`nil`, then unibyte non-ASCII characters in strings are unconditionally printed as backslash sequences by the print functions `prin1` and `print` that print with quoting.

    Those functions also use backslash sequences for unibyte non-ASCII characters, regardless of the value of this variable, when the output stream is a multibyte buffer or a marker pointing into one.

<!---->

*   Variable: **print-escape-multibyte**

    If this variable is non-`nil`, then multibyte non-ASCII characters in strings are unconditionally printed as backslash sequences by the print functions `prin1` and `print` that print with quoting.

    Those functions also use backslash sequences for multibyte non-ASCII characters, regardless of the value of this variable, when the output stream is a unibyte buffer or a marker pointing into one.

<!---->

*   Variable: **print-charset-text-property**

    This variable controls printing of ‘charset’ text property on printing a string. The value should be `nil`, `t`, or `default`.

    If the value is `nil`, `charset` text properties are never printed. If `t`, they are always printed.

    If the value is `default`, only print `charset` text properties if there is an “unexpected” `charset` property. For ascii characters, all charsets are considered “expected”. Otherwise, the expected `charset` property of a character is given by `char-charset`.

<!---->

*   Variable: **print-length**

    The value of this variable is the maximum number of elements to print in any list, vector or bool-vector. If an object being printed has more than this many elements, it is abbreviated with an ellipsis.

    If the value is `nil` (the default), then there is no limit.

        (setq print-length 2)
             ⇒ 2

    <!---->

        (print '(1 2 3 4 5))
             -| (1 2 ...)
             ⇒ (1 2 ...)

<!---->

*   Variable: **print-level**

    The value of this variable is the maximum depth of nesting of parentheses and brackets when printed. Any list or vector at a depth exceeding this limit is abbreviated with an ellipsis. A value of `nil` (which is the default) means no limit.

<!---->

*   *   User Option: **eval-expression-print-length**
    *   User Option: **eval-expression-print-level**

    These are the values for `print-length` and `print-level` used by `eval-expression`, and thus, indirectly, by many interactive evaluation commands (see [Evaluating Emacs Lisp Expressions](https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Eval.html#Lisp-Eval) in The GNU Emacs Manual).

These variables are used for detecting and reporting circular and shared structure:

*   Variable: **print-circle**

    If non-`nil`, this variable enables detection of circular and shared structure in printing. See [Circular Objects](Circular-Objects.html).

<!---->

*   Variable: **print-gensym**

    If non-`nil`, this variable enables detection of uninterned symbols (see [Creating Symbols](Creating-Symbols.html)) in printing. When this is enabled, uninterned symbols print with the prefix ‘`#:`’, which tells the Lisp reader to produce an uninterned symbol.

<!---->

*   Variable: **print-continuous-numbering**

    If non-`nil`, that means number continuously across print calls. This affects the numbers printed for ‘`#n=`’ labels and ‘`#m#`’ references. Don’t set this variable with `setq`; you should only bind it temporarily to `t` with `let`. When you do that, you should also bind `print-number-table` to `nil`.

<!---->

*   Variable: **print-number-table**

    This variable holds a vector used internally by printing to implement the `print-circle` feature. You should not use it except to bind it to `nil` when you bind `print-continuous-numbering`.

<!---->

*   Variable: **float-output-format**

    This variable specifies how to print floating-point numbers. The default is `nil`, meaning use the shortest output that represents the number without losing information.

    To control output format more precisely, you can put a string in this variable. The string should hold a ‘`%`’-specification to be used in the C function `sprintf`. For further restrictions on what you can use, see the variable’s documentation string.

Previous: [Output Functions](Output-Functions.html), Up: [Read and Print](Read-and-Print.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
