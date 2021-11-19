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

Next: [Processing of Errors](Processing-of-Errors.html), Up: [Errors](Errors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 11.7.3.1 How to Signal an Error

*Signaling* an error means beginning error processing. Error processing normally aborts all or part of the running program and returns to a point that is set up to handle the error (see [Processing of Errors](Processing-of-Errors.html)). Here we describe how to signal an error.

Most errors are signaled automatically within Lisp primitives which you call for other purposes, such as if you try to take the CAR of an integer or move forward a character at the end of the buffer. You can also signal errors explicitly with the functions `error` and `signal`.

Quitting, which happens when the user types `C-g`, is not considered an error, but it is handled almost like an error. See [Quitting](Quitting.html).

Every error specifies an error message, one way or another. The message should state what is wrong (“File does not exist”), not how things ought to be (“File must exist”). The convention in Emacs Lisp is that error messages should start with a capital letter, but should not end with any sort of punctuation.

*   Function: **error** *format-string \&rest args*

    This function signals an error with an error message constructed by applying `format-message` (see [Formatting Strings](Formatting-Strings.html)) to `format-string` and `args`.

    These examples show typical uses of `error`:

        (error "That is an error -- try something else")
             error→ That is an error -- try something else

    ```
    ```

        (error "Invalid name `%s'" "A%%B")
             error→ Invalid name ‘A%%B’

    `error` works by calling `signal` with two arguments: the error symbol `error`, and a list containing the string returned by `format-message`.

    Typically grave accent and apostrophe in the format translate to matching curved quotes, e.g., ``"Missing `%s'"`` might result in `"Missing ‘foo’"`. See [Text Quoting Style](Text-Quoting-Style.html), for how to influence or inhibit this translation.

    **Warning:** If you want to use your own string as an error message verbatim, don’t just write `(error string)`. If `string` `string` contains ‘`%`’, ‘`` ` ``’, or ‘`'`’ it may be reformatted, with undesirable results. Instead, use `(error "%s" string)`.

<!---->

*   Function: **signal** *error-symbol data*

    This function signals an error named by `error-symbol`. The argument `data` is a list of additional Lisp objects relevant to the circumstances of the error.

    The argument `error-symbol` must be an *error symbol*—a symbol defined with `define-error`. This is how Emacs Lisp classifies different sorts of errors. See [Error Symbols](Error-Symbols.html), for a description of error symbols, error conditions and condition names.

    If the error is not handled, the two arguments are used in printing the error message. Normally, this error message is provided by the `error-message` property of `error-symbol`. If `data` is non-`nil`, this is followed by a colon and a comma separated list of the unevaluated elements of `data`. For `error`, the error message is the CAR of `data` (that must be a string). Subcategories of `file-error` are handled specially.

    The number and significance of the objects in `data` depends on `error-symbol`. For example, with a `wrong-type-argument` error, there should be two objects in the list: a predicate that describes the type that was expected, and the object that failed to fit that type.

    Both `error-symbol` and `data` are available to any error handlers that handle the error: `condition-case` binds a local variable to a list of the form `(error-symbol . data)` (see [Handling Errors](Handling-Errors.html)).

    The function `signal` never returns.

        (signal 'wrong-number-of-arguments '(x y))
             error→ Wrong number of arguments: x, y

    ```
    ```

        (signal 'no-such-error '("My unknown error condition"))
             error→ peculiar error: "My unknown error condition"

<!---->

*   Function: **user-error** *format-string \&rest args*

    This function behaves exactly like `error`, except that it uses the error symbol `user-error` rather than `error`. As the name suggests, this is intended to report errors on the part of the user, rather than errors in the code itself. For example, if you try to use the command `Info-history-back` (`l`) to move back beyond the start of your Info browsing history, Emacs signals a `user-error`. Such errors do not cause entry to the debugger, even when `debug-on-error` is non-`nil`. See [Error Debugging](Error-Debugging.html).

> **Common Lisp note:** Emacs Lisp has nothing like the Common Lisp concept of continuable errors.

Next: [Processing of Errors](Processing-of-Errors.html), Up: [Errors](Errors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
