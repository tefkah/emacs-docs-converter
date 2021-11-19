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

Previous: [Handling Errors](Handling-Errors.html), Up: [Errors](Errors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 11.7.3.4 Error Symbols and Condition Names

When you signal an error, you specify an *error symbol* to specify the kind of error you have in mind. Each error has one and only one error symbol to categorize it. This is the finest classification of errors defined by the Emacs Lisp language.

These narrow classifications are grouped into a hierarchy of wider classes called *error conditions*, identified by *condition names*. The narrowest such classes belong to the error symbols themselves: each error symbol is also a condition name. There are also condition names for more extensive classes, up to the condition name `error` which takes in all kinds of errors (but not `quit`). Thus, each error has one or more condition names: `error`, the error symbol if that is distinct from `error`, and perhaps some intermediate classifications.

*   Function: **define-error** *name message \&optional parent*

    In order for a symbol to be an error symbol, it must be defined with `define-error` which takes a parent condition (defaults to `error`). This parent defines the conditions that this kind of error belongs to. The transitive set of parents always includes the error symbol itself, and the symbol `error`. Because quitting is not considered an error, the set of parents of `quit` is just `(quit)`.

In addition to its parents, the error symbol has a `message` which is a string to be printed when that error is signaled but not handled. If that message is not valid, the error message ‘`peculiar error`’ is used. See [Definition of signal](Signaling-Errors.html#Definition-of-signal).

Internally, the set of parents is stored in the `error-conditions` property of the error symbol and the message is stored in the `error-message` property of the error symbol.

Here is how we define a new error symbol, `new-error`:

    (define-error 'new-error "A new error" 'my-own-errors)

This error has several condition names: `new-error`, the narrowest classification; `my-own-errors`, which we imagine is a wider classification; and all the conditions of `my-own-errors` which should include `error`, which is the widest of all.

The error string should start with a capital letter but it should not end with a period. This is for consistency with the rest of Emacs.

Naturally, Emacs will never signal `new-error` on its own; only an explicit call to `signal` (see [Definition of signal](Signaling-Errors.html#Definition-of-signal)) in your code can do this:

    (signal 'new-error '(x y))
         error→ A new error: x, y

This error can be handled through any of its condition names. This example handles `new-error` and any other errors in the class `my-own-errors`:

    (condition-case foo
        (bar nil t)
      (my-own-errors nil))

The significant way that errors are classified is by their condition names—the names used to match errors with handlers. An error symbol serves only as a convenient way to specify the intended error message and list of condition names. It would be cumbersome to give `signal` a list of condition names rather than one error symbol.

By contrast, using only error symbols without condition names would seriously decrease the power of `condition-case`. Condition names make it possible to categorize errors at various levels of generality when you write an error handler. Using error symbols alone would eliminate all but the narrowest level of classification.

See [Standard Errors](Standard-Errors.html), for a list of the main error symbols and their conditions.

Previous: [Handling Errors](Handling-Errors.html), Up: [Errors](Errors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
