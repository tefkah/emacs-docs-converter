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

Next: [Declaring Functions](Declaring-Functions.html), Previous: [Inline Functions](Inline-Functions.html), Up: [Functions](Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 13.14 The `declare` Form

`declare` is a special macro which can be used to add meta properties to a function or macro: for example, marking it as obsolete, or giving its forms a special `TAB` indentation convention in Emacs Lisp mode.

*   Macro: **declare** *specs…*

    This macro ignores its arguments and evaluates to `nil`; it has no run-time effect. However, when a `declare` form occurs in the `declare` argument of a `defun` or `defsubst` function definition (see [Defining Functions](Defining-Functions.html)) or a `defmacro` macro definition (see [Defining Macros](Defining-Macros.html)), it appends the properties specified by `specs` to the function or macro. This work is specially performed by `defun`, `defsubst`, and `defmacro`.

    Each element in `specs` should have the form `(property args…)`, which should not be quoted. These have the following effects:

    *   `(advertised-calling-convention signature when)`

        This acts like a call to `set-advertised-calling-convention` (see [Obsolete Functions](Obsolete-Functions.html)); `signature` specifies the correct argument list for calling the function or macro, and `when` should be a string indicating when the old argument list was first made obsolete.

    *   `(debug edebug-form-spec)`

        This is valid for macros only. When stepping through the macro with Edebug, use `edebug-form-spec`. See [Instrumenting Macro Calls](Instrumenting-Macro-Calls.html).

    *   `(doc-string n)`

        This is used when defining a function or macro which itself will be used to define entities like functions, macros, or variables. It indicates that the `n`th argument, if any, should be considered as a documentation string.

    *   `(indent indent-spec)`

        Indent calls to this function or macro according to `indent-spec`. This is typically used for macros, though it works for functions too. See [Indenting Macros](Indenting-Macros.html).

    *   `(interactive-only value)`

        Set the function’s `interactive-only` property to `value`. See [The interactive-only property](Defining-Commands.html#The-interactive_002donly-property).

    *   `(obsolete current-name when)`

        Mark the function or macro as obsolete, similar to a call to `make-obsolete` (see [Obsolete Functions](Obsolete-Functions.html)). `current-name` should be a symbol (in which case the warning message says to use that instead), a string (specifying the warning message), or `nil` (in which case the warning message gives no extra details). `when` should be a string indicating when the function or macro was first made obsolete.

    *   `(compiler-macro expander)`

        This can only be used for functions, and tells the compiler to use `expander` as an optimization function. When encountering a call to the function, of the form `(function args…)`, the macro expander will call `expander` with that form as well as with `args`…, and `expander` can either return a new expression to use instead of the function call, or it can return just the form unchanged, to indicate that the function call should be left alone. `expander` can be a symbol, or it can be a form `(lambda (arg) body)` in which case `arg` will hold the original function call expression, and the (unevaluated) arguments to the function can be accessed using the function’s formal arguments.

    *   `(gv-expander expander)`

        Declare `expander` to be the function to handle calls to the macro (or function) as a generalized variable, similarly to `gv-define-expander`. `expander` can be a symbol or it can be of the form `(lambda (arg) body)` in which case that function will additionally have access to the macro (or function)’s arguments.

    *   `(gv-setter setter)`

        Declare `setter` to be the function to handle calls to the macro (or function) as a generalized variable. `setter` can be a symbol in which case it will be passed to `gv-define-simple-setter`, or it can be of the form `(lambda (arg) body)` in which case that function will additionally have access to the macro (or function)’s arguments and it will be passed to `gv-define-setter`.

Next: [Declaring Functions](Declaring-Functions.html), Previous: [Inline Functions](Inline-Functions.html), Up: [Functions](Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
