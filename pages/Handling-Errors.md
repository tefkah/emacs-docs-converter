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

Next: [Error Symbols](Error-Symbols.html), Previous: [Processing of Errors](Processing-of-Errors.html), Up: [Errors](Errors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 11.7.3.3 Writing Code to Handle Errors

The usual effect of signaling an error is to terminate the command that is running and return immediately to the Emacs editor command loop. You can arrange to trap errors occurring in a part of your program by establishing an error handler, with the special form `condition-case`. A simple example looks like this:

    (condition-case nil
        (delete-file filename)
      (error nil))

This deletes the file named `filename`, catching any error and returning `nil` if an error occurs. (You can use the macro `ignore-errors` for a simple case like this; see below.)

The `condition-case` construct is often used to trap errors that are predictable, such as failure to open a file in a call to `insert-file-contents`. It is also used to trap errors that are totally unpredictable, such as when the program evaluates an expression read from the user.

The second argument of `condition-case` is called the *protected form*. (In the example above, the protected form is a call to `delete-file`.) The error handlers go into effect when this form begins execution and are deactivated when this form returns. They remain in effect for all the intervening time. In particular, they are in effect during the execution of functions called by this form, in their subroutines, and so on. This is a good thing, since, strictly speaking, errors can be signaled only by Lisp primitives (including `signal` and `error`) called by the protected form, not by the protected form itself.

The arguments after the protected form are handlers. Each handler lists one or more *condition names* (which are symbols) to specify which errors it will handle. The error symbol specified when an error is signaled also defines a list of condition names. A handler applies to an error if they have any condition names in common. In the example above, there is one handler, and it specifies one condition name, `error`, which covers all errors.

The search for an applicable handler checks all the established handlers starting with the most recently established one. Thus, if two nested `condition-case` forms offer to handle the same error, the inner of the two gets to handle it.

If an error is handled by some `condition-case` form, this ordinarily prevents the debugger from being run, even if `debug-on-error` says this error should invoke the debugger.

If you want to be able to debug errors that are caught by a `condition-case`, set the variable `debug-on-signal` to a non-`nil` value. You can also specify that a particular handler should let the debugger run first, by writing `debug` among the conditions, like this:

    (condition-case nil
        (delete-file filename)
      ((debug error) nil))

The effect of `debug` here is only to prevent `condition-case` from suppressing the call to the debugger. Any given error will invoke the debugger only if `debug-on-error` and the other usual filtering mechanisms say it should. See [Error Debugging](Error-Debugging.html).

*   Macro: **condition-case-unless-debug** *var protected-form handlers…*

    The macro `condition-case-unless-debug` provides another way to handle debugging of such forms. It behaves exactly like `condition-case`, unless the variable `debug-on-error` is non-`nil`, in which case it does not handle any errors at all.

Once Emacs decides that a certain handler handles the error, it returns control to that handler. To do so, Emacs unbinds all variable bindings made by binding constructs that are being exited, and executes the cleanups of all `unwind-protect` forms that are being exited. Once control arrives at the handler, the body of the handler executes normally.

After execution of the handler body, execution returns from the `condition-case` form. Because the protected form is exited completely before execution of the handler, the handler cannot resume execution at the point of the error, nor can it examine variable bindings that were made within the protected form. All it can do is clean up and proceed.

Error signaling and handling have some resemblance to `throw` and `catch` (see [Catch and Throw](Catch-and-Throw.html)), but they are entirely separate facilities. An error cannot be caught by a `catch`, and a `throw` cannot be handled by an error handler (though using `throw` when there is no suitable `catch` signals an error that can be handled).

*   Special Form: **condition-case** *var protected-form handlers…*

    This special form establishes the error handlers `handlers` around the execution of `protected-form`. If `protected-form` executes without error, the value it returns becomes the value of the `condition-case` form; in this case, the `condition-case` has no effect. The `condition-case` form makes a difference when an error occurs during `protected-form`.

    Each of the `handlers` is a list of the form `(conditions body…)`. Here `conditions` is an error condition name to be handled, or a list of condition names (which can include `debug` to allow the debugger to run before the handler). A condition name of `t` matches any condition. `body` is one or more Lisp expressions to be executed when this handler handles an error. Here are examples of handlers:

        (error nil)

        (arith-error (message "Division by zero"))

        ((arith-error file-error)
         (message
          "Either division by zero or failure to open a file"))

    Each error that occurs has an *error symbol* that describes what kind of error it is, and which describes also a list of condition names (see [Error Symbols](Error-Symbols.html)). Emacs searches all the active `condition-case` forms for a handler that specifies one or more of these condition names; the innermost matching `condition-case` handles the error. Within this `condition-case`, the first applicable handler handles the error.

    After executing the body of the handler, the `condition-case` returns normally, using the value of the last form in the handler body as the overall value.

    The argument `var` is a variable. `condition-case` does not bind this variable when executing the `protected-form`, only when it handles an error. At that time, it binds `var` locally to an *error description*, which is a list giving the particulars of the error. The error description has the form `(error-symbol . data)`. The handler can refer to this list to decide what to do. For example, if the error is for failure opening a file, the file name is the second element of `data`—the third element of the error description.

    If `var` is `nil`, that means no variable is bound. Then the error symbol and associated data are not available to the handler.

    Sometimes it is necessary to re-throw a signal caught by `condition-case`, for some outer-level handler to catch. Here’s how to do that:

          (signal (car err) (cdr err))

    where `err` is the error description variable, the first argument to `condition-case` whose error condition you want to re-throw. See [Definition of signal](Signaling-Errors.html#Definition-of-signal).

<!---->

*   Function: **error-message-string** *error-descriptor*

    This function returns the error message string for a given error descriptor. It is useful if you want to handle an error by printing the usual error message for that error. See [Definition of signal](Signaling-Errors.html#Definition-of-signal).

Here is an example of using `condition-case` to handle the error that results from dividing by zero. The handler displays the error message (but without a beep), then returns a very large number.

    (defun safe-divide (dividend divisor)
      (condition-case err
          ;; Protected form.
          (/ dividend divisor)

<!---->

        ;; The handler.
        (arith-error                        ; Condition.
         ;; Display the usual message for this error.
         (message "%s" (error-message-string err))
         1000000)))
    ⇒ safe-divide

```
```

    (safe-divide 5 0)
         -| Arithmetic error: (arith-error)
    ⇒ 1000000

The handler specifies condition name `arith-error` so that it will handle only division-by-zero errors. Other kinds of errors will not be handled (by this `condition-case`). Thus:

    (safe-divide nil 3)
         error→ Wrong type argument: number-or-marker-p, nil

Here is a `condition-case` that catches all kinds of errors, including those from `error`:

    (setq baz 34)
         ⇒ 34

```
```

    (condition-case err
        (if (eq baz 35)
            t
          ;; This is a call to the function error.
          (error "Rats!  The variable %s was %s, not 35" 'baz baz))
      ;; This is the handler; it is not a form.
      (error (princ (format "The error was: %s" err))
             2))
    -| The error was: (error "Rats!  The variable baz was 34, not 35")
    ⇒ 2

*   Macro: **ignore-errors** *body…*

    This construct executes `body`, ignoring any errors that occur during its execution. If the execution is without error, `ignore-errors` returns the value of the last form in `body`; otherwise, it returns `nil`.

    Here’s the example at the beginning of this subsection rewritten using `ignore-errors`:

          (ignore-errors
           (delete-file filename))

<!---->

*   Macro: **ignore-error** *condition body…*

    This macro is like `ignore-errors`, but will only ignore the specific error condition specified.

          (ignore-error end-of-file
            (read ""))

    `condition` can also be a list of error conditions.

<!---->

*   Macro: **with-demoted-errors** *format body…*

    This macro is like a milder version of `ignore-errors`. Rather than suppressing errors altogether, it converts them into messages. It uses the string `format` to format the message. `format` should contain a single ‘`%`’-sequence; e.g., `"Error: %S"`. Use `with-demoted-errors` around code that is not expected to signal errors, but should be robust if one does occur. Note that this macro uses `condition-case-unless-debug` rather than `condition-case`.

Next: [Error Symbols](Error-Symbols.html), Previous: [Processing of Errors](Processing-of-Errors.html), Up: [Errors](Errors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
