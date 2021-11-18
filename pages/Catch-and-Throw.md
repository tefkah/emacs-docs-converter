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

Next: [Examples of Catch](Examples-of-Catch.html), Up: [Nonlocal Exits](Nonlocal-Exits.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 11.7.1 Explicit Nonlocal Exits: `catch` and `throw`

Most control constructs affect only the flow of control within the construct itself. The function `throw` is the exception to this rule of normal program execution: it performs a nonlocal exit on request. (There are other exceptions, but they are for error handling only.) `throw` is used inside a `catch`, and jumps back to that `catch`. For example:

    (defun foo-outer ()
      (catch 'foo
        (foo-inner)))

    (defun foo-inner ()
      …
      (if x
          (throw 'foo t))
      …)

The `throw` form, if executed, transfers control straight back to the corresponding `catch`, which returns immediately. The code following the `throw` is not executed. The second argument of `throw` is used as the return value of the `catch`.

The function `throw` finds the matching `catch` based on the first argument: it searches for a `catch` whose first argument is `eq` to the one specified in the `throw`. If there is more than one applicable `catch`, the innermost one takes precedence. Thus, in the above example, the `throw` specifies `foo`, and the `catch` in `foo-outer` specifies the same symbol, so that `catch` is the applicable one (assuming there is no other matching `catch` in between).

Executing `throw` exits all Lisp constructs up to the matching `catch`, including function calls. When binding constructs such as `let` or function calls are exited in this way, the bindings are unbound, just as they are when these constructs exit normally (see [Local Variables](Local-Variables.html)). Likewise, `throw` restores the buffer and position saved by `save-excursion` (see [Excursions](Excursions.html)), and the narrowing status saved by `save-restriction`. It also runs any cleanups established with the `unwind-protect` special form when it exits that form (see [Cleanups](Cleanups.html)).

The `throw` need not appear lexically within the `catch` that it jumps to. It can equally well be called from another function called within the `catch`. As long as the `throw` takes place chronologically after entry to the `catch`, and chronologically before exit from it, it has access to that `catch`. This is why `throw` can be used in commands such as `exit-recursive-edit` that throw back to the editor command loop (see [Recursive Editing](Recursive-Editing.html)).

> **Common Lisp note:** Most other versions of Lisp, including Common Lisp, have several ways of transferring control nonsequentially: `return`, `return-from`, and `go`, for example. Emacs Lisp has only `throw`. The `cl-lib` library provides versions of some of these. See [Blocks and Exits](https://www.gnu.org/software/emacs/manual/html_node/cl/Blocks-and-Exits.html#Blocks-and-Exits) in Common Lisp Extensions.

*   Special Form: **catch** *tag body…*

    `catch` establishes a return point for the `throw` function. The return point is distinguished from other such return points by `tag`, which may be any Lisp object except `nil`. The argument `tag` is evaluated normally before the return point is established.

    With the return point in effect, `catch` evaluates the forms of the `body` in textual order. If the forms execute normally (without error or nonlocal exit) the value of the last body form is returned from the `catch`.

    If a `throw` is executed during the execution of `body`, specifying the same value `tag`, the `catch` form exits immediately; the value it returns is whatever was specified as the second argument of `throw`.

<!---->

*   Function: **throw** *tag value*

    The purpose of `throw` is to return from a return point previously established with `catch`. The argument `tag` is used to choose among the various existing return points; it must be `eq` to the value specified in the `catch`. If multiple return points match `tag`, the innermost one is used.

    The argument `value` is used as the value to return from that `catch`.

    If no return point is in effect with tag `tag`, then a `no-catch` error is signaled with data `(tag value)`.

Next: [Examples of Catch](Examples-of-Catch.html), Up: [Nonlocal Exits](Nonlocal-Exits.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
