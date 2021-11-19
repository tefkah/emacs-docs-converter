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

Next: [Nonlocal Exits](Nonlocal-Exits.html), Previous: [Iteration](Iteration.html), Up: [Control Structures](Control-Structures.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 11.6 Generators

A *generator* is a function that produces a potentially-infinite stream of values. Each time the function produces a value, it suspends itself and waits for a caller to request the next value.

*   Macro: **iter-defun** *name args \[doc] \[declare] \[interactive] body…*

    `iter-defun` defines a generator function. A generator function has the same signature as a normal function, but works differently. Instead of executing `body` when called, a generator function returns an iterator object. That iterator runs `body` to generate values, emitting a value and pausing where `iter-yield` or `iter-yield-from` appears. When `body` returns normally, `iter-next` signals `iter-end-of-sequence` with `body`’s result as its condition data.

    Any kind of Lisp code is valid inside `body`, but `iter-yield` and `iter-yield-from` cannot appear inside `unwind-protect` forms.

<!---->

*   Macro: **iter-lambda** *args \[doc] \[interactive] body…*

    `iter-lambda` produces an unnamed generator function that works just like a generator function produced with `iter-defun`.

<!---->

*   Macro: **iter-yield** *value*

    When it appears inside a generator function, `iter-yield` indicates that the current iterator should pause and return `value` from `iter-next`. `iter-yield` evaluates to the `value` parameter of next call to `iter-next`.

<!---->

*   Macro: **iter-yield-from** *iterator*

    `iter-yield-from` yields all the values that `iterator` produces and evaluates to the value that `iterator`’s generator function returns normally. While it has control, `iterator` receives values sent to the iterator using `iter-next`.

To use a generator function, first call it normally, producing a *iterator* object. An iterator is a specific instance of a generator. Then use `iter-next` to retrieve values from this iterator. When there are no more values to pull from an iterator, `iter-next` raises an `iter-end-of-sequence` condition with the iterator’s final value.

It’s important to note that generator function bodies only execute inside calls to `iter-next`. A call to a function defined with `iter-defun` produces an iterator; you must drive this iterator with `iter-next` for anything interesting to happen. Each call to a generator function produces a *different* iterator, each with its own state.

*   Function: **iter-next** *iterator value*

    Retrieve the next value from `iterator`. If there are no more values to be generated (because `iterator`’s generator function returned), `iter-next` signals the `iter-end-of-sequence` condition; the data value associated with this condition is the value with which `iterator`’s generator function returned.

    `value` is sent into the iterator and becomes the value to which `iter-yield` evaluates. `value` is ignored for the first `iter-next` call to a given iterator, since at the start of `iterator`’s generator function, the generator function is not evaluating any `iter-yield` form.

<!---->

*   Function: **iter-close** *iterator*

    If `iterator` is suspended inside an `unwind-protect`’s `bodyform` and becomes unreachable, Emacs will eventually run unwind handlers after a garbage collection pass. (Note that `iter-yield` is illegal inside an `unwind-protect`’s `unwindforms`.) To ensure that these handlers are run before then, use `iter-close`.

Some convenience functions are provided to make working with iterators easier:

*   Macro: **iter-do** *(var iterator) body …*

    Run `body` with `var` bound to each value that `iterator` produces.

The Common Lisp loop facility also contains features for working with iterators. See [Loop Facility](https://www.gnu.org/software/emacs/manual/html_node/cl/Loop-Facility.html#Loop-Facility) in Common Lisp Extensions.

The following piece of code demonstrates some important principles of working with iterators.

    (require 'generator)
    (iter-defun my-iter (x)
      (iter-yield (1+ (iter-yield (1+ x))))
       ;; Return normally
      -1)

    (let* ((iter (my-iter 5))
           (iter2 (my-iter 0)))
      ;; Prints 6
      (print (iter-next iter))
      ;; Prints 9
      (print (iter-next iter 8))
      ;; Prints 1; iter and iter2 have distinct states
      (print (iter-next iter2 nil))

      ;; We expect the iter sequence to end now
      (condition-case x
          (iter-next iter)
        (iter-end-of-sequence
          ;; Prints -1, which my-iter returned normally
          (print (cdr x)))))

Next: [Nonlocal Exits](Nonlocal-Exits.html), Previous: [Iteration](Iteration.html), Up: [Control Structures](Control-Structures.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
