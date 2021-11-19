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

Next: [Eval During Expansion](Eval-During-Expansion.html), Previous: [Argument Evaluation](Argument-Evaluation.html), Up: [Problems with Macros](Problems-with-Macros.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 14.5.3 Local Variables in Macro Expansions

In the previous section, the definition of `for` was fixed as follows to make the expansion evaluate the macro arguments the proper number of times:

    (defmacro for (var from init to final do &rest body)
      "Execute a simple for loop: (for i from 1 to 10 do (print i))."

<!---->

      `(let ((,var ,init)
             (max ,final))
         (while (<= ,var max)
           ,@body
           (inc ,var))))

The new definition of `for` has a new problem: it introduces a local variable named `max` which the user does not expect. This causes trouble in examples such as the following:

    (let ((max 0))
      (for x from 0 to 10 do
        (let ((this (frob x)))
          (if (< max this)
              (setq max this)))))

The references to `max` inside the body of the `for`, which are supposed to refer to the user’s binding of `max`, really access the binding made by `for`.

The way to correct this is to use an uninterned symbol instead of `max` (see [Creating Symbols](Creating-Symbols.html)). The uninterned symbol can be bound and referred to just like any other symbol, but since it is created by `for`, we know that it cannot already appear in the user’s program. Since it is not interned, there is no way the user can put it into the program later. It will never appear anywhere except where put by `for`. Here is a definition of `for` that works this way:

    (defmacro for (var from init to final do &rest body)
      "Execute a simple for loop: (for i from 1 to 10 do (print i))."
      (let ((tempvar (make-symbol "max")))
        `(let ((,var ,init)
               (,tempvar ,final))
           (while (<= ,var ,tempvar)
             ,@body
             (inc ,var)))))

This creates an uninterned symbol named `max` and puts it in the expansion instead of the usual interned symbol `max` that appears in expressions ordinarily.

Next: [Eval During Expansion](Eval-During-Expansion.html), Previous: [Argument Evaluation](Argument-Evaluation.html), Up: [Problems with Macros](Problems-with-Macros.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
