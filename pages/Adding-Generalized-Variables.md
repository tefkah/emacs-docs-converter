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

Previous: [Setting Generalized Variables](Setting-Generalized-Variables.html), Up: [Generalized Variables](Generalized-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 12.17.2 Defining new `setf` forms

This section describes how to define new forms that `setf` can operate on.

*   Macro: **gv-define-simple-setter** *name setter \&optional fix-return*

    This macro enables you to easily define `setf` methods for simple cases. `name` is the name of a function, macro, or special form. You can use this macro whenever `name` has a directly corresponding `setter` function that updates it, e.g., `(gv-define-simple-setter car setcar)`.

    This macro translates a call of the form

        (setf (name args…) value)

    into

        (setter args… value)

    Such a `setf` call is documented to return `value`. This is no problem with, e.g., `car` and `setcar`, because `setcar` returns the value that it set. If your `setter` function does not return `value`, use a non-`nil` value for the `fix-return` argument of `gv-define-simple-setter`. This expands into something equivalent to

        (let ((temp value))
          (setter args… temp)
          temp)

    so ensuring that it returns the correct result.

<!---->

*   Macro: **gv-define-setter** *name arglist \&rest body*

    This macro allows for more complex `setf` expansions than the previous form. You may need to use this form, for example, if there is no simple setter function to call, or if there is one but it requires different arguments to the place form.

    This macro expands the form `(setf (name args…) value)` by first binding the `setf` argument forms `(value args…)` according to `arglist`, and then executing `body`. `body` should return a Lisp form that does the assignment, and finally returns the value that was set. An example of using this macro is:

        (gv-define-setter caar (val x) `(setcar (car ,x) ,val))

<!---->

*   Macro: **gv-define-expander** *name handler*

    For more control over the expansion, the `gv-define-expander` macro can be used. For instance, a settable `substring` could be implemented this way:

        (gv-define-expander substring
          (lambda (do place from &optional to)
            (gv-letplace (getter setter) place
              (macroexp-let2* nil ((start from) (end to))
                (funcall do `(substring ,getter ,start ,end)
                         (lambda (v)
                           (funcall setter `(cl--set-substring
                                             ,getter ,start ,end ,v))))))))

<!---->

*   Macro: **gv-letplace** *(getter setter) place \&rest body*

    The macro `gv-letplace` can be useful in defining macros that perform similarly to `setf`; for example, the `incf` macro of Common Lisp could be implemented this way:

        (defmacro incf (place &optional n)
          (gv-letplace (getter setter) place
            (macroexp-let2 nil v (or n 1)
              (funcall setter `(+ ,v ,getter)))))

    `getter` will be bound to a copyable expression that returns the value of `place`. `setter` will be bound to a function that takes an expression `v` and returns a new expression that sets `place` to `v`. `body` should return a Emacs Lisp expression manipulating `place` via `getter` and `setter`.

Consult the source file `gv.el` for more details.

> **Common Lisp note:** Common Lisp defines another way to specify the `setf` behavior of a function, namely `setf` functions, whose names are lists `(setf name)` rather than symbols. For example, `(defun (setf foo) …)` defines the function that is used when `setf` is applied to `foo`. Emacs does not support this. It is a compile-time error to use `setf` on a form that has not already had an appropriate expansion defined. In Common Lisp, this is not an error since the function `(setf func)` might be defined later.

Previous: [Setting Generalized Variables](Setting-Generalized-Variables.html), Up: [Generalized Variables](Generalized-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
