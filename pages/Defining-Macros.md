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

Next: [Problems with Macros](Problems-with-Macros.html), Previous: [Compiling Macros](Compiling-Macros.html), Up: [Macros](Macros.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 14.4 Defining Macros

A Lisp macro object is a list whose CAR is `macro`, and whose CDR is a function. Expansion of the macro works by applying the function (with `apply`) to the list of *unevaluated* arguments from the macro call.

It is possible to use an anonymous Lisp macro just like an anonymous function, but this is never done, because it does not make sense to pass an anonymous macro to functionals such as `mapcar`. In practice, all Lisp macros have names, and they are almost always defined with the `defmacro` macro.

*   Macro: **defmacro** *name args \[doc] \[declare] body…*

    `defmacro` defines the symbol `name` (which should not be quoted) as a macro that looks like this:

        (macro lambda args . body)

    (Note that the CDR of this list is a lambda expression.) This macro object is stored in the function cell of `name`. The meaning of `args` is the same as in a function, and the keywords `&rest` and `&optional` may be used (see [Argument List](Argument-List.html)). Neither `name` nor `args` should be quoted. The return value of `defmacro` is undefined.

    `doc`, if present, should be a string specifying the macro’s documentation string. `declare`, if present, should be a `declare` form specifying metadata for the macro (see [Declare Form](Declare-Form.html)). Note that macros cannot have interactive declarations, since they cannot be called interactively.

Macros often need to construct large list structures from a mixture of constants and nonconstant parts. To make this easier, use the ‘`` ` ``’ syntax (see [Backquote](Backquote.html)). For example:

    (defmacro t-becomes-nil (variable)
      `(if (eq ,variable t)
           (setq ,variable nil)))

```
```

    (t-becomes-nil foo)
         ≡ (if (eq foo t) (setq foo nil))

Next: [Problems with Macros](Problems-with-Macros.html), Previous: [Compiling Macros](Compiling-Macros.html), Up: [Macros](Macros.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
