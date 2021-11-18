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

Next: [Function Documentation](Function-Documentation.html), Previous: [Simple Lambda](Simple-Lambda.html), Up: [Lambda Expressions](Lambda-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 13.2.3 Features of Argument Lists

Our simple sample function, `(lambda (a b c) (+ a b c))`, specifies three argument variables, so it must be called with three arguments: if you try to call it with only two arguments or four arguments, you get a `wrong-number-of-arguments` error (see [Errors](Errors.html)).

It is often convenient to write a function that allows certain arguments to be omitted. For example, the function `substring` accepts three arguments—a string, the start index and the end index—but the third argument defaults to the `length` of the string if you omit it. It is also convenient for certain functions to accept an indefinite number of arguments, as the functions `list` and `+` do.

To specify optional arguments that may be omitted when a function is called, simply include the keyword `&optional` before the optional arguments. To specify a list of zero or more extra arguments, include the keyword `&rest` before one final argument.

Thus, the complete syntax for an argument list is as follows:

    (required-vars…
     [&optional [optional-vars…]]
     [&rest [rest-var]])

The square brackets indicate that the `&optional` and `&rest` clauses, and the variables that follow them, are optional.

A call to the function requires one actual argument for each of the `required-vars`. There may be actual arguments for zero or more of the `optional-vars`, and there cannot be any actual arguments beyond that unless the lambda list uses `&rest`. In that case, there may be any number of extra actual arguments.

If actual arguments for the optional and rest variables are omitted, then they always default to `nil`. There is no way for the function to distinguish between an explicit argument of `nil` and an omitted argument. However, the body of the function is free to consider `nil` an abbreviation for some other meaningful value. This is what `substring` does; `nil` as the third argument to `substring` means to use the length of the string supplied.

> **Common Lisp note:** Common Lisp allows the function to specify what default value to use when an optional argument is omitted; Emacs Lisp always uses `nil`. Emacs Lisp does not support `supplied-p` variables that tell you whether an argument was explicitly passed.

For example, an argument list that looks like this:

    (a b &optional c d &rest e)

binds `a` and `b` to the first two actual arguments, which are required. If one or two more arguments are provided, `c` and `d` are bound to them respectively; any arguments after the first four are collected into a list and `e` is bound to that list. Thus, if there are only two arguments, `c`, `d` and `e` are `nil`; if two or three arguments, `d` and `e` are `nil`; if four arguments or fewer, `e` is `nil`. Note that exactly five arguments with an explicit `nil` argument provided for `e` will cause that `nil` argument to be passed as a list with one element, `(nil)`, as with any other single value for `e`.

There is no way to have required arguments following optional ones—it would not make sense. To see why this must be so, suppose that `c` in the example were optional and `d` were required. Suppose three actual arguments are given; which variable would the third argument be for? Would it be used for the `c`, or for `d`? One can argue for both possibilities. Similarly, it makes no sense to have any more arguments (either required or optional) after a `&rest` argument.

Here are some examples of argument lists and proper calls:

    (funcall (lambda (n) (1+ n))        ; One required:
             1)                         ; requires exactly one argument.
         ⇒ 2
    (funcall (lambda (n &optional n1)   ; One required and one optional:
               (if n1 (+ n n1) (1+ n))) ; 1 or 2 arguments.
             1 2)
         ⇒ 3
    (funcall (lambda (n &rest ns)       ; One required and one rest:
               (+ n (apply '+ ns)))     ; 1 or more arguments.
             1 2 3 4 5)
         ⇒ 15

Next: [Function Documentation](Function-Documentation.html), Previous: [Simple Lambda](Simple-Lambda.html), Up: [Lambda Expressions](Lambda-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
