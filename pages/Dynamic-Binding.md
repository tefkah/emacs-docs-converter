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

Next: [Dynamic Binding Tips](Dynamic-Binding-Tips.html), Up: [Variable Scoping](Variable-Scoping.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 12.10.1 Dynamic Binding

By default, the local variable bindings made by Emacs are dynamic bindings. When a variable is dynamically bound, its current binding at any point in the execution of the Lisp program is simply the most recently-created dynamic local binding for that symbol, or the global binding if there is no such local binding.

Dynamic bindings have dynamic scope and extent, as shown by the following example:

    (defvar x -99)  ; x receives an initial value of -99.

    (defun getx ()
      x)            ; x is used free in this function.

    (let ((x 1))    ; x is dynamically bound.
      (getx))
         ⇒ 1

    ;; After the let form finishes, x reverts to its
    ;; previous value, which is -99.

    (getx)
         ⇒ -99

The function `getx` refers to `x`. This is a *free* reference, in the sense that there is no binding for `x` within that `defun` construct itself. When we call `getx` from within a `let` form in which `x` is (dynamically) bound, it retrieves the local value (i.e., 1). But when we call `getx` outside the `let` form, it retrieves the global value (i.e., -99).

Here is another example, which illustrates setting a dynamically bound variable using `setq`:

    (defvar x -99)      ; x receives an initial value of -99.

    (defun addx ()
      (setq x (1+ x)))  ; Add 1 to x and return its new value.

    (let ((x 1))
      (addx)
      (addx))
         ⇒ 3           ; The two addx calls add to x twice.

    ;; After the let form finishes, x reverts to its
    ;; previous value, which is -99.

    (addx)
         ⇒ -98

Dynamic binding is implemented in Emacs Lisp in a simple way. Each symbol has a value cell, which specifies its current dynamic value (or absence of value). See [Symbol Components](Symbol-Components.html). When a symbol is given a dynamic local binding, Emacs records the contents of the value cell (or absence thereof) in a stack, and stores the new local value in the value cell. When the binding construct finishes executing, Emacs pops the old value off the stack, and puts it in the value cell.

Next: [Dynamic Binding Tips](Dynamic-Binding-Tips.html), Up: [Variable Scoping](Variable-Scoping.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]