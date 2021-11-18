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

Previous: [Lexical Binding](Lexical-Binding.html), Up: [Variable Scoping](Variable-Scoping.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 12.10.4 Using Lexical Binding

When loading an Emacs Lisp file or evaluating a Lisp buffer, lexical binding is enabled if the buffer-local variable `lexical-binding` is non-`nil`:

*   Variable: **lexical-binding**

    If this buffer-local variable is non-`nil`, Emacs Lisp files and buffers are evaluated using lexical binding instead of dynamic binding. (However, special variables are still dynamically bound; see below.) If `nil`, dynamic binding is used for all local variables. This variable is typically set for a whole Emacs Lisp file, as a file local variable (see [File Local Variables](File-Local-Variables.html)). Note that unlike other such variables, this one must be set in the first line of a file.

When evaluating Emacs Lisp code directly using an `eval` call, lexical binding is enabled if the `lexical` argument to `eval` is non-`nil`. See [Eval](Eval.html).

Lexical binding is also enabled in Lisp Interaction and IELM mode, used in the `*scratch*` and `*ielm*` buffers, and also when evaluating expressions via `M-:` (`eval-expression`) and when processing the `--eval` command-line options of Emacs (see [Action Arguments](https://www.gnu.org/software/emacs/manual/html_node/emacs/Action-Arguments.html#Action-Arguments) in The GNU Emacs Manual) and `emacsclient` (see [emacsclient Options](https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html#emacsclient-Options) in The GNU Emacs Manual).

Even when lexical binding is enabled, certain variables will continue to be dynamically bound. These are called *special variables*. Every variable that has been defined with `defvar`, `defcustom` or `defconst` is a special variable (see [Defining Variables](Defining-Variables.html)). All other variables are subject to lexical binding.

Using `defvar` without a value, it is possible to bind a variable dynamically just in one file, or in just one part of a file while still binding it lexically elsewhere. For example:

    (let (_)
      (defvar x)      ; Let-bindings of x will be dynamic within this let.
      (let ((x -99))  ; This is a dynamic binding of x.
        (defun get-dynamic-x ()
          x)))

    (let ((x 'lexical)) ; This is a lexical binding of x.
      (defun get-lexical-x ()
        x))

    (let (_)
      (defvar x)
      (let ((x 'dynamic))
        (list (get-lexical-x)
              (get-dynamic-x))))
        ⇒ (lexical dynamic)

*   Function: **special-variable-p** *symbol*

    This function returns non-`nil` if `symbol` is a special variable (i.e., it has a `defvar`, `defcustom`, or `defconst` variable definition). Otherwise, the return value is `nil`.

    Note that since this is a function, it can only return non-`nil` for variables which are permanently special, but not for those that are only special in the current lexical scope.

The use of a special variable as a formal argument in a function is discouraged. Doing so gives rise to unspecified behavior when lexical binding mode is enabled (it may use lexical binding sometimes, and dynamic binding other times).

Converting an Emacs Lisp program to lexical binding is easy. First, add a file-local variable setting of `lexical-binding` to `t` in the header line of the Emacs Lisp source file (see [File Local Variables](File-Local-Variables.html)). Second, check that every variable in the program which needs to be dynamically bound has a variable definition, so that it is not inadvertently bound lexically.

A simple way to find out which variables need a variable definition is to byte-compile the source file. See [Byte Compilation](Byte-Compilation.html). If a non-special variable is used outside of a `let` form, the byte-compiler will warn about reference or assignment to a free variable. If a non-special variable is bound but not used within a `let` form, the byte-compiler will warn about an unused lexical variable. The byte-compiler will also issue a warning if you use a special variable as a function argument.

(To silence byte-compiler warnings about unused variables, just use a variable name that starts with an underscore. The byte-compiler interprets this as an indication that this is a variable known not to be used.)

Previous: [Lexical Binding](Lexical-Binding.html), Up: [Variable Scoping](Variable-Scoping.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
