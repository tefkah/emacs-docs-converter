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

Previous: [Backtracking](Backtracking.html), Up: [Edebug and Macros](Edebug-and-Macros.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.15.4 Specification Examples

It may be easier to understand Edebug specifications by studying the examples provided here.

A `let` special form has a sequence of bindings and a body. Each of the bindings is either a symbol or a sublist with a symbol and optional expression. In the specification below, notice the `gate` inside of the sublist to prevent backtracking once a sublist is found.

    (def-edebug-spec let
      ((&rest
        &or symbolp (gate symbolp &optional form))
       body))

Edebug uses the following specifications for `defun` and the associated argument list and `interactive` specifications. It is necessary to handle interactive forms specially since an expression argument is actually evaluated outside of the function body. (The specification for `defmacro` is very similar to that for `defun`, but allows for the `declare` statement.)

    (def-edebug-spec defun
      (&define name lambda-list
               [&optional stringp]   ; Match the doc string, if present.
               [&optional ("interactive" interactive)]
               def-body))

    (def-edebug-spec lambda-list
      (([&rest arg]
        [&optional ["&optional" arg &rest arg]]
        &optional ["&rest" arg]
        )))

    (def-edebug-spec interactive
      (&optional &or stringp def-form))    ; Notice: def-form

The specification for backquote below illustrates how to match dotted lists and use `nil` to terminate recursion. It also illustrates how components of a vector may be matched. (The actual specification defined by Edebug is a little different, and does not support dotted lists because doing so causes very deep recursion that could fail.)

    (def-edebug-spec \` (backquote-form))   ; Alias just for clarity.

    (def-edebug-spec backquote-form
      (&or ([&or "," ",@"] &or ("quote" backquote-form) form)
           (backquote-form . [&or nil backquote-form])
           (vector &rest backquote-form)
           sexp))
