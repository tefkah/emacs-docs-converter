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

Next: [Adding Generalized Variables](Adding-Generalized-Variables.html), Up: [Generalized Variables](Generalized-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 12.17.1 The `setf` Macro

The `setf` macro is the most basic way to operate on generalized variables. The `setf` form is like `setq`, except that it accepts arbitrary place forms on the left side rather than just symbols. For example, `(setf (car a) b)` sets the car of `a` to `b`, doing the same operation as `(setcar a b)`, but without you having to use two separate functions for setting and accessing this type of place.

*   Macro: **setf** *\[place form]…*

    This macro evaluates `form` and stores it in `place`, which must be a valid generalized variable form. If there are several `place` and `form` pairs, the assignments are done sequentially just as with `setq`. `setf` returns the value of the last `form`.

The following Lisp forms are the forms in Emacs that will work as generalized variables, and so may appear in the `place` argument of `setf`:

*   A symbol. In other words, `(setf x y)` is exactly equivalent to `(setq x y)`, and `setq` itself is strictly speaking redundant given that `setf` exists. Most programmers will continue to prefer `setq` for setting simple variables, though, for stylistic and historical reasons. The macro `(setf x y)` actually expands to `(setq x y)`, so there is no performance penalty for using it in compiled code.

*   A call to any of the following standard Lisp functions:

        aref      cddr      symbol-function
        car       elt       symbol-plist
        caar      get       symbol-value
        cadr      gethash
        cdr       nth
        cdar      nthcdr

*   A call to any of the following Emacs-specific functions:

        alist-get                     process-get
        frame-parameter               process-sentinel
        terminal-parameter            window-buffer
        keymap-parent                 window-display-table
        match-data                    window-dedicated-p
        overlay-get                   window-hscroll
        overlay-start                 window-parameter
        overlay-end                   window-point
        process-buffer                window-start
        process-filter                default-value

`setf` signals an error if you pass a `place` form that it does not know how to handle.

Note that for `nthcdr`, the list argument of the function must itself be a valid `place` form. For example, `(setf (nthcdr 0 foo) 7)` will set `foo` itself to 7.

The macros `push` (see [List Variables](List-Variables.html)) and `pop` (see [List Elements](List-Elements.html)) can manipulate generalized variables, not just lists. `(pop place)` removes and returns the first element of the list stored in `place`. It is analogous to `(prog1 (car place) (setf place (cdr place)))`, except that it takes care to evaluate all subforms only once. `(push x place)` inserts `x` at the front of the list stored in `place`. It is analogous to `(setf place (cons x place))`, except for evaluation of the subforms. Note that `push` and `pop` on an `nthcdr` place can be used to insert or delete at any position in a list.

The `cl-lib` library defines various extensions for generalized variables, including additional `setf` places. See [Generalized Variables](https://www.gnu.org/software/emacs/manual/html_node/cl/Generalized-Variables.html#Generalized-Variables) in Common Lisp Extensions.

Next: [Adding Generalized Variables](Adding-Generalized-Variables.html), Up: [Generalized Variables](Generalized-Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]