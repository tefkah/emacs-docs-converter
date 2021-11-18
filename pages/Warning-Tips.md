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

Next: [Documentation Tips](Documentation-Tips.html), Previous: [Compilation Tips](Compilation-Tips.html), Up: [Tips](Tips.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### D.5 Tips for Avoiding Compiler Warnings

*   Try to avoid compiler warnings about undefined free variables, by adding dummy `defvar` definitions for these variables, like this:

        (defvar foo)

    Such a definition has no effect except to tell the compiler not to warn about uses of the variable `foo` in this file.

*   Similarly, to avoid a compiler warning about an undefined function that you know *will* be defined, use a `declare-function` statement (see [Declaring Functions](Declaring-Functions.html)).

*   If you use many functions, macros, and variables from a certain file, you can add a `require` (see [require](Named-Features.html)) for that package to avoid compilation warnings for them, like this:

        (require 'foo)

    If you need only macros from some file, you can require it only at compile time (see [Eval During Compile](Eval-During-Compile.html)). For instance,

        (eval-when-compile
          (require 'foo))

*   If you bind a variable in one function, and use it or set it in another function, the compiler warns about the latter function unless the variable has a definition. But adding a definition would be unclean if the variable has a short name, since Lisp packages should not define short variable names. The right thing to do is to rename this variable to start with the name prefix used for the other functions and variables in your package.

*   The last resort for avoiding a warning, when you want to do something that is usually a mistake but you know is not a mistake in your usage, is to put it inside `with-no-warnings`. See [Compiler Errors](Compiler-Errors.html).

Next: [Documentation Tips](Documentation-Tips.html), Previous: [Compilation Tips](Compilation-Tips.html), Up: [Tips](Tips.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
