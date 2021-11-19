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

Next: [Compiler Errors](Compiler-Errors.html), Previous: [Dynamic Loading](Dynamic-Loading.html), Up: [Byte Compilation](Byte-Compilation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 17.5 Evaluation During Compilation

These features permit you to write code to be evaluated during compilation of a program.

*   Special Form: **eval-and-compile** *body…*

    This form marks `body` to be evaluated both when you compile the containing code and when you run it (whether compiled or not).

    You can get a similar result by putting `body` in a separate file and referring to that file with `require`. That method is preferable when `body` is large. Effectively `require` is automatically `eval-and-compile`, the package is loaded both when compiling and executing.

    `autoload` is also effectively `eval-and-compile` too. It’s recognized when compiling, so uses of such a function don’t produce “not known to be defined” warnings.

    Most uses of `eval-and-compile` are fairly sophisticated.

    If a macro has a helper function to build its result, and that macro is used both locally and outside the package, then `eval-and-compile` should be used to get the helper both when compiling and then later when running.

    If functions are defined programmatically (with `fset` say), then `eval-and-compile` can be used to have that done at compile-time as well as run-time, so calls to those functions are checked (and warnings about “not known to be defined” suppressed).

<!---->

*   Special Form: **eval-when-compile** *body…*

    This form marks `body` to be evaluated at compile time but not when the compiled program is loaded. The result of evaluation by the compiler becomes a constant which appears in the compiled program. If you load the source file, rather than compiling it, `body` is evaluated normally.

    If you have a constant that needs some calculation to produce, `eval-when-compile` can do that at compile-time. For example,

        (defvar my-regexp
          (eval-when-compile (regexp-opt '("aaa" "aba" "abb"))))

    If you’re using another package, but only need macros from it (the byte compiler will expand those), then `eval-when-compile` can be used to load it for compiling, but not executing. For example,

        (eval-when-compile
          (require 'my-macro-package))

    The same sort of thing goes for macros and `defsubst` functions defined locally and only for use within the file. They are needed for compiling the file, but in most cases they are not needed for execution of the compiled file. For example,

        (eval-when-compile
          (unless (fboundp 'some-new-thing)
            (defmacro 'some-new-thing ()
              (compatibility code))))

    This is often good for code that’s only a fallback for compatibility with other versions of Emacs.

    **Common Lisp Note:** At top level, `eval-when-compile` is analogous to the Common Lisp idiom `(eval-when (compile eval) …)`. Elsewhere, the Common Lisp ‘`#.`’ reader macro (but not when interpreting) is closer to what `eval-when-compile` does.

Next: [Compiler Errors](Compiler-Errors.html), Previous: [Dynamic Loading](Dynamic-Loading.html), Up: [Byte Compilation](Byte-Compilation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
