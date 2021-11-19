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

Next: [Backquote Patterns](Backquote-Patterns.html), Previous: [pcase Macro](pcase-Macro.html), Up: [Pattern-Matching Conditional](Pattern_002dMatching-Conditional.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 11.4.2 Extending `pcase`

The `pcase` macro supports several kinds of patterns (see [Pattern-Matching Conditional](Pattern_002dMatching-Conditional.html)). You can add support for other kinds of patterns using the `pcase-defmacro` macro.

*   Macro: **pcase-defmacro** *name args \[doc] \&rest body*

    Define a new kind of pattern for `pcase`, to be invoked as `(name actual-args)`<!-- /@w -->. The `pcase` macro expands this into a function call that evaluates `body`, whose job it is to rewrite the invoked pattern into some other pattern, in an environment where `args` are bound to `actual-args`.

    Additionally, arrange to display `doc` along with the docstring of `pcase`. By convention, `doc` should use `EXPVAL` to stand for the result of evaluating `expression` (first arg to `pcase`).

Typically, `body` rewrites the invoked pattern to use more basic patterns. Although all patterns eventually reduce to core patterns, `body` need not use core patterns straight away. The following example defines two patterns, named `less-than` and `integer-less-than`.

    (pcase-defmacro less-than (n)
      "Matches if EXPVAL is a number less than N."
      `(pred (> ,n)))

```
```

    (pcase-defmacro integer-less-than (n)
      "Matches if EXPVAL is an integer less than N."
      `(and (pred integerp)
            (less-than ,n)))

Note that the docstrings mention `args` (in this case, only one: `n`) in the usual way, and also mention `EXPVAL` by convention. The first rewrite (i.e., `body` for `less-than`) uses one core pattern: `pred`. The second uses two core patterns: `and` and `pred`, as well as the newly-defined pattern `less-than`. Both use a single backquote construct (see [Backquote](Backquote.html)).
