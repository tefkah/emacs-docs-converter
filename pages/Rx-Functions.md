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

Next: [Extending Rx](Extending-Rx.html), Previous: [Rx Constructs](Rx-Constructs.html), Up: [Rx Notation](Rx-Notation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.3.3.2 Functions and macros using `rx` regexps

*   Macro: **rx** *rx-expr…*

    Translate the `rx-expr`s to a string regexp, as if they were the body of a `(seq …)` form. The `rx` macro expands to a string constant, or, if `literal` or `regexp` forms are used, a Lisp expression that evaluates to a string.

<!---->

*   Function: **rx-to-string** *rx-expr \&optional no-group*

    Translate `rx-expr` to a string regexp which is returned. If `no-group` is absent or nil, bracket the result in a non-capturing group, ‘`\(?:…\)`’, if necessary to ensure that a postfix operator appended to it will apply to the whole expression.

    Arguments to `literal` and `regexp` forms in `rx-expr` must be string literals.

The `pcase` macro can use `rx` expressions as patterns directly; see [rx in pcase](pcase-Macro.html#rx-in-pcase).

For mechanisms to add user-defined extensions to the `rx` notation, see [Extending Rx](Extending-Rx.html).
