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

Next: [Syntactic Font Lock](Syntactic-Font-Lock.html), Previous: [Precalculated Fontification](Precalculated-Fontification.html), Up: [Font Lock Mode](Font-Lock-Mode.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.6.7 Faces for Font Lock

Font Lock mode can highlight using any face, but Emacs defines several faces specifically for Font Lock to use to highlight text. These *Font Lock faces* are listed below. They can also be used by major modes for syntactic highlighting outside of Font Lock mode (see [Major Mode Conventions](Major-Mode-Conventions.html)).

Each of these symbols is both a face name, and a variable whose default value is the symbol itself. Thus, the default value of `font-lock-comment-face` is `font-lock-comment-face`.

The faces are listed with descriptions of their typical usage, and in order of greater to lesser prominence. If a mode’s syntactic categories do not fit well with the usage descriptions, the faces can be assigned using the ordering as a guide.

*   `font-lock-warning-face`

    for a construct that is peculiar (e.g., an unescaped confusable quote in an Emacs Lisp symbol like ‘`‘foo`’), or that greatly changes the meaning of other text, like ‘`;;;###autoload`’ in Emacs Lisp and ‘`#error`’ in C.

*   `font-lock-function-name-face`

    for the name of a function being defined or declared.

*   `font-lock-variable-name-face`

    for the name of a variable being defined or declared.

*   `font-lock-keyword-face`

    for a keyword with special syntactic significance, like ‘`for`’ and ‘`if`’ in C.

*   `font-lock-comment-face`

    for comments.

*   `font-lock-comment-delimiter-face`

    for comments delimiters, like ‘`/*`’ and ‘`*/`’ in C. On most terminals, this inherits from `font-lock-comment-face`.

*   `font-lock-type-face`

    for the names of user-defined data types.

*   `font-lock-constant-face`

    for the names of constants, like ‘`NULL`’ in C.

*   `font-lock-builtin-face`

    for the names of built-in functions.

*   `font-lock-preprocessor-face`

    for preprocessor commands. This inherits, by default, from `font-lock-builtin-face`.

*   `font-lock-string-face`

    for string constants.

*   `font-lock-doc-face`

    for documentation strings in the code. This inherits, by default, from `font-lock-string-face`.

*   `font-lock-negation-char-face`

    for easily-overlooked negation characters.

Next: [Syntactic Font Lock](Syntactic-Font-Lock.html), Previous: [Precalculated Fontification](Precalculated-Fontification.html), Up: [Font Lock Mode](Font-Lock-Mode.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
