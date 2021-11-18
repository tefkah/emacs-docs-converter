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

Previous: [Problems with Macros](Problems-with-Macros.html), Up: [Macros](Macros.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 14.6 Indenting Macros

Within a macro definition, you can use the `declare` form (see [Defining Macros](Defining-Macros.html)) to specify how `TAB` should indent calls to the macro. An indentation specification is written like this:

    (declare (indent indent-spec))

This results in the `lisp-indent-function` property being set on the macro name.

Here are the possibilities for `indent-spec`:

*   `nil`

    This is the same as no property—use the standard indentation pattern.

*   `defun`

    Handle this function like a ‘`def`’ construct: treat the second line as the start of a *body*.

*   an integer, `number`

    The first `number` arguments of the function are *distinguished* arguments; the rest are considered the body of the expression. A line in the expression is indented according to whether the first argument on it is distinguished or not. If the argument is part of the body, the line is indented `lisp-body-indent` more columns than the open-parenthesis starting the containing expression. If the argument is distinguished and is either the first or second argument, it is indented *twice* that many extra columns. If the argument is distinguished and not the first or second argument, the line uses the standard pattern.

*   a symbol, `symbol`

    `symbol` should be a function name; that function is called to calculate the indentation of a line within this expression. The function receives two arguments:

    *   `pos`

        The position at which the line being indented begins.

    *   `state`

        The value returned by `parse-partial-sexp` (a Lisp primitive for indentation and nesting computation) when it parses up to the beginning of this line.

    It should return either a number, which is the number of columns of indentation for that line, or a list whose car is such a number. The difference between returning a number and returning a list is that a number says that all following lines at the same nesting level should be indented just like this one; a list says that following lines might call for different indentations. This makes a difference when the indentation is being computed by `C-M-q`; if the value is a number, `C-M-q` need not recalculate indentation for the following lines until the end of the list.

Previous: [Problems with Macros](Problems-with-Macros.html), Up: [Macros](Macros.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
