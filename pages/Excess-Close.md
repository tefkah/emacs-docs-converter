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

Previous: [Excess Open](Excess-Open.html), Up: [Syntax Errors](Syntax-Errors.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.3.2 Excess Close Parentheses

To deal with an excess close parenthesis, first go to the beginning of the file, then type `C-u -1 C-M-u` (`backward-up-list` with an argument of -1) to find the end of the first unbalanced defun.

Then find the actual matching close parenthesis by typing `C-M-f` (`forward-sexp`, see [Expressions](https://www.gnu.org/software/emacs/manual/html_node/emacs/Expressions.html#Expressions) in The GNU Emacs Manual) at the beginning of that defun. This will leave you somewhere short of the place where the defun ought to end. It is possible that you will find a spurious close parenthesis in that vicinity.

If you don’t see a problem at that point, the next thing to do is to type `C-M-q` (`indent-pp-sexp`) at the beginning of the defun. A range of lines will probably shift left; if so, the missing open parenthesis or spurious close parenthesis is probably near the first of those lines. (However, don’t assume this is true; study the code to make sure.) Once you have found the discrepancy, undo the `C-M-q` with `C-_` (`undo`), since the old indentation is probably appropriate to the intended parentheses.

After you think you have fixed the problem, use `C-M-q` again. If the old indentation actually fits the intended nesting of parentheses, and you have put back those parentheses, `C-M-q` should not change anything.
