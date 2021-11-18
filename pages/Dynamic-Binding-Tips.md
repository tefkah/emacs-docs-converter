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

Next: [Lexical Binding](Lexical-Binding.html), Previous: [Dynamic Binding](Dynamic-Binding.html), Up: [Variable Scoping](Variable-Scoping.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 12.10.2 Proper Use of Dynamic Binding

Dynamic binding is a powerful feature, as it allows programs to refer to variables that are not defined within their local textual scope. However, if used without restraint, this can also make programs hard to understand. There are two clean ways to use this technique:

*   If a variable has no global definition, use it as a local variable only within a binding construct, such as the body of the `let` form where the variable was bound. If this convention is followed consistently throughout a program, the value of the variable will not affect, nor be affected by, any uses of the same variable symbol elsewhere in the program.

*   Otherwise, define the variable with `defvar`, `defconst` (see [Defining Variables](Defining-Variables.html)), or `defcustom` (see [Variable Definitions](Variable-Definitions.html)). Usually, the definition should be at top-level in an Emacs Lisp file. As far as possible, it should include a documentation string which explains the meaning and purpose of the variable. You should also choose the variable’s name to avoid name conflicts (see [Coding Conventions](Coding-Conventions.html)).

    Then you can bind the variable anywhere in a program, knowing reliably what the effect will be. Wherever you encounter the variable, it will be easy to refer back to the definition, e.g., via the `C-h v` command (provided the variable definition has been loaded into Emacs). See [Name Help](https://www.gnu.org/software/emacs/manual/html_node/emacs/Name-Help.html#Name-Help) in The GNU Emacs Manual.

    For example, it is common to use local bindings for customizable variables like `case-fold-search`:

        (defun search-for-abc ()
          "Search for the string \"abc\", ignoring case differences."
          (let ((case-fold-search t))
            (re-search-forward "abc")))

Next: [Lexical Binding](Lexical-Binding.html), Previous: [Dynamic Binding](Dynamic-Binding.html), Up: [Variable Scoping](Variable-Scoping.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
