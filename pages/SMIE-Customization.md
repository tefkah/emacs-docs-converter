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

Previous: [SMIE Indentation Example](SMIE-Indentation-Example.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.7.1.9 Customizing Indentation

If you are using a mode whose indentation is provided by SMIE, you can customize the indentation to suit your preferences. You can do this on a per-mode basis (using the option `smie-config`), or a per-file basis (using the function `smie-config-local` in a file-local variable specification).

*   User Option: **smie-config**

    This option lets you customize indentation on a per-mode basis. It is an alist with elements of the form `(mode . rules)`. For the precise form of rules, see the variable’s documentation; but you may find it easier to use the command `smie-config-guess`.

<!---->

*   Command: **smie-config-guess**

    This command tries to work out appropriate settings to produce your preferred style of indentation. Simply call the command while visiting a file that is indented with your style.

<!---->

*   Command: **smie-config-save**

    Call this command after using `smie-config-guess`, to save your settings for future sessions.

<!---->

*   Command: **smie-config-show-indent** *\&optional move*

    This command displays the rules that are used to indent the current line.

<!---->

*   Command: **smie-config-set-indent**

    This command adds a local rule to adjust the indentation of the current line.

<!---->

*   Function: **smie-config-local** *rules*

    This function adds `rules` as indentation rules for the current buffer. These add to any mode-specific rules defined by the `smie-config` option. To specify custom indentation rules for a specific file, add an entry to the file’s local variables of the form: `eval: (smie-config-local '(rules))`.
