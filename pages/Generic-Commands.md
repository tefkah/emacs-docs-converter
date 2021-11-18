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

Previous: [Interactive Examples](Interactive-Examples.html), Up: [Defining Commands](Defining-Commands.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.2.4 Select among Command Alternatives

The macro `define-alternatives` can be used to define *generic commands*. These are interactive functions whose implementation can be selected from several alternatives, as a matter of user preference.

*   Macro: **define-alternatives** *command \&rest customizations*

    Define the new command `command`, a symbol.

    When a user runs `M-x command RET` for the first time, Emacs prompts for which real form of the command to use, and records the selection by way of a custom variable. Using a prefix argument repeats this process of choosing an alternative.

    The variable `command-alternatives` should contain an alist with alternative implementations of `command`. Until this variable is set, `define-alternatives` has no effect.

    If `customizations` is non-`nil`, it should consist of alternating `defcustom` keywords (typically `:group` and `:version`) and values to add to the declaration of `command-alternatives`.
