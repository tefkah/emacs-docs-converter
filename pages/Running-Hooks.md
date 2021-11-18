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

Next: [Setting Hooks](Setting-Hooks.html), Up: [Hooks](Hooks.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.1.1 Running Hooks

In this section, we document the `run-hooks` function, which is used to run a normal hook. We also document the functions for running various kinds of abnormal hooks.

*   Function: **run-hooks** *\&rest hookvars*

    This function takes one or more normal hook variable names as arguments, and runs each hook in turn. Each argument should be a symbol that is a normal hook variable. These arguments are processed in the order specified.

    If a hook variable has a non-`nil` value, that value should be a list of functions. `run-hooks` calls all the functions, one by one, with no arguments.

    The hook variable’s value can also be a single function—either a lambda expression or a symbol with a function definition—which `run-hooks` calls. But this usage is obsolete.

    If the hook variable is buffer-local, the buffer-local variable will be used instead of the global variable. However, if the buffer-local variable contains the element `t`, the global hook variable will be run as well.

<!---->

*   Function: **run-hook-with-args** *hook \&rest args*

    This function runs an abnormal hook by calling all the hook functions in `hook`, passing each one the arguments `args`.

<!---->

*   Function: **run-hook-with-args-until-failure** *hook \&rest args*

    This function runs an abnormal hook by calling each hook function in turn, stopping if one of them fails by returning `nil`. Each hook function is passed the arguments `args`. If this function stops because one of the hook functions fails, it returns `nil`; otherwise it returns a non-`nil` value.

<!---->

*   Function: **run-hook-with-args-until-success** *hook \&rest args*

    This function runs an abnormal hook by calling each hook function, stopping if one of them succeeds by returning a non-`nil` value. Each hook function is passed the arguments `args`. If this function stops because one of the hook functions returns a non-`nil` value, it returns that value; otherwise it returns `nil`.

Next: [Setting Hooks](Setting-Hooks.html), Up: [Hooks](Hooks.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
