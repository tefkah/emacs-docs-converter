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

Next: [Variable Scoping](Variable-Scoping.html), Previous: [Setting Variables](Setting-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 12.9 Running a function when a variable is changed.

It is sometimes useful to take some action when a variable changes its value. The *variable watchpoint* facility provides the means to do so. Some possible uses for this feature include keeping display in sync with variable settings, and invoking the debugger to track down unexpected changes to variables (see [Variable Debugging](Variable-Debugging.html)).

The following functions may be used to manipulate and query the watch functions for a variable.

*   Function: **add-variable-watcher** *symbol watch-function*

    This function arranges for `watch-function` to be called whenever `symbol` is modified. Modifications through aliases (see [Variable Aliases](Variable-Aliases.html)) will have the same effect.

    `watch-function` will be called, just before changing the value of `symbol`, with 4 arguments: `symbol`, `newval`, `operation`, and `where`. `symbol` is the variable being changed. `newval` is the value it will be changed to. (The old value is available to `watch-function` as the value of `symbol`, since it was not yet changed to `newval`.) `operation` is a symbol representing the kind of change, one of: `set`, `let`, `unlet`, `makunbound`, or `defvaralias`. `where` is a buffer if the buffer-local value of the variable is being changed, `nil` otherwise.

<!---->

*   Function: **remove-variable-watcher** *symbol watch-function*

    This function removes `watch-function` from `symbol`’s list of watchers.

<!---->

*   Function: **get-variable-watchers** *symbol*

    This function returns the list of `symbol`’s active watcher functions.

#### 12.9.1 Limitations

There are a couple of ways in which a variable could be modified (or at least appear to be modified) without triggering a watchpoint.

Since watchpoints are attached to symbols, modification to the objects contained within variables (e.g., by a list modification function see [Modifying Lists](Modifying-Lists.html)) is not caught by this mechanism.

Additionally, C code can modify the value of variables directly, bypassing the watchpoint mechanism.

A minor limitation of this feature, again because it targets symbols, is that only variables of dynamic scope may be watched. This poses little difficulty, since modifications to lexical variables can be discovered easily by inspecting the code within the scope of the variable (unlike dynamic variables, which can be modified by any code at all, see [Variable Scoping](Variable-Scoping.html)).

Next: [Variable Scoping](Variable-Scoping.html), Previous: [Setting Variables](Setting-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
