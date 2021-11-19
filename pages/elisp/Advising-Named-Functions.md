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

Next: [Advice Combinators](Advice-Combinators.html), Previous: [Core Advising Primitives](Core-Advising-Primitives.html), Up: [Advising Functions](Advising-Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 13.11.2 Advising Named Functions

A common use of advice is for named functions and macros. You could just use `add-function` as in:

    (add-function :around (symbol-function 'fun) #'his-tracing-function)

But you should use `advice-add` and `advice-remove` for that instead. This separate set of functions to manipulate pieces of advice applied to named functions, offers the following extra features compared to `add-function`: they know how to deal with macros and autoloaded functions, they let `describe-function` preserve the original docstring as well as document the added advice, and they let you add and remove advice before a function is even defined.

`advice-add` can be useful for altering the behavior of existing calls to an existing function without having to redefine the whole function. However, it can be a source of bugs, since existing callers to the function may assume the old behavior, and work incorrectly when the behavior is changed by advice. Advice can also cause confusion in debugging, if the person doing the debugging does not notice or remember that the function has been modified by advice.

For these reasons, advice should be reserved for the cases where you cannot modify a function’s behavior in any other way. If it is possible to do the same thing via a hook, that is preferable (see [Hooks](Hooks.html)). If you simply want to change what a particular key does, it may be better to write a new command, and remap the old command’s key bindings to the new one (see [Remapping Commands](Remapping-Commands.html)).

If you are writing code for release, for others to use, try to avoid including advice in it. If the function you want to advise has no hook to do the job, please talk with the Emacs developers about adding a suitable hook. Especially, Emacs’s own source files should not put advice on functions in Emacs. (There are currently a few exceptions to this convention, but we aim to correct them.) It is generally cleaner to create a new hook in `foo`, and make `bar` use the hook, than to have `bar` put advice in `foo`.

Special forms (see [Special Forms](Special-Forms.html)) cannot be advised, however macros can be advised, in much the same way as functions. Of course, this will not affect code that has already been macro-expanded, so you need to make sure the advice is installed before the macro is expanded.

It is possible to advise a primitive (see [What Is a Function](What-Is-a-Function.html)), but one should typically *not* do so, for two reasons. Firstly, some primitives are used by the advice mechanism, and advising them could cause an infinite recursion. Secondly, many primitives are called directly from C, and such calls ignore advice; hence, one ends up in a confusing situation where some calls (occurring from Lisp code) obey the advice and other calls (from C code) do not.

*   Macro: **define-advice** *symbol (where lambda-list \&optional name depth) \&rest body*

    This macro defines a piece of advice and adds it to the function named `symbol`. The advice is an anonymous function if `name` is `nil` or a function named `symbol@name`. See `advice-add` for explanation of other arguments.

<!---->

*   Function: **advice-add** *symbol where function \&optional props*

    Add the advice `function` to the named function `symbol`. `where` and `props` have the same meaning as for `add-function` (see [Core Advising Primitives](Core-Advising-Primitives.html)).

<!---->

*   Function: **advice-remove** *symbol function*

    Remove the advice `function` from the named function `symbol`. `function` can also be the `name` of a piece of advice.

<!---->

*   Function: **advice-member-p** *function symbol*

    Return non-`nil` if the advice `function` is already in the named function `symbol`. `function` can also be the `name` of a piece of advice.

<!---->

*   Function: **advice-mapc** *function symbol*

    Call `function` for every piece of advice that was added to the named function `symbol`. `function` is called with two arguments: the advice function and its properties.

Next: [Advice Combinators](Advice-Combinators.html), Previous: [Core Advising Primitives](Core-Advising-Primitives.html), Up: [Advising Functions](Advising-Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
