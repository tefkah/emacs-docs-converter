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

Previous: [Module Misc](Module-Misc.html), Up: [Writing Dynamic Modules](Writing-Dynamic-Modules.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### E.8.5 Nonlocal Exits in Modules

Emacs Lisp supports nonlocal exits, whereby program control is transferred from one point in a program to another remote point. See [Nonlocal Exits](Nonlocal-Exits.html). Thus, Lisp functions called by your module might exit nonlocally by calling `signal` or `throw`, and your module functions must handle such nonlocal exits properly. Such handling is needed because C programs will not automatically release resources and perform other cleanups in these cases; your module code must itself do it. The module API provides facilities for that, described in this subsection. They are generally available since Emacs 25; those of them that became available in later releases explicitly call out the first Emacs version where they became part of the API.

When some Lisp code called by a module function signals an error or throws, the nonlocal exit is trapped, and the pending exit and its associated data are stored in the environment. Whenever a nonlocal exit is pending in the environment, any module API function called with a pointer to that environment will return immediately without any processing (the functions `non_local_exit_check`, `non_local_exit_get`, and `non_local_exit_clear` are exceptions from this rule). If your module function then does nothing and returns to Emacs, a pending nonlocal exit will cause Emacs to act on it: signal an error or throw to the corresponding `catch`.

So the simplest “handling” of nonlocal exits in module functions is to do nothing special and let the rest of your code to run as if nothing happened. However, this can cause two classes of problems:

*   \- Your module function might use uninitialized or undefined values, since API functions return immediately without producing the expected results.
*   \- Your module might leak resources, because it might not have the opportunity to release them.

Therefore, we recommend that your module functions check for nonlocal exit conditions and recover from them, using the functions described below.

*   Function: *enum* **emacs\_funcall\_exit** *non\_local\_exit\_check (emacs\_env \*`env`)*

    This function returns the kind of nonlocal exit condition stored in `env`. The possible values are:

    *   `emacs_funcall_exit_return`

        The last API function exited normally.

    *   `emacs_funcall_exit_signal`

        The last API function signaled an error.

    *   `emacs_funcall_exit_throw`

        The last API function exited via `throw`.

<!---->

*   Function: *enum* **emacs\_funcall\_exit** *non\_local\_exit\_get (emacs\_env \*`env`, emacs\_value \*`symbol`, emacs\_value \*`data`)*

    This function returns the kind of nonlocal exit condition stored in `env`, like `non_local_exit_check` does, but it also returns the full information about the nonlocal exit, if any. If the return value is `emacs_funcall_exit_signal`, the function stores the error symbol in `*symbol` and the error data in `*data` (see [Signaling Errors](Signaling-Errors.html)). If the return value is `emacs_funcall_exit_throw`, the function stores the `catch` tag symbol in `*symbol` and the `throw` value in `*data`. The function doesn’t store anything in memory pointed by these arguments when the return value is `emacs_funcall_exit_return`.

You should check nonlocal exit conditions where it matters: before you allocated some resource or after you allocated a resource that might need freeing, or where a failure means further processing is impossible or infeasible.

Once your module function detected that a nonlocal exit is pending, it can either return to Emacs (after performing the necessary local cleanup), or it can attempt to recover from the nonlocal exit. The following API functions will help with these tasks.

*   Function: *void* **non\_local\_exit\_clear** *(emacs\_env \*`env`)*

    This function clears the pending nonlocal exit conditions and data from `env`. After calling it, the module API functions will work normally. Use this function if your module function can recover from nonlocal exits of the Lisp functions it calls and continue, and also before calling any of the following two functions (or any other API functions, if you want them to perform their intended processing when a nonlocal exit is pending).

<!---->

*   Function: *void* **non\_local\_exit\_throw** *(emacs\_env \*`env`, emacs\_value `tag`, emacs\_value `value`)*

    This function throws to the Lisp `catch` symbol represented by `tag`, passing it `value` as the value to return. Your module function should in general return soon after calling this function. One use of this function is when you want to re-throw a non-local exit from one of the called API or Lisp functions.

<!---->

*   Function: *void* **non\_local\_exit\_signal** *(emacs\_env \*`env`, emacs\_value `error`, emacs\_value `data`)*

    This function signals the error represented by `error` with the specified error data `data`. The module function should return soon after calling this function. This function could be useful, e.g., for signaling errors from module functions to Emacs.

Previous: [Module Misc](Module-Misc.html), Up: [Writing Dynamic Modules](Writing-Dynamic-Modules.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
