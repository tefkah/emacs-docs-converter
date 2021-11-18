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

Next: [Condition Variables](Condition-Variables.html), Previous: [Basic Thread Functions](Basic-Thread-Functions.html), Up: [Threads](Threads.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 37.2 Mutexes

A *mutex* is an exclusive lock. At any moment, zero or one threads may own a mutex. If a thread attempts to acquire a mutex, and the mutex is already owned by some other thread, then the acquiring thread will block until the mutex becomes available.

Emacs Lisp mutexes are of a type called *recursive*, which means that a thread can re-acquire a mutex it owns any number of times. A mutex keeps a count of how many times it has been acquired, and each acquisition of a mutex must be paired with a release. The last release by a thread of a mutex reverts it to the unowned state, potentially allowing another thread to acquire the mutex.

*   Function: **mutexp** *object*

    This function returns `t` if `object` represents an Emacs mutex, `nil` otherwise.

<!---->

*   Function: **make-mutex** *\&optional name*

    Create a new mutex and return it. If `name` is specified, it is a name given to the mutex. It must be a string. The name is for debugging purposes only; it has no meaning to Emacs.

<!---->

*   Function: **mutex-name** *mutex*

    Return the name of `mutex`, as specified to `make-mutex`.

<!---->

*   Function: **mutex-lock** *mutex*

    This will block until this thread acquires `mutex`, or until this thread is signaled using `thread-signal`. If `mutex` is already owned by this thread, this simply returns.

<!---->

*   Function: **mutex-unlock** *mutex*

    Release `mutex`. If `mutex` is not owned by this thread, this will signal an error.

<!---->

*   Macro: **with-mutex** *mutex body…*

    This macro is the simplest and safest way to evaluate forms while holding a mutex. It acquires `mutex`, invokes `body`, and then releases `mutex`. It returns the result of `body`.

Next: [Condition Variables](Condition-Variables.html), Previous: [Basic Thread Functions](Basic-Thread-Functions.html), Up: [Threads](Threads.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]