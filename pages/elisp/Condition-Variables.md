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

Next: [The Thread List](The-Thread-List.html), Previous: [Mutexes](Mutexes.html), Up: [Threads](Threads.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 37.3 Condition Variables

A *condition variable* is a way for a thread to block until some event occurs. A thread can wait on a condition variable, to be woken up when some other thread notifies the condition.

A condition variable is associated with a mutex and, conceptually, with some condition. For proper operation, the mutex must be acquired, and then a waiting thread must loop, testing the condition and waiting on the condition variable. For example:

    (with-mutex mutex
      (while (not global-variable)
        (condition-wait cond-var)))

The mutex ensures atomicity, and the loop is for robustness—there may be spurious notifications.

Similarly, the mutex must be held before notifying the condition. The typical, and best, approach is to acquire the mutex, make the changes associated with this condition, and then notify it:

    (with-mutex mutex
      (setq global-variable (some-computation))
      (condition-notify cond-var))

*   Function: **make-condition-variable** *mutex \&optional name*

    Make a new condition variable associated with `mutex`. If `name` is specified, it is a name given to the condition variable. It must be a string. The name is for debugging purposes only; it has no meaning to Emacs.

<!---->

*   Function: **condition-variable-p** *object*

    This function returns `t` if `object` represents a condition variable, `nil` otherwise.

<!---->

*   Function: **condition-wait** *cond*

    Wait for another thread to notify `cond`, a condition variable. This function will block until the condition is notified, or until a signal is delivered to this thread using `thread-signal`.

    It is an error to call `condition-wait` without holding the condition’s associated mutex.

    `condition-wait` releases the associated mutex while waiting. This allows other threads to acquire the mutex in order to notify the condition.

<!---->

*   Function: **condition-notify** *cond \&optional all*

    Notify `cond`. The mutex with `cond` must be held before calling this. Ordinarily a single waiting thread is woken by `condition-notify`; but if `all` is not `nil`, then all threads waiting on `cond` are notified.

    `condition-notify` releases the associated mutex while waiting. This allows other threads to acquire the mutex in order to wait on the condition.

<!---->

*   Function: **condition-name** *cond*

    Return the name of `cond`, as passed to `make-condition-variable`.

<!---->

*   Function: **condition-mutex** *cond*

    Return the mutex associated with `cond`. Note that the associated mutex cannot be changed.

Next: [The Thread List](The-Thread-List.html), Previous: [Mutexes](Mutexes.html), Up: [Threads](Threads.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
