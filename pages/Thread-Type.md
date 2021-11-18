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

Next: [Mutex Type](Mutex-Type.html), Previous: [Process Type](Process-Type.html), Up: [Editing Types](Editing-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.5.9 Thread Type

A *thread* in Emacs represents a separate thread of Emacs Lisp execution. It runs its own Lisp program, has its own current buffer, and can have subprocesses locked to it, i.e. subprocesses whose output only this thread can accept. See [Threads](Threads.html).

Thread objects have no read syntax. They print in hash notation, giving the name of the thread (if it has been given a name) or its address in core:

    (all-threads)
        ⇒ (#<thread 0176fc40>)
