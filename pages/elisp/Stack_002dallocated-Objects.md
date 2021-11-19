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

Next: [Memory Usage](Memory-Usage.html), Previous: [Garbage Collection](Garbage-Collection.html), Up: [GNU Emacs Internals](GNU-Emacs-Internals.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### E.4 Stack-allocated Objects

The garbage collector described above is used to manage data visible from Lisp programs, as well as most of the data internally used by the Lisp interpreter. Sometimes it may be useful to allocate temporary internal objects using the C stack of the interpreter. This can help performance, as stack allocation is typically faster than using heap memory to allocate and the garbage collector to free. The downside is that using such objects after they are freed results in undefined behavior, so uses should be well thought out and carefully debugged by using the `GC_CHECK_MARKED_OBJECTS` feature (see `src/alloc.c`). In particular, stack-allocated objects should never be made visible to user Lisp code.

Currently, cons cells and strings can be allocated this way. This is implemented by C macros like `AUTO_CONS` and `AUTO_STRING` that define a named `Lisp_Object` with block lifetime. These objects are not freed by the garbage collector; instead, they have automatic storage duration, i.e., they are allocated like local variables and are automatically freed at the end of execution of the C block that defined the object.

For performance reasons, stack-allocated strings are limited to ASCII characters, and many of these strings are immutable, i.e., calling `ASET` on them produces undefined behavior.
