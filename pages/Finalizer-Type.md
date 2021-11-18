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

Previous: [Autoload Type](Autoload-Type.html), Up: [Programming Types](Programming-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.20 Finalizer Type

A *finalizer object* helps Lisp code clean up after objects that are no longer needed. A finalizer holds a Lisp function object. When a finalizer object becomes unreachable after a garbage collection pass, Emacs calls the finalizer’s associated function object. When deciding whether a finalizer is reachable, Emacs does not count references from finalizer objects themselves, allowing you to use finalizers without having to worry about accidentally capturing references to finalized objects themselves.

Errors in finalizers are printed to `*Messages*`. Emacs runs a given finalizer object’s associated function exactly once, even if that function fails.

*   Function: **make-finalizer** *function*

    Make a finalizer that will run `function`. `function` will be called after garbage collection when the returned finalizer object becomes unreachable. If the finalizer object is reachable only through references from finalizer objects, it does not count as reachable for the purpose of deciding whether to run `function`. `function` will be run once per finalizer object.
