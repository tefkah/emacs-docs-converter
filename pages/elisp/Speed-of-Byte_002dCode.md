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

Next: [Compilation Functions](Compilation-Functions.html), Up: [Byte Compilation](Byte-Compilation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 17.1 Performance of Byte-Compiled Code

A byte-compiled function is not as efficient as a primitive function written in C, but runs much faster than the version written in Lisp. Here is an example:

    (defun silly-loop (n)
      "Return the time, in seconds, to run N iterations of a loop."
      (let ((t1 (float-time)))
        (while (> (setq n (1- n)) 0))
        (- (float-time) t1)))
    ⇒ silly-loop

```
```

    (silly-loop 50000000)
    ⇒ 10.235304117202759

```
```

    (byte-compile 'silly-loop)
    ⇒ [Compiled code not shown]

```
```

    (silly-loop 50000000)
    ⇒ 3.705854892730713

In this example, the interpreted code required 10 seconds to run, whereas the byte-compiled code required less than 4 seconds. These results are representative, but actual results may vary.
