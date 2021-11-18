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

Previous: [Autoload by Prefix](Autoload-by-Prefix.html), Up: [Autoload](Autoload.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 16.5.2 When to Use Autoload

Do not add an autoload comment unless it is really necessary. Autoloading code means it is always globally visible. Once an item is autoloaded, there is no compatible way to transition back to it not being autoloaded (after people become accustomed to being able to use it without an explicit load).

*   The most common items to autoload are the interactive entry points to a library. For example, if `python.el` is a library defining a major-mode for editing Python code, autoload the definition of the `python-mode` function, so that people can simply use `M-x python-mode` to load the library.
*   Variables usually don’t need to be autoloaded. An exception is if the variable on its own is generally useful without the whole defining library being loaded. (An example of this might be something like `find-exec-terminator`.)
*   Don’t autoload a user option just so that a user can set it.
*   Never add an autoload *comment* to silence a compiler warning in another file. In the file that produces the warning, use `(defvar foo)` to silence an undefined variable warning, and `declare-function` (see [Declaring Functions](Declaring-Functions.html)) to silence an undefined function warning; or require the relevant library; or use an explicit autoload *statement*.
