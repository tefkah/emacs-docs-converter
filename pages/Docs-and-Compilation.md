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

Next: [Dynamic Loading](Dynamic-Loading.html), Previous: [Compilation Functions](Compilation-Functions.html), Up: [Byte Compilation](Byte-Compilation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 17.3 Documentation Strings and Compilation

When Emacs loads functions and variables from a byte-compiled file, it normally does not load their documentation strings into memory. Each documentation string is dynamically loaded from the byte-compiled file only when needed. This saves memory, and speeds up loading by skipping the processing of the documentation strings.

This feature has a drawback: if you delete, move, or alter the compiled file (such as by compiling a new version), Emacs may no longer be able to access the documentation string of previously-loaded functions or variables. Such a problem normally only occurs if you build Emacs yourself, and happen to edit and/or recompile the Lisp source files. To solve it, just reload each file after recompilation.

Dynamic loading of documentation strings from byte-compiled files is determined, at compile time, for each byte-compiled file. It can be disabled via the option `byte-compile-dynamic-docstrings`.

*   User Option: **byte-compile-dynamic-docstrings**

    If this is non-`nil`, the byte compiler generates compiled files that are set up for dynamic loading of documentation strings.

    To disable the dynamic loading feature for a specific file, set this option to `nil` in its header line (see [Local Variables in Files](https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html#File-Variables) in The GNU Emacs Manual), like this:

        -*-byte-compile-dynamic-docstrings: nil;-*-

    This is useful mainly if you expect to change the file, and you want Emacs sessions that have already loaded it to keep working when the file changes.

Internally, the dynamic loading of documentation strings is accomplished by writing compiled files with a special Lisp reader construct, ‘`#@count`’. This construct skips the next `count` characters. It also uses the ‘`#$`’ construct, which stands for the name of this file, as a string. Do not use these constructs in Lisp source files; they are not designed to be clear to humans reading the file.

Next: [Dynamic Loading](Dynamic-Loading.html), Previous: [Compilation Functions](Compilation-Functions.html), Up: [Byte Compilation](Byte-Compilation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
