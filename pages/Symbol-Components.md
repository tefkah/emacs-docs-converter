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

Next: [Definitions](Definitions.html), Up: [Symbols](Symbols.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 9.1 Symbol Components

Each symbol has four components (or “cells”), each of which references another object:

*   Print name

    The symbol’s name.

*   Value

    The symbol’s current value as a variable.

*   Function

    The symbol’s function definition. It can also hold a symbol, a keymap, or a keyboard macro.

*   Property list

    The symbol’s property list.

The print name cell always holds a string, and cannot be changed. Each of the other three cells can be set to any Lisp object.

The print name cell holds the string that is the name of a symbol. Since symbols are represented textually by their names, it is important not to have two symbols with the same name. The Lisp reader ensures this: every time it reads a symbol, it looks for an existing symbol with the specified name before it creates a new one. To get a symbol’s name, use the function `symbol-name` (see [Creating Symbols](Creating-Symbols.html)).

The value cell holds a symbol’s value as a variable, which is what you get if the symbol itself is evaluated as a Lisp expression. See [Variables](Variables.html), for details about how values are set and retrieved, including complications such as *local bindings* and *scoping rules*. Most symbols can have any Lisp object as a value, but certain special symbols have values that cannot be changed; these include `nil` and `t`, and any symbol whose name starts with ‘`:`’ (those are called *keywords*). See [Constant Variables](Constant-Variables.html).

The function cell holds a symbol’s function definition. Often, we refer to “the function `foo`” when we really mean the function stored in the function cell of `foo`; we make the distinction explicit only when necessary. Typically, the function cell is used to hold a function (see [Functions](Functions.html)) or a macro (see [Macros](Macros.html)). However, it can also be used to hold a symbol (see [Function Indirection](Function-Indirection.html)), keyboard macro (see [Keyboard Macros](Keyboard-Macros.html)), keymap (see [Keymaps](Keymaps.html)), or autoload object (see [Autoloading](Autoloading.html)). To get the contents of a symbol’s function cell, use the function `symbol-function` (see [Function Cells](Function-Cells.html)).

The property list cell normally should hold a correctly formatted property list. To get a symbol’s property list, use the function `symbol-plist`. See [Symbol Properties](Symbol-Properties.html).

The function cell or the value cell may be *void*, which means that the cell does not reference any object. (This is not the same thing as holding the symbol `void`, nor the same as holding the symbol `nil`.) Examining a function or value cell that is void results in an error, such as ‘`Symbol's value as variable is void`’.

Because each symbol has separate value and function cells, variables names and function names do not conflict. For example, the symbol `buffer-file-name` has a value (the name of the file being visited in the current buffer) as well as a function definition (a primitive function that returns the name of the file):

    buffer-file-name
         ⇒ "/gnu/elisp/symbols.texi"
    (symbol-function 'buffer-file-name)
         ⇒ #<subr buffer-file-name>

Next: [Definitions](Definitions.html), Up: [Symbols](Symbols.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
