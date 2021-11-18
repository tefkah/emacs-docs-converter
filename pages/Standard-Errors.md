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

Next: [Standard Keymaps](Standard-Keymaps.html), Previous: [GNU Emacs Internals](GNU-Emacs-Internals.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## Appendix F Standard Errors

Here is a list of the more important error symbols in standard Emacs, grouped by concept. The list includes each symbol’s message and a cross reference to a description of how the error can occur.

Each error symbol has a set of parent error conditions that is a list of symbols. Normally this list includes the error symbol itself and the symbol `error`. Occasionally it includes additional symbols, which are intermediate classifications, narrower than `error` but broader than a single error symbol. For example, all the errors in accessing files have the condition `file-error`. If we do not say here that a certain error symbol has additional error conditions, that means it has none.

As a special exception, the error symbol `quit` does not have the condition `error`, because quitting is not considered an error.

Most of these error symbols are defined in C (mainly `data.c`), but some are defined in Lisp. For example, the file `userlock.el` defines the `file-locked` and `file-supersession` errors. Several of the specialized Lisp libraries distributed with Emacs define their own error symbols. We do not attempt to list of all those here.

See [Errors](Errors.html), for an explanation of how errors are generated and handled.

*   `error`

    The message is ‘`error`’. See [Errors](Errors.html).

*   `quit`

    The message is ‘`Quit`’. See [Quitting](Quitting.html).

*   `args-out-of-range`

    The message is ‘`Args out of range`’. This happens when trying to access an element beyond the range of a sequence, buffer, or other container-like object. See [Sequences Arrays Vectors](Sequences-Arrays-Vectors.html), and see [Text](Text.html).

*   `arith-error`

    The message is ‘`Arithmetic error`’. This occurs when trying to perform integer division by zero. See [Numeric Conversions](Numeric-Conversions.html), and see [Arithmetic Operations](Arithmetic-Operations.html).

*   `beginning-of-buffer`

    The message is ‘`Beginning of buffer`’. See [Character Motion](Character-Motion.html).

*   `buffer-read-only`

    The message is ‘`Buffer is read-only`’. See [Read Only Buffers](Read-Only-Buffers.html).

*   `circular-list`

    The message is ‘`List contains a loop`’. This happens when a circular structure is encountered. See [Circular Objects](Circular-Objects.html).

*   `cl-assertion-failed`

    The message is ‘`Assertion failed`’. This happens when the `cl-assert` macro fails a test. See [Assertions](https://www.gnu.org/software/emacs/manual/html_node/cl/Assertions.html#Assertions) in Common Lisp Extensions.

*   `coding-system-error`

    The message is ‘`Invalid coding system`’. See [Lisp and Coding Systems](Lisp-and-Coding-Systems.html).

*   `cyclic-function-indirection`

    The message is ‘`Symbol's chain of function indirections contains a loop`’. See [Function Indirection](Function-Indirection.html).

*   `cyclic-variable-indirection`

    The message is ‘`Symbol's chain of variable indirections contains a loop`’. See [Variable Aliases](Variable-Aliases.html).

*   `dbus-error`

    The message is ‘`D-Bus error`’. See [Errors and Events](../dbus/Errors-and-Events.html#Errors-and-Events) in D-Bus integration in Emacs.

*   `end-of-buffer`

    The message is ‘`End of buffer`’. See [Character Motion](Character-Motion.html).

*   `end-of-file`

    The message is ‘`End of file during parsing`’. Note that this is not a subcategory of `file-error`, because it pertains to the Lisp reader, not to file I/O. See [Input Functions](Input-Functions.html).

*   `file-already-exists`

    This is a subcategory of `file-error`. See [Writing to Files](Writing-to-Files.html).

*   `file-date-error`

    This is a subcategory of `file-error`. It occurs when `copy-file` tries and fails to set the last-modification time of the output file. See [Changing Files](Changing-Files.html).

*   `file-error`

    We do not list the error-strings of this error and its subcategories, because the error message is normally constructed from the data items alone when the error condition `file-error` is present. Thus, the error-strings are not very relevant. However, these error symbols do have `error-message` properties, and if no data is provided, the `error-message` property *is* used. See [Files](Files.html).

*   `file-missing`

    This is a subcategory of `file-error`. It occurs when an operation attempts to act on a file that is missing. See [Changing Files](Changing-Files.html).

*   `compression-error`

    This is a subcategory of `file-error`, which results from problems handling a compressed file. See [How Programs Do Loading](How-Programs-Do-Loading.html).

*   `file-locked`

    This is a subcategory of `file-error`. See [File Locks](File-Locks.html).

*   `file-supersession`

    This is a subcategory of `file-error`. See [Modification Time](Modification-Time.html).

*   `file-notify-error`

    This is a subcategory of `file-error`. It happens, when a file could not be watched for changes. See [File Notifications](File-Notifications.html).

*   `ftp-error`

    This is a subcategory of `file-error`, which results from problems in accessing a remote file using ftp. See [Remote Files](https://www.gnu.org/software/emacs/manual/html_node/emacs/Remote-Files.html#Remote-Files) in The GNU Emacs Manual.

*   `invalid-function`

    The message is ‘`Invalid function`’. See [Function Indirection](Function-Indirection.html).

*   `invalid-read-syntax`

    The message is usually ‘`Invalid read syntax`’. See [Printed Representation](Printed-Representation.html). This error can also be raised by commands like `eval-expression` when there’s text following an expression. In that case, the message is ‘`Trailing garbage following expression`’.

*   `invalid-regexp`

    The message is ‘`Invalid regexp`’. See [Regular Expressions](Regular-Expressions.html).

*   `mark-inactive`

    The message is ‘`The mark is not active now`’. See [The Mark](The-Mark.html).

*   `no-catch`

    The message is ‘`No catch for tag`’. See [Catch and Throw](Catch-and-Throw.html).

*   `range-error`

    The message is `Arithmetic range error`.

*   `overflow-error`

    The message is ‘`Arithmetic overflow error`’. This is a subcategory of `range-error`. This can happen with integers exceeding the `integer-width` limit. See [Integer Basics](Integer-Basics.html).

*   `scan-error`

    The message is ‘`Scan error`’. This happens when certain syntax-parsing functions find invalid syntax or mismatched parentheses. Conventionally raised with three argument: a human-readable error message, the start of the obstacle that cannot be moved over, and the end of the obstacle. See [List Motion](List-Motion.html), and see [Parsing Expressions](Parsing-Expressions.html).

*   `search-failed`

    The message is ‘`Search failed`’. See [Searching and Matching](Searching-and-Matching.html).

*   `setting-constant`

    The message is ‘`Attempt to set a constant symbol`’. This happens when attempting to assign values to `nil`, `t`, `most-positive-fixnum`, `most-negative-fixnum`, and keyword symbols. It also happens when attempting to assign values to `enable-multibyte-characters` and some other symbols whose direct assignment is not allowed for some reason. See [Constant Variables](Constant-Variables.html).

*   `text-read-only`

    The message is ‘`Text is read-only`’. This is a subcategory of `buffer-read-only`. See [Special Properties](Special-Properties.html).

*   `undefined-color`

    The message is ‘`Undefined color`’. See [Color Names](Color-Names.html).

*   `user-error`

    The message is the empty string. See [Signaling Errors](Signaling-Errors.html).

*   `user-search-failed`

    This is like ‘`search-failed`’, but doesn’t trigger the debugger, like ‘`user-error`’. See [Signaling Errors](Signaling-Errors.html), and see [Searching and Matching](Searching-and-Matching.html). This is used for searching in Info files, see [Search Text](../info/Search-Text.html#Search-Text) in Info.

*   `void-function`

    The message is ‘`Symbol's function definition is void`’. See [Function Cells](Function-Cells.html).

*   `void-variable`

    The message is ‘`Symbol's value as variable is void`’. See [Accessing Variables](Accessing-Variables.html).

*   `wrong-number-of-arguments`

    The message is ‘`Wrong number of arguments`’. See [Argument List](Argument-List.html).

*   `wrong-type-argument`

    The message is ‘`Wrong type argument`’. See [Type Predicates](Type-Predicates.html).

*   `unknown-image-type`

    The message is ‘`Cannot determine image type`’. See [Images](Images.html).

Next: [Standard Keymaps](Standard-Keymaps.html), Previous: [GNU Emacs Internals](GNU-Emacs-Internals.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
