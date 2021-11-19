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

Next: [Coverage Testing](Coverage-Testing.html), Previous: [Printing in Edebug](Printing-in-Edebug.html), Up: [Edebug](Edebug.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.12 Trace Buffer

Edebug can record an execution trace, storing it in a buffer named `*edebug-trace*`. This is a log of function calls and returns, showing the function names and their arguments and values. To enable trace recording, set `edebug-trace` to a non-`nil` value.

Making a trace buffer is not the same thing as using trace execution mode (see [Edebug Execution Modes](Edebug-Execution-Modes.html)).

When trace recording is enabled, each function entry and exit adds lines to the trace buffer. A function entry record consists of ‘`::::{`’, followed by the function name and argument values. A function exit record consists of ‘`::::}`’, followed by the function name and result of the function.

The number of ‘`:`’s in an entry shows its recursion depth. You can use the braces in the trace buffer to find the matching beginning or end of function calls.

You can customize trace recording for function entry and exit by redefining the functions `edebug-print-trace-before` and `edebug-print-trace-after`.

*   Macro: **edebug-tracing** *string body…*

    This macro requests additional trace information around the execution of the `body` forms. The argument `string` specifies text to put in the trace buffer, after the ‘`{`’ or ‘`}`’. All the arguments are evaluated, and `edebug-tracing` returns the value of the last form in `body`.

<!---->

*   Function: **edebug-trace** *format-string \&rest format-args*

    This function inserts text in the trace buffer. It computes the text with `(apply 'format format-string format-args)`. It also appends a newline to separate entries.

`edebug-tracing` and `edebug-trace` insert lines in the trace buffer whenever they are called, even if Edebug is not active. Adding text to the trace buffer also scrolls its window to show the last lines inserted.

Next: [Coverage Testing](Coverage-Testing.html), Previous: [Printing in Edebug](Printing-in-Edebug.html), Up: [Edebug](Edebug.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
