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

Next: [Time Calculations](Time-Calculations.html), Previous: [Time Parsing](Time-Parsing.html), Up: [System Interface](System-Interface.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 40.9 Processor Run time

Emacs provides several functions and primitives that return time, both elapsed and processor time, used by the Emacs process.

*   Command: **emacs-uptime** *\&optional format*

    This function returns a string representing the Emacs *uptime*—the elapsed wall-clock time this instance of Emacs is running. The string is formatted by `format-seconds` according to the optional argument `format`. For the available format descriptors, see [format-seconds](Time-Parsing.html). If `format` is `nil` or omitted, it defaults to `"%Y, %D, %H, %M, %z%S"`.

    When called interactively, it prints the uptime in the echo area.

<!---->

*   Function: **get-internal-run-time**

    This function returns the processor run time used by Emacs, as a Lisp timestamp (see [Time of Day](Time-of-Day.html)).

    Note that the time returned by this function excludes the time Emacs was not using the processor, and if the Emacs process has several threads, the returned value is the sum of the processor times used up by all Emacs threads.

    If the system doesn’t provide a way to determine the processor run time, `get-internal-run-time` returns the same time as `current-time`.

<!---->

*   Command: **emacs-init-time**

    This function returns the duration of the Emacs initialization (see [Startup Summary](Startup-Summary.html)) in seconds, as a string. When called interactively, it prints the duration in the echo area.
