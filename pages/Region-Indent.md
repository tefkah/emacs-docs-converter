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

Next: [Relative Indent](Relative-Indent.html), Previous: [Mode-Specific Indent](Mode_002dSpecific-Indent.html), Up: [Indentation](Indentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.17.3 Indenting an Entire Region

This section describes commands that indent all the lines in the region. They return unpredictable values.

*   Command: **indent-region** *start end \&optional to-column*

    This command indents each nonblank line starting between `start` (inclusive) and `end` (exclusive). If `to-column` is `nil`, `indent-region` indents each nonblank line by calling the current mode’s indentation function, the value of `indent-line-function`.

    If `to-column` is non-`nil`, it should be an integer specifying the number of columns of indentation; then this function gives each line exactly that much indentation, by either adding or deleting whitespace.

    If there is a fill prefix, `indent-region` indents each line by making it start with the fill prefix.

<!---->

*   Variable: **indent-region-function**

    The value of this variable is a function that can be used by `indent-region` as a short cut. It should take two arguments, the start and end of the region. You should design the function so that it will produce the same results as indenting the lines of the region one by one, but presumably faster.

    If the value is `nil`, there is no short cut, and `indent-region` actually works line by line.

    A short-cut function is useful in modes such as C mode and Lisp mode, where the `indent-line-function` must scan from the beginning of the function definition: applying it to each line would be quadratic in time. The short cut can update the scan information as it moves through the lines indenting them; this takes linear time. In a mode where indenting a line individually is fast, there is no need for a short cut.

    `indent-region` with a non-`nil` argument `to-column` has a different meaning and does not use this variable.

<!---->

*   Command: **indent-rigidly** *start end count*

    This function indents all lines starting between `start` (inclusive) and `end` (exclusive) sideways by `count` columns. This preserves the shape of the affected region, moving it as a rigid unit.

    This is useful not only for indenting regions of unindented text, but also for indenting regions of formatted code. For example, if `count` is 3, this command adds 3 columns of indentation to every line that begins in the specified region.

    If called interactively with no prefix argument, this command invokes a transient mode for adjusting indentation rigidly. See [Indentation Commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Indentation-Commands.html#Indentation-Commands) in The GNU Emacs Manual.

<!---->

*   Command: **indent-code-rigidly** *start end columns \&optional nochange-regexp*

    This is like `indent-rigidly`, except that it doesn’t alter lines that start within strings or comments.

    In addition, it doesn’t alter a line if `nochange-regexp` matches at the beginning of the line (if `nochange-regexp` is non-`nil`).

Next: [Relative Indent](Relative-Indent.html), Previous: [Mode-Specific Indent](Mode_002dSpecific-Indent.html), Up: [Indentation](Indentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
