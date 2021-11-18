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

Next: [Yanking](Yanking.html), Previous: [Kill Ring Concepts](Kill-Ring-Concepts.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.8.2 Functions for Killing

`kill-region` is the usual subroutine for killing text. Any command that calls this function is a kill command (and should probably have ‘`kill`’ in its name). `kill-region` puts the newly killed text in a new element at the beginning of the kill ring or adds it to the most recent element. It determines automatically (using `last-command`) whether the previous command was a kill command, and if so appends the killed text to the most recent entry.

The commands described below can filter the killed text before they save it in the kill ring. They call `filter-buffer-substring` (see [Buffer Contents](Buffer-Contents.html)) to perform the filtering. By default, there’s no filtering, but major and minor modes and hook functions can set up filtering, so that text saved in the kill ring is different from what was in the buffer.

*   Command: **kill-region** *start end \&optional region*

    This function kills the stretch of text between `start` and `end`; but if the optional argument `region` is non-`nil`, it ignores `start` and `end`, and kills the text in the current region instead. The text is deleted but saved in the kill ring, along with its text properties. The value is always `nil`.

    In an interactive call, `start` and `end` are point and the mark, and `region` is always non-`nil`, so the command always kills the text in the current region.

    If the buffer or text is read-only, `kill-region` modifies the kill ring just the same, then signals an error without modifying the buffer. This is convenient because it lets the user use a series of kill commands to copy text from a read-only buffer into the kill ring.

<!---->

*   User Option: **kill-read-only-ok**

    If this option is non-`nil`, `kill-region` does not signal an error if the buffer or text is read-only. Instead, it simply returns, updating the kill ring but not changing the buffer.

<!---->

*   Command: **copy-region-as-kill** *start end \&optional region*

    This function saves the stretch of text between `start` and `end` on the kill ring (including text properties), but does not delete the text from the buffer. However, if the optional argument `region` is non-`nil`, the function ignores `start` and `end`, and saves the current region instead. It always returns `nil`.

    In an interactive call, `start` and `end` are point and the mark, and `region` is always non-`nil`, so the command always saves the text in the current region.

    The command does not set `this-command` to `kill-region`, so a subsequent kill command does not append to the same kill ring entry.

Next: [Yanking](Yanking.html), Previous: [Kill Ring Concepts](Kill-Ring-Concepts.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
