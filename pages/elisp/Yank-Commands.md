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

Next: [Low-Level Kill Ring](Low_002dLevel-Kill-Ring.html), Previous: [Yanking](Yanking.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.8.4 Functions for Yanking

This section describes higher-level commands for yanking, which are intended primarily for the user but useful also in Lisp programs. Both `yank` and `yank-pop` honor the `yank-excluded-properties` variable and `yank-handler` text property (see [Yanking](Yanking.html)).

*   Command: **yank** *\&optional arg*

    This command inserts before point the text at the front of the kill ring. It sets the mark at the beginning of that text, using `push-mark` (see [The Mark](The-Mark.html)), and puts point at the end.

    If `arg` is a non-`nil` list (which occurs interactively when the user types `C-u` with no digits), then `yank` inserts the text as described above, but puts point before the yanked text and sets the mark after it.

    If `arg` is a number, then `yank` inserts the `arg`th most recently killed text—the `arg`th element of the kill ring list, counted cyclically from the front, which is considered the first element for this purpose.

    `yank` does not alter the contents of the kill ring, unless it used text provided by another program, in which case it pushes that text onto the kill ring. However if `arg` is an integer different from one, it rotates the kill ring to place the yanked string at the front.

    `yank` returns `nil`.

<!---->

*   Command: **yank-pop** *\&optional arg*

    This command replaces the just-yanked entry from the kill ring with a different entry from the kill ring.

    This is allowed only immediately after a `yank` or another `yank-pop`. At such a time, the region contains text that was just inserted by yanking. `yank-pop` deletes that text and inserts in its place a different piece of killed text. It does not add the deleted text to the kill ring, since it is already in the kill ring somewhere. It does however rotate the kill ring to place the newly yanked string at the front.

    If `arg` is `nil`, then the replacement text is the previous element of the kill ring. If `arg` is numeric, the replacement is the `arg`th previous kill. If `arg` is negative, a more recent kill is the replacement.

    The sequence of kills in the kill ring wraps around, so that after the oldest one comes the newest one, and before the newest one goes the oldest.

    The return value is always `nil`.

<!---->

*   Variable: **yank-undo-function**

    If this variable is non-`nil`, the function `yank-pop` uses its value instead of `delete-region` to delete the text inserted by the previous `yank` or `yank-pop` command. The value must be a function of two arguments, the start and end of the current region.

    The function `insert-for-yank` automatically sets this variable according to the `undo` element of the `yank-handler` text property, if there is one.

Next: [Low-Level Kill Ring](Low_002dLevel-Kill-Ring.html), Previous: [Yanking](Yanking.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
