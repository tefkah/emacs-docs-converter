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

Next: [Yank Commands](Yank-Commands.html), Previous: [Kill Functions](Kill-Functions.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.8.3 Yanking

Yanking means inserting text from the kill ring, but it does not insert the text blindly. The `yank` command, and related commands, use `insert-for-yank` to perform special processing on the text before it is inserted.

*   Function: **insert-for-yank** *string*

    This function works like `insert`, except that it processes the text in `string` according to the `yank-handler` text property, as well as the variables `yank-handled-properties` and `yank-excluded-properties` (see below), before inserting the result into the current buffer.

<!---->

*   Function: **insert-buffer-substring-as-yank** *buf \&optional start end*

    This function resembles `insert-buffer-substring`, except that it processes the text according to `yank-handled-properties` and `yank-excluded-properties`. (It does not handle the `yank-handler` property, which does not normally occur in buffer text anyway.)

If you put a `yank-handler` text property on all or part of a string, that alters how `insert-for-yank` inserts the string. If different parts of the string have different `yank-handler` values (comparison being done with `eq`), each substring is handled separately. The property value must be a list of one to four elements, with the following format (where elements after the first may be omitted):

    (function param noexclude undo)

Here is what the elements do:

*   `function`

    When `function` is non-`nil`, it is called instead of `insert` to insert the string, with one argument—the string to insert.

*   `param`

    If `param` is present and non-`nil`, it replaces `string` (or the substring of `string` being processed) as the object passed to `function` (or `insert`). For example, if `function` is `yank-rectangle`, `param` should be a list of strings to insert as a rectangle.

*   `noexclude`

    If `noexclude` is present and non-`nil`, that disables the normal action of `yank-handled-properties` and `yank-excluded-properties` on the inserted string.

*   `undo`

    If `undo` is present and non-`nil`, it is a function that will be called by `yank-pop` to undo the insertion of the current object. It is called with two arguments, the start and end of the current region. `function` can set `yank-undo-function` to override the `undo` value.

<!---->

*   User Option: **yank-handled-properties**

    This variable specifies special text property handling conditions for yanked text. It takes effect after the text has been inserted (either normally, or via the `yank-handler` property), and prior to `yank-excluded-properties` taking effect.

    The value should be an alist of elements `(prop . fun)`. Each alist element is handled in order. The inserted text is scanned for stretches of text having text properties `eq` to `prop`; for each such stretch, `fun` is called with three arguments: the value of the property, and the start and end positions of the text.

<!---->

*   User Option: **yank-excluded-properties**

    The value of this variable is the list of properties to remove from inserted text. Its default value contains properties that might lead to annoying results, such as causing the text to respond to the mouse or specifying key bindings. It takes effect after `yank-handled-properties`.

Next: [Yank Commands](Yank-Commands.html), Previous: [Kill Functions](Kill-Functions.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
