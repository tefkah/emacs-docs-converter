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

Next: [Buffer List](Buffer-List.html), Previous: [Modification Time](Modification-Time.html), Up: [Buffers](Buffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 27.7 Read-Only Buffers

If a buffer is *read-only*, then you cannot change its contents, although you may change your view of the contents by scrolling and narrowing.

Read-only buffers are used in two kinds of situations:

*   A buffer visiting a write-protected file is normally read-only.

    Here, the purpose is to inform the user that editing the buffer with the aim of saving it in the file may be futile or undesirable. The user who wants to change the buffer text despite this can do so after clearing the read-only flag with `C-x C-q`.

*   Modes such as Dired and Rmail make buffers read-only when altering the contents with the usual editing commands would probably be a mistake.

    The special commands of these modes bind `buffer-read-only` to `nil` (with `let`) or bind `inhibit-read-only` to `t` around the places where they themselves change the text.

<!---->

*   Variable: **buffer-read-only**

    This buffer-local variable specifies whether the buffer is read-only. The buffer is read-only if this variable is non-`nil`. However, characters that have the `inhibit-read-only` text property can still be modified. See [inhibit-read-only](Special-Properties.html).

<!---->

*   Variable: **inhibit-read-only**

    If this variable is non-`nil`, then read-only buffers and, depending on the actual value, some or all read-only characters may be modified. Read-only characters in a buffer are those that have a non-`nil` `read-only` text property. See [Special Properties](Special-Properties.html), for more information about text properties.

    If `inhibit-read-only` is `t`, all `read-only` character properties have no effect. If `inhibit-read-only` is a list, then `read-only` character properties have no effect if they are members of the list (comparison is done with `eq`).

<!---->

*   Command: **read-only-mode** *\&optional arg*

    This is the mode command for Read Only minor mode, a buffer-local minor mode. When the mode is enabled, `buffer-read-only` is non-`nil` in the buffer; when disabled, `buffer-read-only` is `nil` in the buffer. The calling convention is the same as for other minor mode commands (see [Minor Mode Conventions](Minor-Mode-Conventions.html)).

    This minor mode mainly serves as a wrapper for `buffer-read-only`; unlike most minor modes, there is no separate `read-only-mode` variable. Even when Read Only mode is disabled, characters with non-`nil` `read-only` text properties remain read-only. To temporarily ignore all read-only states, bind `inhibit-read-only`, as described above.

    When enabling Read Only mode, this mode command also enables View mode if the option `view-read-only` is non-`nil`. See [Miscellaneous Buffer Operations](https://www.gnu.org/software/emacs/manual/html_node/emacs/Misc-Buffer.html#Misc-Buffer) in The GNU Emacs Manual. When disabling Read Only mode, it disables View mode if View mode was enabled.

<!---->

*   Function: **barf-if-buffer-read-only** *\&optional position*

    This function signals a `buffer-read-only` error if the current buffer is read-only. If the text at `position` (which defaults to point) has the `inhibit-read-only` text property set, the error will not be raised.

    See [Using Interactive](Using-Interactive.html), for another way to signal an error if the current buffer is read-only.

Next: [Buffer List](Buffer-List.html), Previous: [Modification Time](Modification-Time.html), Up: [Buffers](Buffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
