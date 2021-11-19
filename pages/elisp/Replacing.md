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

Next: [Decompression](Decompression.html), Previous: [Transposition](Transposition.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.23 Replacing Buffer Text

You can use the following function to replace the text of one buffer with the text of another buffer:

*   Command: **replace-buffer-contents** *source \&optional max-secs max-costs*

    This function replaces the accessible portion of the current buffer with the accessible portion of the buffer `source`. `source` may either be a buffer object or the name of a buffer. When `replace-buffer-contents` succeeds, the text of the accessible portion of the current buffer will be equal to the text of the accessible portion of the `source` buffer.

    This function attempts to keep point, markers, text properties, and overlays in the current buffer intact. One potential case where this behavior is useful is external code formatting programs: they typically write the reformatted text into a temporary buffer or file, and using `delete-region` and `insert-buffer-substring` would destroy these properties. However, the latter combination is typically faster (See [Deletion](Deletion.html), and [Insertion](Insertion.html)).

    For its working, `replace-buffer-contents` needs to compare the contents of the original buffer with that of `source` which is a costly operation if the buffers are huge and there is a high number of differences between them. In order to keep `replace-buffer-contents`’s runtime in bounds, it has two optional arguments.

    `max-secs` defines a hard boundary in terms of seconds. If given and exceeded, it will fall back to `delete-region` and `insert-buffer-substring`.

    `max-costs` defines the quality of the difference computation. If the actual costs exceed this limit, heuristics are used to provide a faster but suboptimal solution. The default value is 1000000.

    `replace-buffer-contents` returns t if a non-destructive replacement could be performed. Otherwise, i.e., if `max-secs` was exceeded, it returns nil.

<!---->

*   Function: **replace-region-contents** *beg end replace-fn \&optional max-secs max-costs*

    This function replaces the region between `beg` and `end` using the given `replace-fn`. The function `replace-fn` is run in the current buffer narrowed to the specified region and it should return either a string or a buffer replacing the region.

    The replacement is performed using `replace-buffer-contents` (see above) which also describes the `max-secs` and `max-costs` arguments and the return value.

    Note: If the replacement is a string, it will be placed in a temporary buffer so that `replace-buffer-contents` can operate on it. Therefore, if you already have the replacement in a buffer, it makes no sense to convert it to a string using `buffer-substring` or similar.

Next: [Decompression](Decompression.html), Previous: [Transposition](Transposition.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
