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

Next: [Writing to Files](Writing-to-Files.html), Previous: [Saving Buffers](Saving-Buffers.html), Up: [Files](Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 25.3 Reading from Files

To copy the contents of a file into a buffer, use the function `insert-file-contents`. (Don’t use the command `insert-file` in a Lisp program, as that sets the mark.)

*   Function: **insert-file-contents** *filename \&optional visit beg end replace*

    This function inserts the contents of file `filename` into the current buffer after point. It returns a list of the absolute file name and the length of the data inserted. An error is signaled if `filename` is not the name of a file that can be read.

    This function checks the file contents against the defined file formats, and converts the file contents if appropriate and also calls the functions in the list `after-insert-file-functions`. See [Format Conversion](Format-Conversion.html). Normally, one of the functions in the `after-insert-file-functions` list determines the coding system (see [Coding Systems](Coding-Systems.html)) used for decoding the file’s contents, including end-of-line conversion. However, if the file contains null bytes, it is by default visited without any code conversions. See [inhibit-nul-byte-detection](Lisp-and-Coding-Systems.html).

    If `visit` is non-`nil`, this function additionally marks the buffer as unmodified and sets up various fields in the buffer so that it is visiting the file `filename`: these include the buffer’s visited file name and its last save file modtime. This feature is used by `find-file-noselect` and you probably should not use it yourself.

    If `beg` and `end` are non-`nil`, they should be numbers that are byte offsets specifying the portion of the file to insert. In this case, `visit` must be `nil`. For example,

        (insert-file-contents filename nil 0 500)

    inserts the first 500 characters of a file.

    If the argument `replace` is non-`nil`, it means to replace the contents of the buffer (actually, just the accessible portion) with the contents of the file. This is better than simply deleting the buffer contents and inserting the whole file, because (1) it preserves some marker positions and (2) it puts less data in the undo list.

    It is possible to read a special file (such as a FIFO or an I/O device) with `insert-file-contents`, as long as `replace` and `visit` are `nil`.

<!---->

*   Function: **insert-file-contents-literally** *filename \&optional visit beg end replace*

    This function works like `insert-file-contents` except that it does not run `after-insert-file-functions`, and does not do format decoding, character code conversion, automatic uncompression, and so on.

If you want to pass a file name to another process so that another program can read the file, use the function `file-local-copy`; see [Magic File Names](Magic-File-Names.html).

Next: [Writing to Files](Writing-to-Files.html), Previous: [Saving Buffers](Saving-Buffers.html), Up: [Files](Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
