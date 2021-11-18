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

Next: [File Locks](File-Locks.html), Previous: [Reading from Files](Reading-from-Files.html), Up: [Files](Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 25.4 Writing to Files

You can write the contents of a buffer, or part of a buffer, directly to a file on disk using the `append-to-file` and `write-region` functions. Don’t use these functions to write to files that are being visited; that could cause confusion in the mechanisms for visiting.

*   Command: **append-to-file** *start end filename*

    This function appends the contents of the region delimited by `start` and `end` in the current buffer to the end of file `filename`. If that file does not exist, it is created. This function returns `nil`.

    An error is signaled if you cannot write or create `filename`.

    When called from Lisp, this function is completely equivalent to:

        (write-region start end filename t)

<!---->

*   Command: **write-region** *start end filename \&optional append visit lockname mustbenew*

    This function writes the region delimited by `start` and `end` in the current buffer into the file specified by `filename`.

    If `start` is `nil`, then the command writes the entire buffer contents (*not* just the accessible portion) to the file and ignores `end`.

    If `start` is a string, then `write-region` writes or appends that string, rather than text from the buffer. `end` is ignored in this case.

    If `append` is non-`nil`, then the specified text is appended to the existing file contents (if any). If `append` is a number, `write-region` seeks to that byte offset from the start of the file and writes the data from there.

    If `mustbenew` is non-`nil`, then `write-region` asks for confirmation if `filename` names an existing file. If `mustbenew` is the symbol `excl`, then `write-region` does not ask for confirmation, but instead it signals an error `file-already-exists` if the file already exists. Although `write-region` normally follows a symbolic link and creates the pointed-to file if the symbolic link is dangling, it does not follow symbolic links if `mustbenew` is `excl`.

    The test for an existing file, when `mustbenew` is `excl`, uses a special system feature. At least for files on a local disk, there is no chance that some other program could create a file of the same name before Emacs does, without Emacs’s noticing.

    If `visit` is `t`, then Emacs establishes an association between the buffer and the file: the buffer is then visiting that file. It also sets the last file modification time for the current buffer to `filename`’s modtime, and marks the buffer as not modified. This feature is used by `save-buffer`, but you probably should not use it yourself.

    If `visit` is a string, it specifies the file name to visit. This way, you can write the data to one file (`filename`) while recording the buffer as visiting another file (`visit`). The argument `visit` is used in the echo area message and also for file locking; `visit` is stored in `buffer-file-name`. This feature is used to implement `file-precious-flag`; don’t use it yourself unless you really know what you’re doing.

    The optional argument `lockname`, if non-`nil`, specifies the file name to use for purposes of locking and unlocking, overriding `filename` and `visit` for that purpose.

    The function `write-region` converts the data which it writes to the appropriate file formats specified by `buffer-file-format` and also calls the functions in the list `write-region-annotate-functions`. See [Format Conversion](Format-Conversion.html).

    Normally, `write-region` displays the message ‘`Wrote filename`’ in the echo area. This message is inhibited if `visit` is neither `t` nor `nil` nor a string, or if Emacs is operating in batch mode (see [Batch Mode](Batch-Mode.html)). This feature is useful for programs that use files for internal purposes, files that the user does not need to know about.

<!---->

*   Variable: **write-region-inhibit-fsync**

    If this variable’s value is `nil`, `write-region` uses the `fsync` system call after writing a file. Although this slows Emacs down, it lessens the risk of data loss after power failure. If the value is `t`, Emacs does not use `fsync`. The default value is `nil` when Emacs is interactive, and `t` when Emacs runs in batch mode. See [Files and Storage](Files-and-Storage.html).

<!---->

*   Macro: **with-temp-file** *file body…*

    The `with-temp-file` macro evaluates the `body` forms with a temporary buffer as the current buffer; then, at the end, it writes the buffer contents into file `file`. It kills the temporary buffer when finished, restoring the buffer that was current before the `with-temp-file` form. Then it returns the value of the last form in `body`.

    The current buffer is restored even in case of an abnormal exit via `throw` or error (see [Nonlocal Exits](Nonlocal-Exits.html)).

    See also `with-temp-buffer` in [The Current Buffer](Current-Buffer.html#Definition-of-with_002dtemp_002dbuffer).

Next: [File Locks](File-Locks.html), Previous: [Reading from Files](Reading-from-Files.html), Up: [Files](Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
