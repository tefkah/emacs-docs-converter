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

Next: [Relative File Names](Relative-File-Names.html), Up: [File Names](File-Names.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 25.9.1 File Name Components

The operating system groups files into directories. To specify a file, you must specify the directory and the file’s name within that directory. Therefore, Emacs considers a file name as having two main parts: the *directory name* part, and the *nondirectory* part (or *file name within the directory*). Either part may be empty. Concatenating these two parts reproduces the original file name.

On most systems, the directory part is everything up to and including the last slash (backslash is also allowed in input on MS-DOS or MS-Windows); the nondirectory part is the rest.

For some purposes, the nondirectory part is further subdivided into the name proper and the *version number*. On most systems, only backup files have version numbers in their names.

*   Function: **file-name-directory** *filename*

    This function returns the directory part of `filename`, as a directory name (see [Directory Names](Directory-Names.html)), or `nil` if `filename` does not include a directory part.

    On GNU and other POSIX-like systems, a string returned by this function always ends in a slash. On MS-DOS it can also end in a colon.

        (file-name-directory "lewis/foo")  ; GNU example
             ⇒ "lewis/"

    <!---->

        (file-name-directory "foo")        ; GNU example
             ⇒ nil

<!---->

*   Function: **file-name-nondirectory** *filename*

    This function returns the nondirectory part of `filename`.

        (file-name-nondirectory "lewis/foo")
             ⇒ "foo"

    <!---->

        (file-name-nondirectory "foo")
             ⇒ "foo"

    <!---->

        (file-name-nondirectory "lewis/")
             ⇒ ""

<!---->

*   Function: **file-name-sans-versions** *filename \&optional keep-backup-version*

    This function returns `filename` with any file version numbers, backup version numbers, or trailing tildes discarded.

    If `keep-backup-version` is non-`nil`, then true file version numbers understood as such by the file system are discarded from the return value, but backup version numbers are kept.

        (file-name-sans-versions "~rms/foo.~1~")
             ⇒ "~rms/foo"

    <!---->

        (file-name-sans-versions "~rms/foo~")
             ⇒ "~rms/foo"

    <!---->

        (file-name-sans-versions "~rms/foo")
             ⇒ "~rms/foo"

<!---->

*   Function: **file-name-extension** *filename \&optional period*

    This function returns `filename`’s final extension, if any, after applying `file-name-sans-versions` to remove any version/backup part. The extension, in a file name, is the part that follows the last ‘`.`’ in the last name component (minus any version/backup part).

    This function returns `nil` for extensionless file names such as `foo`. It returns `""` for null extensions, as in `foo.`. If the last component of a file name begins with a ‘`.`’, that ‘`.`’ doesn’t count as the beginning of an extension. Thus, `.emacs`’s extension is `nil`, not ‘`.emacs`’.

    If `period` is non-`nil`, then the returned value includes the period that delimits the extension, and if `filename` has no extension, the value is `""`.

<!---->

*   Function: **file-name-sans-extension** *filename*

    This function returns `filename` minus its extension, if any. The version/backup part, if present, is only removed if the file has an extension. For example,

        (file-name-sans-extension "foo.lose.c")
             ⇒ "foo.lose"
        (file-name-sans-extension "big.hack/foo")
             ⇒ "big.hack/foo"
        (file-name-sans-extension "/my/home/.emacs")
             ⇒ "/my/home/.emacs"
        (file-name-sans-extension "/my/home/.emacs.el")
             ⇒ "/my/home/.emacs"
        (file-name-sans-extension "~/foo.el.~3~")
             ⇒ "~/foo"
        (file-name-sans-extension "~/foo.~3~")
             ⇒ "~/foo.~3~"

    Note that the ‘`.~3~`’ in the two last examples is the backup part, not an extension.

<!---->

*   Function: **file-name-base** *filename*

    This function is the composition of `file-name-sans-extension` and `file-name-nondirectory`. For example,

        (file-name-base "/my/home/foo.c")
            ⇒ "foo"

Next: [Relative File Names](Relative-File-Names.html), Up: [File Names](File-Names.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
