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

Next: [Directory Names](Directory-Names.html), Previous: [File Name Components](File-Name-Components.html), Up: [File Names](File-Names.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 25.9.2 Absolute and Relative File Names

All the directories in the file system form a tree starting at the root directory. A file name can specify all the directory names starting from the root of the tree; then it is called an *absolute* file name. Or it can specify the position of the file in the tree relative to a default directory; then it is called a *relative* file name. On GNU and other POSIX-like systems, after any leading ‘`~`’ has been expanded, an absolute file name starts with a ‘`/`’ (see [abbreviate-file-name](Directory-Names.html#abbreviate_002dfile_002dname)), and a relative one does not. On MS-DOS and MS-Windows, an absolute file name starts with a slash or a backslash, or with a drive specification ‘`x:/`’, where `x` is the *drive letter*.

*   Function: **file-name-absolute-p** *filename*

    This function returns `t` if file `filename` is an absolute file name, `nil` otherwise. A file name is considered to be absolute if its first component is ‘`~`’, or is ‘`~user`’ where `user` is a valid login name. In the following examples, assume that there is a user named ‘`rms`’ but no user named ‘`nosuchuser`’.

        (file-name-absolute-p "~rms/foo")
             ⇒ t

    <!---->

        (file-name-absolute-p "~nosuchuser/foo")
             ⇒ nil

    <!---->

        (file-name-absolute-p "rms/foo")
             ⇒ nil

    <!---->

        (file-name-absolute-p "/user/rms/foo")
             ⇒ t

Given a possibly relative file name, you can expand any leading ‘`~`’ and convert the result to an absolute name using `expand-file-name` (see [File Name Expansion](File-Name-Expansion.html)). This function converts absolute file names to relative names:

*   Function: **file-relative-name** *filename \&optional directory*

    This function tries to return a relative name that is equivalent to `filename`, assuming the result will be interpreted relative to `directory` (an absolute directory name or directory file name). If `directory` is omitted or `nil`, it defaults to the current buffer’s default directory.

    On some operating systems, an absolute file name begins with a device name. On such systems, `filename` has no relative equivalent based on `directory` if they start with two different device names. In this case, `file-relative-name` returns `filename` in absolute form.

        (file-relative-name "/foo/bar" "/foo/")
             ⇒ "bar"
        (file-relative-name "/foo/bar" "/hack/")
             ⇒ "../foo/bar"

Next: [Directory Names](Directory-Names.html), Previous: [File Name Components](File-Name-Components.html), Up: [File Names](File-Names.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
