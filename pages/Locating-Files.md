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

Previous: [Extended Attributes](Extended-Attributes.html), Up: [Information about Files](Information-about-Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 25.6.6 Locating Files in Standard Places

This section explains how to search for a file in a list of directories (a *path*), or for an executable file in the standard list of executable file directories.

To search for a user-specific configuration file, See [Standard File Names](Standard-File-Names.html), for the `locate-user-emacs-file` function.

*   Function: **locate-file** *filename path \&optional suffixes predicate*

    This function searches for a file whose name is `filename` in a list of directories given by `path`, trying the suffixes in `suffixes`. If it finds such a file, it returns the file’s absolute file name (see [Relative File Names](Relative-File-Names.html)); otherwise it returns `nil`.

    The optional argument `suffixes` gives the list of file-name suffixes to append to `filename` when searching. `locate-file` tries each possible directory with each of these suffixes. If `suffixes` is `nil`, or `("")`, then there are no suffixes, and `filename` is used only as-is. Typical values of `suffixes` are `exec-suffixes` (see [Subprocess Creation](Subprocess-Creation.html)), `load-suffixes`, `load-file-rep-suffixes` and the return value of the function `get-load-suffixes` (see [Load Suffixes](Load-Suffixes.html)).

    Typical values for `path` are `exec-path` (see [Subprocess Creation](Subprocess-Creation.html)) when looking for executable programs, or `load-path` (see [Library Search](Library-Search.html)) when looking for Lisp files. If `filename` is absolute, `path` has no effect, but the suffixes in `suffixes` are still tried.

    The optional argument `predicate`, if non-`nil`, specifies a predicate function for testing whether a candidate file is suitable. The predicate is passed the candidate file name as its single argument. If `predicate` is `nil` or omitted, `locate-file` uses `file-readable-p` as the predicate. See [Kinds of Files](Kinds-of-Files.html), for other useful predicates, e.g., `file-executable-p` and `file-directory-p`.

    This function will normally skip directories, so if you want it to find directories, make sure the `predicate` function returns `dir-ok` for them. For example:

        (locate-file "html" '("/var/www" "/srv") nil
                     (lambda (f) (if (file-directory-p f) 'dir-ok)))

    For compatibility, `predicate` can also be one of the symbols `executable`, `readable`, `writable`, `exists`, or a list of one or more of these symbols.

<!---->

*   Function: **executable-find** *program \&optional remote*

    This function searches for the executable file of the named `program` and returns the absolute file name of the executable, including its file-name extensions, if any. It returns `nil` if the file is not found. The function searches in all the directories in `exec-path`, and tries all the file-name extensions in `exec-suffixes` (see [Subprocess Creation](Subprocess-Creation.html)).

    If `remote` is non-`nil`, and `default-directory` is a remote directory, `program` is searched on the respective remote host.

Previous: [Extended Attributes](Extended-Attributes.html), Up: [Information about Files](Information-about-Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]