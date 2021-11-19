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

Next: [Connection Local Variables](Connection-Local-Variables.html), Previous: [File Local Variables](File-Local-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 12.13 Directory Local Variables

A directory can specify local variable values common to all files in that directory; Emacs uses these to create buffer-local bindings for those variables in buffers visiting any file in that directory. This is useful when the files in the directory belong to some *project* and therefore share the same local variables.

There are two different methods for specifying directory local variables: by putting them in a special file, or by defining a *project class* for that directory.

*   Constant: **dir-locals-file**

    This constant is the name of the file where Emacs expects to find the directory-local variables. The name of the file is `.dir-locals.el`[10](#FOOT10). A file by that name in a directory causes Emacs to apply its settings to any file in that directory or any of its subdirectories (optionally, you can exclude subdirectories; see below). If some of the subdirectories have their own `.dir-locals.el` files, Emacs uses the settings from the deepest file it finds starting from the file’s directory and moving up the directory tree. This constant is also used to derive the name of a second dir-locals file `.dir-locals-2.el`. If this second dir-locals file is present, then that is loaded in addition to `.dir-locals.el`. This is useful when `.dir-locals.el` is under version control in a shared repository and cannot be used for personal customizations. The file specifies local variables as a specially formatted list; see [Per-directory Local Variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables) in The GNU Emacs Manual, for more details.

<!---->

*   Function: **hack-dir-local-variables**

    This function reads the `.dir-locals.el` file and stores the directory-local variables in `file-local-variables-alist` that is local to the buffer visiting any file in the directory, without applying them. It also stores the directory-local settings in `dir-locals-class-alist`, where it defines a special class for the directory in which `.dir-locals.el` file was found. This function works by calling `dir-locals-set-class-variables` and `dir-locals-set-directory-class`, described below.

<!---->

*   Function: **hack-dir-local-variables-non-file-buffer**

    This function looks for directory-local variables, and immediately applies them in the current buffer. It is intended to be called in the mode commands for non-file buffers, such as Dired buffers, to let them obey directory-local variable settings. For non-file buffers, Emacs looks for directory-local variables in `default-directory` and its parent directories.

<!---->

*   Function: **dir-locals-set-class-variables** *class variables*

    This function defines a set of variable settings for the named `class`, which is a symbol. You can later assign the class to one or more directories, and Emacs will apply those variable settings to all files in those directories. The list in `variables` can be of one of the two forms: `(major-mode . alist)` or `(directory . list)`. With the first form, if the file’s buffer turns on a mode that is derived from `major-mode`, then all the variables in the associated `alist` are applied; `alist` should be of the form `(name . value)`. A special value `nil` for `major-mode` means the settings are applicable to any mode. In `alist`, you can use a special `name`: `subdirs`. If the associated value is `nil`, the alist is only applied to files in the relevant directory, not to those in any subdirectories.

    With the second form of `variables`, if `directory` is the initial substring of the file’s directory, then `list` is applied recursively by following the above rules; `list` should be of one of the two forms accepted by this function in `variables`.

<!---->

*   Function: **dir-locals-set-directory-class** *directory class \&optional mtime*

    This function assigns `class` to all the files in `directory` and its subdirectories. Thereafter, all the variable settings specified for `class` will be applied to any visited file in `directory` and its children. `class` must have been already defined by `dir-locals-set-class-variables`.

    Emacs uses this function internally when it loads directory variables from a `.dir-locals.el` file. In that case, the optional argument `mtime` holds the file modification time (as returned by `file-attributes`). Emacs uses this time to check stored local variables are still valid. If you are assigning a class directly, not via a file, this argument should be `nil`.

<!---->

*   Variable: **dir-locals-class-alist**

    This alist holds the class symbols and the associated variable settings. It is updated by `dir-locals-set-class-variables`.

<!---->

*   Variable: **dir-locals-directory-cache**

    This alist holds directory names, their assigned class names, and modification times of the associated directory local variables file (if there is one). The function `dir-locals-set-directory-class` updates this list.

<!---->

*   Variable: **enable-dir-local-variables**

    If `nil`, directory-local variables are ignored. This variable may be useful for modes that want to ignore directory-locals while still respecting file-local variables (see [File Local Variables](File-Local-Variables.html)).

***

#### Footnotes

##### [(10)](#DOCF10)

The MS-DOS version of Emacs uses `_dir-locals.el` instead, due to limitations of the DOS filesystems.

Next: [Connection Local Variables](Connection-Local-Variables.html), Previous: [File Local Variables](File-Local-Variables.html), Up: [Variables](Variables.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
