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

Previous: [Numbered Backups](Numbered-Backups.html), Up: [Backup Files](Backup-Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 26.1.4 Naming Backup Files

The functions in this section are documented mainly because you can customize the naming conventions for backup files by redefining them. If you change one, you probably need to change the rest.

*   Function: **backup-file-name-p** *filename*

    This function returns a non-`nil` value if `filename` is a possible name for a backup file. It just checks the name, not whether a file with the name `filename` exists.

        (backup-file-name-p "foo")
             ⇒ nil

    <!---->

        (backup-file-name-p "foo~")
             ⇒ 3

    The standard definition of this function is as follows:

        (defun backup-file-name-p (file)
          "Return non-nil if FILE is a backup file \
        name (numeric or not)..."
          (string-match "~\\'" file))

    Thus, the function returns a non-`nil` value if the file name ends with a ‘`~`’. (We use a backslash to split the documentation string’s first line into two lines in the text, but produce just one line in the string itself.)

    This simple expression is placed in a separate function to make it easy to redefine for customization.

<!---->

*   Function: **make-backup-file-name** *filename*

    This function returns a string that is the name to use for a non-numbered backup file for file `filename`. On Unix, this is just `filename` with a tilde appended.

    The standard definition of this function, on most operating systems, is as follows:

        (defun make-backup-file-name (file)
          "Create the non-numeric backup file name for FILE..."
          (concat file "~"))

    You can change the backup-file naming convention by redefining this function. The following example redefines `make-backup-file-name` to prepend a ‘`.`’ in addition to appending a tilde:

        (defun make-backup-file-name (filename)
          (expand-file-name
            (concat "." (file-name-nondirectory filename) "~")
            (file-name-directory filename)))

    ```
    ```

        (make-backup-file-name "backups.texi")
             ⇒ ".backups.texi~"

    Some parts of Emacs, including some Dired commands, assume that backup file names end with ‘`~`’. If you do not follow that convention, it will not cause serious problems, but these commands may give less-than-desirable results.

<!---->

*   Function: **find-backup-file-name** *filename*

    This function computes the file name for a new backup file for `filename`. It may also propose certain existing backup files for deletion. `find-backup-file-name` returns a list whose CAR is the name for the new backup file and whose CDR is a list of backup files whose deletion is proposed. The value can also be `nil`, which means not to make a backup.

    Two variables, `kept-old-versions` and `kept-new-versions`, determine which backup versions should be kept. This function keeps those versions by excluding them from the CDR of the value. See [Numbered Backups](Numbered-Backups.html).

    In this example, the value says that `~rms/foo.~5~` is the name to use for the new backup file, and `~rms/foo.~3~` is an excess version that the caller should consider deleting now.

        (find-backup-file-name "~rms/foo")
             ⇒ ("~rms/foo.~5~" "~rms/foo.~3~")

<!---->

*   Function: **file-newest-backup** *filename*

    This function returns the name of the most recent backup file for `filename`, or `nil` if that file has no backup files.

    Some file comparison commands use this function so that they can automatically compare a file with its most recent backup.

Previous: [Numbered Backups](Numbered-Backups.html), Up: [Backup Files](Backup-Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
