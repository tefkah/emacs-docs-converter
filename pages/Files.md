

Next: [Backups and Auto-Saving](Backups-and-Auto_002dSaving.html), Previous: [Documentation](Documentation.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 25 Files

This chapter describes the Emacs Lisp functions and variables to find, create, view, save, and otherwise work with files and directories. A few other file-related functions are described in [Buffers](Buffers.html), and those related to backups and auto-saving are described in [Backups and Auto-Saving](Backups-and-Auto_002dSaving.html).

Many of the file functions take one or more arguments that are file names. A file name is a string. Most of these functions expand file name arguments using the function `expand-file-name`, so that `~` is handled correctly, as are relative file names (including `../` and the empty string). See [File Name Expansion](File-Name-Expansion.html).

In addition, certain *magic* file names are handled specially. For example, when a remote file name is specified, Emacs accesses the file over the network via an appropriate protocol. See [Remote Files](https://www.gnu.org/software/emacs/manual/html_node/emacs/Remote-Files.html#Remote-Files) in The GNU Emacs Manual. This handling is done at a very low level, so you may assume that all the functions described in this chapter accept magic file names as file name arguments, except where noted. See [Magic File Names](Magic-File-Names.html), for details.

When file I/O functions signal Lisp errors, they usually use the condition `file-error` (see [Handling Errors](Handling-Errors.html)). The error message is in most cases obtained from the operating system, according to locale `system-messages-locale`, and decoded using coding system `locale-coding-system` (see [Locales](Locales.html)).

|                                                           |    |                                                                             |
| :-------------------------------------------------------- | -- | :-------------------------------------------------------------------------- |
| • [Visiting Files](Visiting-Files.html)                   |    | Reading files into Emacs buffers for editing.                               |
| • [Saving Buffers](Saving-Buffers.html)                   |    | Writing changed buffers back into files.                                    |
| • [Reading from Files](Reading-from-Files.html)           |    | Reading files into buffers without visiting.                                |
| • [Writing to Files](Writing-to-Files.html)               |    | Writing new files from parts of buffers.                                    |
| • [File Locks](File-Locks.html)                           |    | Locking and unlocking files, to prevent simultaneous editing by two people. |
| • [Information about Files](Information-about-Files.html) |    | Testing existence, accessibility, size of files.                            |
| • [Changing Files](Changing-Files.html)                   |    | Renaming files, changing permissions, etc.                                  |
| • [Files and Storage](Files-and-Storage.html)             |    | Surviving power and media failures                                          |
| • [File Names](File-Names.html)                           |    | Decomposing and expanding file names.                                       |
| • [Contents of Directories](Contents-of-Directories.html) |    | Getting a list of the files in a directory.                                 |
| • [Create/Delete Dirs](Create_002fDelete-Dirs.html)       |    | Creating and Deleting Directories.                                          |
| • [Magic File Names](Magic-File-Names.html)               |    | Special handling for certain file names.                                    |
| • [Format Conversion](Format-Conversion.html)             |    | Conversion to and from various file formats.                                |

Next: [Backups and Auto-Saving](Backups-and-Auto_002dSaving.html), Previous: [Documentation](Documentation.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
