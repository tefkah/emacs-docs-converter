

Next: [Changing Files](Changing-Files.html), Previous: [File Locks](File-Locks.html), Up: [Files](Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 25.6 Information about Files

This section describes the functions for retrieving various types of information about files (or directories or symbolic links), such as whether a file is readable or writable, and its size. These functions all take arguments which are file names. Except where noted, these arguments need to specify existing files, or an error is signaled.

Be careful with file names that end in spaces. On some filesystems (notably, MS-Windows), trailing whitespace characters in file names are silently and automatically ignored.

|                                                       |    |                                              |
| :---------------------------------------------------- | -- | :------------------------------------------- |
| • [Testing Accessibility](Testing-Accessibility.html) |    | Is a given file readable? Writable?          |
| • [Kinds of Files](Kinds-of-Files.html)               |    | Is it a directory? A symbolic link?          |
| • [Truenames](Truenames.html)                         |    | Eliminating symbolic links from a file name. |
| • [File Attributes](File-Attributes.html)             |    | File sizes, modification times, etc.         |
| • [Extended Attributes](Extended-Attributes.html)     |    | Extended file attributes for access control. |
| • [Locating Files](Locating-Files.html)               |    | How to find a file in standard places.       |
