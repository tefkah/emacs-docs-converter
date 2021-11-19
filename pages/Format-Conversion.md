

Previous: [Magic File Names](Magic-File-Names.html), Up: [Files](Files.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 25.13 File Format Conversion

Emacs performs several steps to convert the data in a buffer (text, text properties, and possibly other information) to and from a representation suitable for storing into a file. This section describes the fundamental functions that perform this *format conversion*, namely `insert-file-contents` for reading a file into a buffer, and `write-region` for writing a buffer into a file.

|                                                       |    |                                            |
| :---------------------------------------------------- | -- | :----------------------------------------- |
| • [Overview](Format-Conversion-Overview.html)         |    | `insert-file-contents` and `write-region`. |
| • [Round-Trip](Format-Conversion-Round_002dTrip.html) |    | Using `format-alist`.                      |
| • [Piecemeal](Format-Conversion-Piecemeal.html)       |    | Specifying non-paired conversion.          |
