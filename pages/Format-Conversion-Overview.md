

Next: [Format Conversion Round-Trip](Format-Conversion-Round_002dTrip.html), Up: [Format Conversion](Format-Conversion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 25.13.1 Overview

The function `insert-file-contents`:

*   initially, inserts bytes from the file into the buffer;
*   decodes bytes to characters as appropriate;
*   processes formats as defined by entries in `format-alist`; and
*   calls functions in `after-insert-file-functions`.

The function `write-region`:

*   initially, calls functions in `write-region-annotate-functions`;
*   processes formats as defined by entries in `format-alist`;
*   encodes characters to bytes as appropriate; and
*   modifies the file with the bytes.

This shows the symmetry of the lowest-level operations; reading and writing handle things in opposite order. The rest of this section describes the two facilities surrounding the three variables named above, as well as some related functions. [Coding Systems](Coding-Systems.html), for details on character encoding and decoding.
