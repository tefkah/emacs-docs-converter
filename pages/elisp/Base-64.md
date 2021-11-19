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

Next: [Checksum/Hash](Checksum_002fHash.html), Previous: [Decompression](Decompression.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.25 Base 64 Encoding

Base 64 code is used in email to encode a sequence of 8-bit bytes as a longer sequence of ASCII graphic characters. It is defined in Internet RFC[16](#FOOT16)2045 and also in RFC 4648. This section describes the functions for converting to and from this code.

*   Command: **base64-encode-region** *beg end \&optional no-line-break*

    This function converts the region from `beg` to `end` into base 64 code. It returns the length of the encoded text. An error is signaled if a character in the region is multibyte, i.e., in a multibyte buffer the region must contain only characters from the charsets `ascii`, `eight-bit-control` and `eight-bit-graphic`.

    Normally, this function inserts newline characters into the encoded text, to avoid overlong lines. However, if the optional argument `no-line-break` is non-`nil`, these newlines are not added, so the output is just one long line.

<!---->

*   Command: **base64url-encode-region** *beg end \&optional no-pad*

    This function is like `base64-encode-region`, but it implements the URL variant of base 64 encoding, per RFC 4648, and it doesn’t insert newline characters into the encoded text, so the output is just one long line.

    If the optional argument `no-pad` is non-`nil` then this function doesn’t generate the padding (`=`).

<!---->

*   Function: **base64-encode-string** *string \&optional no-line-break*

    This function converts the string `string` into base 64 code. It returns a string containing the encoded text. As for `base64-encode-region`, an error is signaled if a character in the string is multibyte.

    Normally, this function inserts newline characters into the encoded text, to avoid overlong lines. However, if the optional argument `no-line-break` is non-`nil`, these newlines are not added, so the result string is just one long line.

<!---->

*   Function: **base64url-encode-string** *string \&optional no-pad*

    Like `base64-encode-string`, but generates the URL variant of base 64, and doesn’t insert newline characters into the encoded text, so the result is just one long line.

    If the optional argument `no-pad` is non-`nil` then this function doesn’t generate the padding.

<!---->

*   Command: **base64-decode-region** *beg end \&optional base64url*

    This function converts the region from `beg` to `end` from base 64 code into the corresponding decoded text. It returns the length of the decoded text.

    The decoding functions ignore newline characters in the encoded text.

    If optional argument `base64url` is non-`nil`, then padding is optional, and the URL variant of base 64 encoding is used.

<!---->

*   Function: **base64-decode-string** *string \&optional base64url*

    This function converts the string `string` from base 64 code into the corresponding decoded text. It returns a unibyte string containing the decoded text.

    The decoding functions ignore newline characters in the encoded text.

    If optional argument `base64url` is non-`nil`, then padding is optional, and the URL variant of base 64 encoding is used.

***

#### Footnotes

##### [(16)](#DOCF16)

An RFC, an acronym for *Request for Comments*, is a numbered Internet informational document describing a standard. RFCs are usually written by technical experts acting on their own initiative, and are traditionally written in a pragmatic, experience-driven manner.

Next: [Checksum/Hash](Checksum_002fHash.html), Previous: [Decompression](Decompression.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
