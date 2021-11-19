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

Next: [Terminal I/O Encoding](Terminal-I_002fO-Encoding.html), Previous: [Specifying Coding Systems](Specifying-Coding-Systems.html), Up: [Coding Systems](Coding-Systems.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 33.10.7 Explicit Encoding and Decoding

All the operations that transfer text in and out of Emacs have the ability to use a coding system to encode or decode the text. You can also explicitly encode and decode text using the functions in this section.

The result of encoding, and the input to decoding, are not ordinary text. They logically consist of a series of byte values; that is, a series of ASCII and eight-bit characters. In unibyte buffers and strings, these characters have codes in the range 0 through #xFF (255). In a multibyte buffer or string, eight-bit characters have character codes higher than #xFF (see [Text Representations](Text-Representations.html)), but Emacs transparently converts them to their single-byte values when you encode or decode such text.

The usual way to read a file into a buffer as a sequence of bytes, so you can decode the contents explicitly, is with `insert-file-contents-literally` (see [Reading from Files](Reading-from-Files.html)); alternatively, specify a non-`nil` `rawfile` argument when visiting a file with `find-file-noselect`. These methods result in a unibyte buffer.

The usual way to use the byte sequence that results from explicitly encoding text is to copy it to a file or process—for example, to write it with `write-region` (see [Writing to Files](Writing-to-Files.html)), and suppress encoding by binding `coding-system-for-write` to `no-conversion`.

Here are the functions to perform explicit encoding or decoding. The encoding functions produce sequences of bytes; the decoding functions are meant to operate on sequences of bytes. All of these functions discard text properties. They also set `last-coding-system-used` to the precise coding system they used.

*   Command: **encode-coding-region** *start end coding-system \&optional destination*

    This command encodes the text from `start` to `end` according to coding system `coding-system`. Normally, the encoded text replaces the original text in the buffer, but the optional argument `destination` can change that. If `destination` is a buffer, the encoded text is inserted in that buffer after point (point does not move); if it is `t`, the command returns the encoded text as a unibyte string without inserting it.

    If encoded text is inserted in some buffer, this command returns the length of the encoded text.

    The result of encoding is logically a sequence of bytes, but the buffer remains multibyte if it was multibyte before, and any 8-bit bytes are converted to their multibyte representation (see [Text Representations](Text-Representations.html)).

    Do *not* use `undecided` for `coding-system` when encoding text, since that may lead to unexpected results. Instead, use `select-safe-coding-system` (see [select-safe-coding-system](User_002dChosen-Coding-Systems.html)) to suggest a suitable encoding, if there’s no obvious pertinent value for `coding-system`.

<!---->

*   Function: **encode-coding-string** *string coding-system \&optional nocopy buffer*

    This function encodes the text in `string` according to coding system `coding-system`. It returns a new string containing the encoded text, except when `nocopy` is non-`nil`, in which case the function may return `string` itself if the encoding operation is trivial. The result of encoding is a unibyte string.

<!---->

*   Command: **decode-coding-region** *start end coding-system \&optional destination*

    This command decodes the text from `start` to `end` according to coding system `coding-system`. To make explicit decoding useful, the text before decoding ought to be a sequence of byte values, but both multibyte and unibyte buffers are acceptable (in the multibyte case, the raw byte values should be represented as eight-bit characters). Normally, the decoded text replaces the original text in the buffer, but the optional argument `destination` can change that. If `destination` is a buffer, the decoded text is inserted in that buffer after point (point does not move); if it is `t`, the command returns the decoded text as a multibyte string without inserting it.

    If decoded text is inserted in some buffer, this command returns the length of the decoded text. If that buffer is a unibyte buffer (see [Selecting a Representation](Selecting-a-Representation.html)), the internal representation of the decoded text (see [Text Representations](Text-Representations.html)) is inserted into the buffer as individual bytes.

    This command puts a `charset` text property on the decoded text. The value of the property states the character set used to decode the original text.

<!---->

*   Function: **decode-coding-string** *string coding-system \&optional nocopy buffer*

    This function decodes the text in `string` according to `coding-system`. It returns a new string containing the decoded text, except when `nocopy` is non-`nil`, in which case the function may return `string` itself if the decoding operation is trivial. To make explicit decoding useful, the contents of `string` ought to be a unibyte string with a sequence of byte values, but a multibyte string is also acceptable (assuming it contains 8-bit bytes in their multibyte form).

    If optional argument `buffer` specifies a buffer, the decoded text is inserted in that buffer after point (point does not move). In this case, the return value is the length of the decoded text. If that buffer is a unibyte buffer, the internal representation of the decoded text is inserted into it as individual bytes.

    This function puts a `charset` text property on the decoded text. The value of the property states the character set used to decode the original text:

        (decode-coding-string "Gr\374ss Gott" 'latin-1)
             ⇒ #("Grüss Gott" 0 9 (charset iso-8859-1))

<!---->

*   Function: **decode-coding-inserted-region** *from to filename \&optional visit beg end replace*

    This function decodes the text from `from` to `to` as if it were being read from file `filename` using `insert-file-contents` using the rest of the arguments provided.

    The normal way to use this function is after reading text from a file without decoding, if you decide you would rather have decoded it. Instead of deleting the text and reading it again, this time with decoding, you can call this function.

Next: [Terminal I/O Encoding](Terminal-I_002fO-Encoding.html), Previous: [Specifying Coding Systems](Specifying-Coding-Systems.html), Up: [Coding Systems](Coding-Systems.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
