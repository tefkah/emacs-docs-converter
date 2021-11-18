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

Next: [Character Properties](Character-Properties.html), Previous: [Selecting a Representation](Selecting-a-Representation.html), Up: [Non-ASCII Characters](Non_002dASCII-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 33.5 Character Codes

The unibyte and multibyte text representations use different character codes. The valid character codes for unibyte representation range from 0 to `#xFF` (255)—the values that can fit in one byte. The valid character codes for multibyte representation range from 0 to `#x3FFFFF`. In this code space, values 0 through `#x7F` (127) are for ASCII characters, and values `#x80` (128) through `#x3FFF7F` (4194175) are for non-ASCII characters.

Emacs character codes are a superset of the Unicode standard. Values 0 through `#x10FFFF` (1114111) correspond to Unicode characters of the same codepoint; values `#x110000` (1114112) through `#x3FFF7F` (4194175) represent characters that are not unified with Unicode; and values `#x3FFF80` (4194176) through `#x3FFFFF` (4194303) represent eight-bit raw bytes.

*   Function: **characterp** *charcode*

    This returns `t` if `charcode` is a valid character, and `nil` otherwise.

        (characterp 65)
             ⇒ t

    <!---->

        (characterp 4194303)
             ⇒ t

    <!---->

        (characterp 4194304)
             ⇒ nil

<!---->

*   Function: **max-char**

    This function returns the largest value that a valid character codepoint can have.

        (characterp (max-char))
             ⇒ t

    <!---->

        (characterp (1+ (max-char)))
             ⇒ nil

<!---->

*   Function: **char-from-name** *string \&optional ignore-case*

    This function returns the character whose Unicode name is `string`. If `ignore-case` is non-`nil`, case is ignored in `string`. This function returns `nil` if `string` does not name a character.

        ;; U+03A3
        (= (char-from-name "GREEK CAPITAL LETTER SIGMA") #x03A3)
             ⇒ t

<!---->

*   Function: **get-byte** *\&optional pos string*

    This function returns the byte at character position `pos` in the current buffer. If the current buffer is unibyte, this is literally the byte at that position. If the buffer is multibyte, byte values of ASCII characters are the same as character codepoints, whereas eight-bit raw bytes are converted to their 8-bit codes. The function signals an error if the character at `pos` is non-ASCII.

    The optional argument `string` means to get a byte value from that string instead of the current buffer.

Next: [Character Properties](Character-Properties.html), Previous: [Selecting a Representation](Selecting-a-Representation.html), Up: [Non-ASCII Characters](Non_002dASCII-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
