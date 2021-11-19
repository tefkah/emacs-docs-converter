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

Next: [Formatting Strings](Formatting-Strings.html), Previous: [Text Comparison](Text-Comparison.html), Up: [Strings and Characters](Strings-and-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 4.6 Conversion of Characters and Strings

This section describes functions for converting between characters, strings and integers. `format` (see [Formatting Strings](Formatting-Strings.html)) and `prin1-to-string` (see [Output Functions](Output-Functions.html)) can also convert Lisp objects into strings. `read-from-string` (see [Input Functions](Input-Functions.html)) can convert a string representation of a Lisp object into an object. The functions `string-to-multibyte` and `string-to-unibyte` convert the text representation of a string (see [Converting Representations](Converting-Representations.html)).

See [Documentation](Documentation.html), for functions that produce textual descriptions of text characters and general input events (`single-key-description` and `text-char-description`). These are used primarily for making help messages.

*   Function: **number-to-string** *number*

    This function returns a string consisting of the printed base-ten representation of `number`. The returned value starts with a minus sign if the argument is negative.

        (number-to-string 256)
             ⇒ "256"

    <!---->

        (number-to-string -23)
             ⇒ "-23"

    <!---->

        (number-to-string -23.5)
             ⇒ "-23.5"

    `int-to-string` is a semi-obsolete alias for this function.

    See also the function `format` in [Formatting Strings](Formatting-Strings.html).

<!---->

*   Function: **string-to-number** *string \&optional base*

    This function returns the numeric value of the characters in `string`. If `base` is non-`nil`, it must be an integer between 2 and 16 (inclusive), and integers are converted in that base. If `base` is `nil`, then base ten is used. Floating-point conversion only works in base ten; we have not implemented other radices for floating-point numbers, because that would be much more work and does not seem useful. If `string` looks like an integer but its value is too large to fit into a Lisp integer, `string-to-number` returns a floating-point result.

    The parsing skips spaces and tabs at the beginning of `string`, then reads as much of `string` as it can interpret as a number in the given base. (On some systems it ignores other whitespace at the beginning, not just spaces and tabs.) If `string` cannot be interpreted as a number, this function returns 0.

        (string-to-number "256")
             ⇒ 256
        (string-to-number "25 is a perfect square.")
             ⇒ 25
        (string-to-number "X256")
             ⇒ 0
        (string-to-number "-4.5")
             ⇒ -4.5
        (string-to-number "1e5")
             ⇒ 100000.0

    `string-to-int` is an obsolete alias for this function.

<!---->

*   Function: **char-to-string** *character*

    This function returns a new string containing one character, `character`. This function is semi-obsolete because the function `string` is more general. See [Creating Strings](Creating-Strings.html).

<!---->

*   Function: **string-to-char** *string*

    This function returns the first character in `string`. This mostly identical to `(aref string 0)`, except that it returns 0 if the string is empty. (The value is also 0 when the first character of `string` is the null character, ASCII code 0.) This function may be eliminated in the future if it does not seem useful enough to retain.

Here are some other functions that can convert to or from a string:

*   `concat`

    This function converts a vector or a list into a string. See [Creating Strings](Creating-Strings.html).

*   `vconcat`

    This function converts a string into a vector. See [Vector Functions](Vector-Functions.html).

*   `append`

    This function converts a string into a list. See [Building Lists](Building-Lists.html).

*   `byte-to-string`

    This function converts a byte of character data into a unibyte string. See [Converting Representations](Converting-Representations.html).

Next: [Formatting Strings](Formatting-Strings.html), Previous: [Text Comparison](Text-Comparison.html), Up: [Strings and Characters](Strings-and-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
