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

Next: [Case Tables](Case-Tables.html), Previous: [Custom Format Strings](Custom-Format-Strings.html), Up: [Strings and Characters](Strings-and-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 4.9 Case Conversion in Lisp

The character case functions change the case of single characters or of the contents of strings. The functions normally convert only alphabetic characters (the letters ‘`A`’ through ‘`Z`’ and ‘`a`’ through ‘`z`’, as well as non-ASCII letters); other characters are not altered. You can specify a different case conversion mapping by specifying a case table (see [Case Tables](Case-Tables.html)).

These functions do not modify the strings that are passed to them as arguments.

The examples below use the characters ‘`X`’ and ‘`x`’ which have ASCII codes 88 and 120 respectively.

*   Function: **downcase** *string-or-char*

    This function converts `string-or-char`, which should be either a character or a string, to lower case.

    When `string-or-char` is a string, this function returns a new string in which each letter in the argument that is upper case is converted to lower case. When `string-or-char` is a character, this function returns the corresponding lower case character (an integer); if the original character is lower case, or is not a letter, the return value is equal to the original character.

        (downcase "The cat in the hat")
             ⇒ "the cat in the hat"

        (downcase ?X)
             ⇒ 120

<!---->

*   Function: **upcase** *string-or-char*

    This function converts `string-or-char`, which should be either a character or a string, to upper case.

    When `string-or-char` is a string, this function returns a new string in which each letter in the argument that is lower case is converted to upper case. When `string-or-char` is a character, this function returns the corresponding upper case character (an integer); if the original character is upper case, or is not a letter, the return value is equal to the original character.

        (upcase "The cat in the hat")
             ⇒ "THE CAT IN THE HAT"

        (upcase ?x)
             ⇒ 88

<!---->

*   Function: **capitalize** *string-or-char*

    This function capitalizes strings or characters. If `string-or-char` is a string, the function returns a new string whose contents are a copy of `string-or-char` in which each word has been capitalized. This means that the first character of each word is converted to upper case, and the rest are converted to lower case.

    The definition of a word is any sequence of consecutive characters that are assigned to the word constituent syntax class in the current syntax table (see [Syntax Class Table](Syntax-Class-Table.html)).

    When `string-or-char` is a character, this function does the same thing as `upcase`.

        (capitalize "The cat in the hat")
             ⇒ "The Cat In The Hat"

    ```
    ```

        (capitalize "THE 77TH-HATTED CAT")
             ⇒ "The 77th-Hatted Cat"

    ```
    ```

        (capitalize ?x)
             ⇒ 88

<!---->

*   Function: **upcase-initials** *string-or-char*

    If `string-or-char` is a string, this function capitalizes the initials of the words in `string-or-char`, without altering any letters other than the initials. It returns a new string whose contents are a copy of `string-or-char`, in which each word has had its initial letter converted to upper case.

    The definition of a word is any sequence of consecutive characters that are assigned to the word constituent syntax class in the current syntax table (see [Syntax Class Table](Syntax-Class-Table.html)).

    When the argument to `upcase-initials` is a character, `upcase-initials` has the same result as `upcase`.

        (upcase-initials "The CAT in the hAt")
             ⇒ "The CAT In The HAt"

Note that case conversion is not a one-to-one mapping of codepoints and length of the result may differ from length of the argument. Furthermore, because passing a character forces return type to be a character, functions are unable to perform proper substitution and result may differ compared to treating a one-character string. For example:

    (upcase "ﬁ")  ; note: single character, ligature "fi"
         ⇒ "FI"

<!---->

    (upcase ?ﬁ)
         ⇒ 64257  ; i.e. ?ﬁ

To avoid this, a character must first be converted into a string, using `string` function, before being passed to one of the casing functions. Of course, no assumptions on the length of the result may be made.

Mapping for such special cases are taken from `special-uppercase`, `special-lowercase` and `special-titlecase` See [Character Properties](Character-Properties.html).

See [Text Comparison](Text-Comparison.html), for functions that compare strings; some of them ignore case differences, or can optionally ignore case differences.

Next: [Case Tables](Case-Tables.html), Previous: [Custom Format Strings](Custom-Format-Strings.html), Up: [Strings and Characters](Strings-and-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
