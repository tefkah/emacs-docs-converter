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

Previous: [List Motion](List-Motion.html), Up: [Motion](Motion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 30.2.7 Skipping Characters

The following two functions move point over a specified set of characters. For example, they are often used to skip whitespace. For related functions, see [Motion and Syntax](Motion-and-Syntax.html).

These functions convert the set string to multibyte if the buffer is multibyte, and they convert it to unibyte if the buffer is unibyte, as the search functions do (see [Searching and Matching](Searching-and-Matching.html)).

*   Function: **skip-chars-forward** *character-set \&optional limit*

    This function moves point in the current buffer forward, skipping over a given set of characters. It examines the character following point, then advances point if the character matches `character-set`. This continues until it reaches a character that does not match. The function returns the number of characters moved over.

    The argument `character-set` is a string, like the inside of a ‘`[…]`’ in a regular expression except that ‘`]`’ does not terminate it, and ‘`\`’ quotes ‘`^`’, ‘`-`’ or ‘`\`’. Thus, `"a-zA-Z"` skips over all letters, stopping before the first nonletter, and `"^a-zA-Z"` skips nonletters stopping before the first letter (see [Regular Expressions](Regular-Expressions.html)). Character classes can also be used, e.g., `"[:alnum:]"` (see [Char Classes](Char-Classes.html)).

    If `limit` is supplied (it must be a number or a marker), it specifies the maximum position in the buffer that point can be skipped to. Point will stop at or before `limit`.

    In the following example, point is initially located directly before the ‘`T`’. After the form is evaluated, point is located at the end of that line (between the ‘`t`’ of ‘`hat`’ and the newline). The function skips all letters and spaces, but not newlines.

        ---------- Buffer: foo ----------
        I read "∗The cat in the hat
        comes back" twice.
        ---------- Buffer: foo ----------

    ```
    ```

        (skip-chars-forward "a-zA-Z ")
             ⇒ 18

        ---------- Buffer: foo ----------
        I read "The cat in the hat∗
        comes back" twice.
        ---------- Buffer: foo ----------

<!---->

*   Function: **skip-chars-backward** *character-set \&optional limit*

    This function moves point backward, skipping characters that match `character-set`, until `limit`. It is just like `skip-chars-forward` except for the direction of motion.

    The return value indicates the distance traveled. It is an integer that is zero or less.

Previous: [List Motion](List-Motion.html), Up: [Motion](Motion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
