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

Next: [Text Properties](Text-Properties.html), Previous: [Indentation](Indentation.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.18 Case Changes

The case change commands described here work on text in the current buffer. See [Case Conversion](Case-Conversion.html), for case conversion functions that work on strings and characters. See [Case Tables](Case-Tables.html), for how to customize which characters are upper or lower case and how to convert them.

*   Command: **capitalize-region** *start end*

    This function capitalizes all words in the region defined by `start` and `end`. To capitalize means to convert each word’s first character to upper case and convert the rest of each word to lower case. The function returns `nil`.

    If one end of the region is in the middle of a word, the part of the word within the region is treated as an entire word.

    When `capitalize-region` is called interactively, `start` and `end` are point and the mark, with the smallest first.

        ---------- Buffer: foo ----------
        This is the contents of the 5th foo.
        ---------- Buffer: foo ----------

    ```
    ```

        (capitalize-region 1 37)
        ⇒ nil

        ---------- Buffer: foo ----------
        This Is The Contents Of The 5th Foo.
        ---------- Buffer: foo ----------

<!---->

*   Command: **downcase-region** *start end*

    This function converts all of the letters in the region defined by `start` and `end` to lower case. The function returns `nil`.

    When `downcase-region` is called interactively, `start` and `end` are point and the mark, with the smallest first.

<!---->

*   Command: **upcase-region** *start end*

    This function converts all of the letters in the region defined by `start` and `end` to upper case. The function returns `nil`.

    When `upcase-region` is called interactively, `start` and `end` are point and the mark, with the smallest first.

<!---->

*   Command: **capitalize-word** *count*

    This function capitalizes `count` words after point, moving point over as it does. To capitalize means to convert each word’s first character to upper case and convert the rest of each word to lower case. If `count` is negative, the function capitalizes the -`count` previous words but does not move point. The value is `nil`.

    If point is in the middle of a word, the part of the word before point is ignored when moving forward. The rest is treated as an entire word.

    When `capitalize-word` is called interactively, `count` is set to the numeric prefix argument.

<!---->

*   Command: **downcase-word** *count*

    This function converts the `count` words after point to all lower case, moving point over as it does. If `count` is negative, it converts the -`count` previous words but does not move point. The value is `nil`.

    When `downcase-word` is called interactively, `count` is set to the numeric prefix argument.

<!---->

*   Command: **upcase-word** *count*

    This function converts the `count` words after point to all upper case, moving point over as it does. If `count` is negative, it converts the -`count` previous words but does not move point. The value is `nil`.

    When `upcase-word` is called interactively, `count` is set to the numeric prefix argument.

Next: [Text Properties](Text-Properties.html), Previous: [Indentation](Indentation.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
