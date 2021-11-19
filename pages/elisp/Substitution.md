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

Next: [Registers](Registers.html), Previous: [Text Properties](Text-Properties.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.20 Substituting for a Character Code

The following functions replace characters within a specified region based on their character codes.

*   Function: **subst-char-in-region** *start end old-char new-char \&optional noundo*

    This function replaces all occurrences of the character `old-char` with the character `new-char` in the region of the current buffer defined by `start` and `end`.

    If `noundo` is non-`nil`, then `subst-char-in-region` does not record the change for undo and does not mark the buffer as modified. This was useful for controlling the old selective display feature (see [Selective Display](Selective-Display.html)).

    `subst-char-in-region` does not move point and returns `nil`.

        ---------- Buffer: foo ----------
        This is the contents of the buffer before.
        ---------- Buffer: foo ----------

    ```
    ```

        (subst-char-in-region 1 20 ?i ?X)
             ⇒ nil

        ---------- Buffer: foo ----------
        ThXs Xs the contents of the buffer before.
        ---------- Buffer: foo ----------

<!---->

*   Command: **translate-region** *start end table*

    This function applies a translation table to the characters in the buffer between positions `start` and `end`.

    The translation table `table` is a string or a char-table; `(aref table ochar)` gives the translated character corresponding to `ochar`. If `table` is a string, any characters with codes larger than the length of `table` are not altered by the translation.

    The return value of `translate-region` is the number of characters that were actually changed by the translation. This does not count characters that were mapped into themselves in the translation table.
