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

Next: [Searching and Case](Searching-and-Case.html), Up: [Searching and Matching](Searching-and-Matching.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 34.1 Searching for Strings

These are the primitive functions for searching through the text in a buffer. They are meant for use in programs, but you may call them interactively. If you do so, they prompt for the search string; the arguments `limit` and `noerror` are `nil`, and `repeat` is 1. For more details on interactive searching, see [Searching and Replacement](https://www.gnu.org/software/emacs/manual/html_node/emacs/Search.html#Search) in The GNU Emacs Manual.

These search functions convert the search string to multibyte if the buffer is multibyte; they convert the search string to unibyte if the buffer is unibyte. See [Text Representations](Text-Representations.html).

*   Command: **search-forward** *string \&optional limit noerror count*

    This function searches forward from point for an exact match for `string`. If successful, it sets point to the end of the occurrence found, and returns the new value of point. If no match is found, the value and side effects depend on `noerror` (see below).

    In the following example, point is initially at the beginning of the line. Then `(search-forward "fox")` moves point after the last letter of ‘`fox`’:

        ---------- Buffer: foo ----------
        ∗The quick brown fox jumped over the lazy dog.
        ---------- Buffer: foo ----------

    ```
    ```

        (search-forward "fox")
             ⇒ 20

        ---------- Buffer: foo ----------
        The quick brown fox∗ jumped over the lazy dog.
        ---------- Buffer: foo ----------

    The argument `limit` specifies the bound to the search, and should be a position in the current buffer. No match extending after that position is accepted. If `limit` is omitted or `nil`, it defaults to the end of the accessible portion of the buffer.

    What happens when the search fails depends on the value of `noerror`. If `noerror` is `nil`, a `search-failed` error is signaled. If `noerror` is `t`, `search-forward` returns `nil` and does nothing. If `noerror` is neither `nil` nor `t`, then `search-forward` moves point to the upper bound and returns `nil`.

    The argument `noerror` only affects valid searches which fail to find a match. Invalid arguments cause errors regardless of `noerror`.

    If `count` is a positive number `n`, the search is done `n` times; each successive search starts at the end of the previous match. If all these successive searches succeed, the function call succeeds, moving point and returning its new value. Otherwise the function call fails, with results depending on the value of `noerror`, as described above. If `count` is a negative number -`n`, the search is done `n` times in the opposite (backward) direction.

<!---->

*   Command: **search-backward** *string \&optional limit noerror count*

    This function searches backward from point for `string`. It is like `search-forward`, except that it searches backwards rather than forwards. Backward searches leave point at the beginning of the match.

<!---->

*   Command: **word-search-forward** *string \&optional limit noerror count*

    This function searches forward from point for a word match for `string`. If it finds a match, it sets point to the end of the match found, and returns the new value of point.

    Word matching regards `string` as a sequence of words, disregarding punctuation that separates them. It searches the buffer for the same sequence of words. Each word must be distinct in the buffer (searching for the word ‘`ball`’ does not match the word ‘`balls`’), but the details of punctuation and spacing are ignored (searching for ‘`ball boy`’ does match ‘`ball. Boy!`’).

    In this example, point is initially at the beginning of the buffer; the search leaves it between the ‘`y`’ and the ‘`!`’.

        ---------- Buffer: foo ----------
        ∗He said "Please!  Find
        the ball boy!"
        ---------- Buffer: foo ----------

    ```
    ```

        (word-search-forward "Please find the ball, boy.")
             ⇒ 39

        ---------- Buffer: foo ----------
        He said "Please!  Find
        the ball boy∗!"
        ---------- Buffer: foo ----------

    If `limit` is non-`nil`, it must be a position in the current buffer; it specifies the upper bound to the search. The match found must not extend after that position.

    If `noerror` is `nil`, then `word-search-forward` signals an error if the search fails. If `noerror` is `t`, then it returns `nil` instead of signaling an error. If `noerror` is neither `nil` nor `t`, it moves point to `limit` (or the end of the accessible portion of the buffer) and returns `nil`.

    If `count` is a positive number, it specifies how many successive occurrences to search for. Point is positioned at the end of the last match. If `count` is a negative number, the search is backward and point is positioned at the beginning of the last match.

    Internally, `word-search-forward` and related functions use the function `word-search-regexp` to convert `string` to a regular expression that ignores punctuation.

<!---->

*   Command: **word-search-forward-lax** *string \&optional limit noerror count*

    This command is identical to `word-search-forward`, except that the beginning or the end of `string` need not match a word boundary, unless `string` begins or ends in whitespace. For instance, searching for ‘`ball boy`’ matches ‘`ball boyee`’, but does not match ‘`balls boy`’.

<!---->

*   Command: **word-search-backward** *string \&optional limit noerror count*

    This function searches backward from point for a word match to `string`. This function is just like `word-search-forward` except that it searches backward and normally leaves point at the beginning of the match.

<!---->

*   Command: **word-search-backward-lax** *string \&optional limit noerror count*

    This command is identical to `word-search-backward`, except that the beginning or the end of `string` need not match a word boundary, unless `string` begins or ends in whitespace.

Next: [Searching and Case](Searching-and-Case.html), Up: [Searching and Matching](Searching-and-Matching.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
