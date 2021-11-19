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

Previous: [Rx Notation](Rx-Notation.html), Up: [Regular Expressions](Regular-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.3.4 Regular Expression Functions

These functions operate on regular expressions.

*   Function: **regexp-quote** *string*

    This function returns a regular expression whose only exact match is `string`. Using this regular expression in `looking-at` will succeed only if the next characters in the buffer are `string`; using it in a search function will succeed if the text being searched contains `string`. See [Regexp Search](Regexp-Search.html).

    This allows you to request an exact string match or search when calling a function that wants a regular expression.

        (regexp-quote "^The cat$")
             ⇒ "\\^The cat\\$"

    One use of `regexp-quote` is to combine an exact string match with context described as a regular expression. For example, this searches for the string that is the value of `string`, surrounded by whitespace:

        (re-search-forward
         (concat "\\s-" (regexp-quote string) "\\s-"))

    The returned string may be `string` itself if it does not contain any special characters.

<!---->

*   Function: **regexp-opt** *strings \&optional paren*

    This function returns an efficient regular expression that will match any of the strings in the list `strings`. This is useful when you need to make matching or searching as fast as possible—for example, for Font Lock mode[20](#FOOT20).

    If `strings` is the empty list, the return value is a regexp that never matches anything.

    The optional argument `paren` can be any of the following:

    *   a string

        The resulting regexp is preceded by `paren` and followed by ‘`\)`’, e.g. use ‘`"\\(?1:"`’ to produce an explicitly numbered group.

    *   `words`

        The resulting regexp is surrounded by ‘`\<\(`’ and ‘`\)\>`’.

    *   `symbols`

        The resulting regexp is surrounded by ‘`\_<\(`’ and ‘`\)\_>`’ (this is often appropriate when matching programming-language keywords and the like).

    *   non-`nil`

        The resulting regexp is surrounded by ‘`\(`’ and ‘`\)`’.

    *   `nil`

        The resulting regexp is surrounded by ‘`\(?:`’ and ‘`\)`’, if it is necessary to ensure that a postfix operator appended to it will apply to the whole expression.

    The returned regexp is ordered in such a way that it will always match the longest string possible.

    Up to reordering, the resulting regexp of `regexp-opt` is equivalent to but usually more efficient than that of a simplified version:

        (defun simplified-regexp-opt (strings &optional paren)
         (let ((parens
                (cond
                 ((stringp paren)       (cons paren "\\)"))
                 ((eq paren 'words)    '("\\<\\(" . "\\)\\>"))
                 ((eq paren 'symbols) '("\\_<\\(" . "\\)\\_>"))
                 ((null paren)          '("\\(?:" . "\\)"))
                 (t                       '("\\(" . "\\)")))))
           (concat (car parens)
                   (mapconcat 'regexp-quote strings "\\|")
                   (cdr parens))))

<!---->

*   Function: **regexp-opt-depth** *regexp*

    This function returns the total number of grouping constructs (parenthesized expressions) in `regexp`. This does not include shy groups (see [Regexp Backslash](Regexp-Backslash.html)).

<!---->

*   Function: **regexp-opt-charset** *chars*

    This function returns a regular expression matching a character in the list of characters `chars`.

        (regexp-opt-charset '(?a ?b ?c ?d ?e))
             ⇒ "[a-e]"

<!---->

*   Variable: **regexp-unmatchable**

    This variable contains a regexp that is guaranteed not to match any string at all. It is particularly useful as default value for variables that may be set to a pattern that actually matches something.

***

#### Footnotes

##### [(20)](#DOCF20)

Note that `regexp-opt` does not guarantee that its result is absolutely the most efficient form possible. A hand-tuned regular expression can sometimes be slightly more efficient, but is almost never worth the effort.

Previous: [Rx Notation](Rx-Notation.html), Up: [Regular Expressions](Regular-Expressions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
