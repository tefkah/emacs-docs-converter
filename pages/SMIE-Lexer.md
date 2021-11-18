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

Next: [SMIE Tricks](SMIE-Tricks.html), Previous: [SMIE Grammar](SMIE-Grammar.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.7.1.4 Defining Tokens

SMIE comes with a predefined lexical analyzer which uses syntax tables in the following way: any sequence of characters that have word or symbol syntax is considered a token, and so is any sequence of characters that have punctuation syntax. This default lexer is often a good starting point but is rarely actually correct for any given language. For example, it will consider `"2,+3"` to be composed of 3 tokens: `"2"`, `",+"`, and `"3"`.

To describe the lexing rules of your language to SMIE, you need 2 functions, one to fetch the next token, and another to fetch the previous token. Those functions will usually first skip whitespace and comments and then look at the next chunk of text to see if it is a special token. If so it should skip the token and return a description of this token. Usually this is simply the string extracted from the buffer, but it can be anything you want. For example:

    (defvar sample-keywords-regexp
      (regexp-opt '("+" "*" "," ";" ">" ">=" "<" "<=" ":=" "=")))

<!---->

    (defun sample-smie-forward-token ()
      (forward-comment (point-max))
      (cond
       ((looking-at sample-keywords-regexp)
        (goto-char (match-end 0))
        (match-string-no-properties 0))
       (t (buffer-substring-no-properties
           (point)
           (progn (skip-syntax-forward "w_")
                  (point))))))

<!---->

    (defun sample-smie-backward-token ()
      (forward-comment (- (point)))
      (cond
       ((looking-back sample-keywords-regexp (- (point) 2) t)
        (goto-char (match-beginning 0))
        (match-string-no-properties 0))
       (t (buffer-substring-no-properties
           (point)
           (progn (skip-syntax-backward "w_")
                  (point))))))

Notice how those lexers return the empty string when in front of parentheses. This is because SMIE automatically takes care of the parentheses defined in the syntax table. More specifically if the lexer returns `nil` or an empty string, SMIE tries to handle the corresponding text as a sexp according to syntax tables.

Next: [SMIE Tricks](SMIE-Tricks.html), Previous: [SMIE Grammar](SMIE-Grammar.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
