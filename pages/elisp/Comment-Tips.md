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

Next: [Library Headers](Library-Headers.html), Previous: [Documentation Tips](Documentation-Tips.html), Up: [Tips](Tips.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### D.7 Tips on Writing Comments

We recommend these conventions for comments:

*   ‘`;`’

    Comments that start with a single semicolon, ‘`;`’, should all be aligned to the same column on the right of the source code. Such comments usually explain how the code on that line does its job. For example:

        (setq base-version-list                 ; There was a base
              (assoc (substring fn 0 start-vn)  ; version to which
                     file-version-assoc-list))  ; this looks like
                                                ; a subversion.

*   ‘`;;`’

    Comments that start with two semicolons, ‘`;;`’, should be aligned to the same level of indentation as the code. Such comments usually describe the purpose of the following lines or the state of the program at that point. For example:

        (prog1 (setq auto-fill-function
                     …
                     …
          ;; Update mode line.
          (force-mode-line-update)))

    We also normally use two semicolons for comments outside functions.

        ;; This Lisp code is run in Emacs when it is to operate as
        ;; a server for other processes.

    If a function has no documentation string, it should instead have a two-semicolon comment right before the function, explaining what the function does and how to call it properly. Explain precisely what each argument means and how the function interprets its possible values. It is much better to convert such comments to documentation strings, though.

*   ‘`;;;`’

    Comments that start with three semicolons, ‘`;;;`’, should start at the left margin. We use them for comments which should be considered a heading by Outline minor mode. By default, comments starting with at least three semicolons (followed by a single space and a non-whitespace character) are considered headings, comments starting with two or fewer are not. Historically, triple-semicolon comments have also been used for commenting out lines within a function, but this use is discouraged.

    When commenting out entire functions, use two semicolons.

*   ‘`;;;;`’

    Comments that start with four (or more) semicolons, ‘`;;;;`’, should be aligned to the left margin and are used for headings of major sections of a program. For example:

        ;;;; The kill ring

    If you wish to have sub-headings under these heading, use more semicolons to nest these sub-headings.

Generally speaking, the `M-;` (`comment-dwim`) command automatically starts a comment of the appropriate type; or indents an existing comment to the right place, depending on the number of semicolons. See [Manipulating Comments](https://www.gnu.org/software/emacs/manual/html_node/emacs/Comments.html#Comments) in The GNU Emacs Manual.

Next: [Library Headers](Library-Headers.html), Previous: [Documentation Tips](Documentation-Tips.html), Up: [Tips](Tips.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
