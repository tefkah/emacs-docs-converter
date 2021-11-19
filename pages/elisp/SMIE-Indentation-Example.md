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

Next: [SMIE Customization](SMIE-Customization.html), Previous: [SMIE Indentation Helpers](SMIE-Indentation-Helpers.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.7.1.8 Sample Indentation Rules

Here is an example of an indentation function:

    (defun sample-smie-rules (kind token)
      (pcase (cons kind token)
        (`(:elem . basic) sample-indent-basic)
        (`(,_ . ",") (smie-rule-separator kind))
        (`(:after . ":=") sample-indent-basic)
        (`(:before . ,(or `"begin" `"(" `"{"))
         (if (smie-rule-hanging-p) (smie-rule-parent)))
        (`(:before . "if")
         (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
              (smie-rule-parent)))))

A few things to note:

*   The first case indicates the basic indentation increment to use. If `sample-indent-basic` is `nil`, then SMIE uses the global setting `smie-indent-basic`. The major mode could have set `smie-indent-basic` buffer-locally instead, but that is discouraged.

*   The rule for the token `","` make SMIE try to be more clever when the comma separator is placed at the beginning of lines. It tries to outdent the separator so as to align the code after the comma; for example:

        x = longfunctionname (
                arg1
              , arg2
            );

*   The rule for indentation after `":="` exists because otherwise SMIE would treat `":="` as an infix operator and would align the right argument with the left one.

*   The rule for indentation before `"begin"` is an example of the use of virtual indentation: This rule is used only when `"begin"` is hanging, which can happen only when `"begin"` is not at the beginning of a line. So this is not used when indenting `"begin"` itself but only when indenting something relative to this `"begin"`. Concretely, this rule changes the indentation from:

            if x > 0 then begin
                    dosomething(x);
                end

    to

            if x > 0 then begin
                dosomething(x);
            end

*   The rule for indentation before `"if"` is similar to the one for `"begin"`, but where the purpose is to treat `"else if"` as a single unit, so as to align a sequence of tests rather than indent each test further to the right. This function does this only in the case where the `"if"` is not placed on a separate line, hence the `smie-rule-bolp` test.

    If we know that the `"else"` is always aligned with its `"if"` and is always at the beginning of a line, we can use a more efficient rule:

        ((equal token "if")
         (and (not (smie-rule-bolp))
              (smie-rule-prev-p "else")
              (save-excursion
                (sample-smie-backward-token)
                (cons 'column (current-column)))))

    The advantage of this formulation is that it reuses the indentation of the previous `"else"`, rather than going all the way back to the first `"if"` of the sequence.

Next: [SMIE Customization](SMIE-Customization.html), Previous: [SMIE Indentation Helpers](SMIE-Indentation-Helpers.html), Up: [SMIE](SMIE.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]