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

Previous: [Advice Combinators](Advice-Combinators.html), Up: [Advising Functions](Advising-Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 13.11.4 Adapting code using the old defadvice

A lot of code uses the old `defadvice` mechanism, which is largely made obsolete by the new `advice-add`, whose implementation and semantics is significantly simpler.

An old piece of advice such as:

    (defadvice previous-line (before next-line-at-end
                                     (&optional arg try-vscroll))
      "Insert an empty line when moving up from the top line."
      (if (and next-line-add-newlines (= arg 1)
               (save-excursion (beginning-of-line) (bobp)))
          (progn
            (beginning-of-line)
            (newline))))

could be translated in the new advice mechanism into a plain function:

    (defun previous-line--next-line-at-end (&optional arg try-vscroll)
      "Insert an empty line when moving up from the top line."
      (if (and next-line-add-newlines (= arg 1)
               (save-excursion (beginning-of-line) (bobp)))
          (progn
            (beginning-of-line)
            (newline))))

Obviously, this does not actually modify `previous-line`. For that the old advice needed:

    (ad-activate 'previous-line)

whereas the new advice mechanism needs:

    (advice-add 'previous-line :before #'previous-line--next-line-at-end)

Note that `ad-activate` had a global effect: it activated all pieces of advice enabled for that specified function. If you wanted to only activate or deactivate a particular piece, you needed to *enable* or *disable* it with `ad-enable-advice` and `ad-disable-advice`. The new mechanism does away with this distinction.

Around advice such as:

    (defadvice foo (around foo-around)
      "Ignore case in `foo'."
      (let ((case-fold-search t))
        ad-do-it))
    (ad-activate 'foo)

could translate into:

    (defun foo--foo-around (orig-fun &rest args)
      "Ignore case in `foo'."
      (let ((case-fold-search t))
        (apply orig-fun args)))
    (advice-add 'foo :around #'foo--foo-around)

Regarding the advice’s *class*, note that the new `:before` is not quite equivalent to the old `before`, because in the old advice you could modify the function’s arguments (e.g., with `ad-set-arg`), and that would affect the argument values seen by the original function, whereas in the new `:before`, modifying an argument via `setq` in the advice has no effect on the arguments seen by the original function. When porting `before` advice which relied on this behavior, you’ll need to turn it into new `:around` or `:filter-args` advice instead.

Similarly old `after` advice could modify the returned value by changing `ad-return-value`, whereas new `:after` advice cannot, so when porting such old `after` advice, you’ll need to turn it into new `:around` or `:filter-return` advice instead.

Previous: [Advice Combinators](Advice-Combinators.html), Up: [Advising Functions](Advising-Functions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
