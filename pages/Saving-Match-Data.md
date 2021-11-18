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

Previous: [Entire Match Data](Entire-Match-Data.html), Up: [Match Data](Match-Data.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.6.4 Saving and Restoring the Match Data

When you call a function that may search, you may need to save and restore the match data around that call, if you want to preserve the match data from an earlier search for later use. Here is an example that shows the problem that arises if you fail to save the match data:

    (re-search-forward "The \\(cat \\)")
         ⇒ 48
    (foo)                   ; foo does more searching.
    (match-end 0)
         ⇒ 61              ; Unexpected result—not 48!

You can save and restore the match data with `save-match-data`:

*   Macro: **save-match-data** *body…*

    This macro executes `body`, saving and restoring the match data around it. The return value is the value of the last form in `body`.

You could use `set-match-data` together with `match-data` to imitate the effect of the special form `save-match-data`. Here is how:

    (let ((data (match-data)))
      (unwind-protect
          …   ; Ok to change the original match data.
        (set-match-data data)))

Emacs automatically saves and restores the match data when it runs process filter functions (see [Filter Functions](Filter-Functions.html)) and process sentinels (see [Sentinels](Sentinels.html)).
