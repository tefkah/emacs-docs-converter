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

Next: [Minibuffer Windows](Minibuffer-Windows.html), Previous: [Reading a Password](Reading-a-Password.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.10 Minibuffer Commands

This section describes some commands meant for use in the minibuffer.

*   Command: **exit-minibuffer**

    This command exits the active minibuffer. It is normally bound to keys in minibuffer local keymaps.

<!---->

*   Command: **self-insert-and-exit**

    This command exits the active minibuffer after inserting the last character typed on the keyboard (found in `last-command-event`; see [Command Loop Info](Command-Loop-Info.html)).

<!---->

*   Command: **previous-history-element** *n*

    This command replaces the minibuffer contents with the value of the `n`th previous (older) history element.

<!---->

*   Command: **next-history-element** *n*

    This command replaces the minibuffer contents with the value of the `n`th more recent history element. The position in the history can go beyond the current position and invoke “future history” (see [Text from Minibuffer](Text-from-Minibuffer.html)).

<!---->

*   Command: **previous-matching-history-element** *pattern n*

    This command replaces the minibuffer contents with the value of the `n`th previous (older) history element that matches `pattern` (a regular expression).

<!---->

*   Command: **next-matching-history-element** *pattern n*

    This command replaces the minibuffer contents with the value of the `n`th next (newer) history element that matches `pattern` (a regular expression).

<!---->

*   Command: **previous-complete-history-element** *n*

    This command replaces the minibuffer contents with the value of the `n`th previous (older) history element that completes the current contents of the minibuffer before the point.

<!---->

*   Command: **next-complete-history-element** *n*

    This command replaces the minibuffer contents with the value of the `n`th next (newer) history element that completes the current contents of the minibuffer before the point.

<!---->

*   Command: **goto-history-element** *nabs*

    This function puts element of the minibuffer history in the minibuffer. The argument `nabs` specifies the absolute history position in descending order, where 0 means the current element and a positive number `n` means the `n`th previous element. NABS being a negative number -`n` means the `n`th entry of “future history.”

Next: [Minibuffer Windows](Minibuffer-Windows.html), Previous: [Reading a Password](Reading-a-Password.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
