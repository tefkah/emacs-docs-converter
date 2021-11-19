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

Previous: [Warning Options](Warning-Options.html), Up: [Warnings](Warnings.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.5.4 Delayed Warnings

Sometimes, you may wish to avoid showing a warning while a command is running, and only show it only after the end of the command. You can use the function `delay-warning` for this.

*   Function: **delay-warning** *type message \&optional level buffer-name*

    This function is the delayed counterpart to `display-warning` (see [Warning Basics](Warning-Basics.html)), and it is called with the same arguments. The warning message is queued into `delayed-warnings-list`.

<!---->

*   Variable: **delayed-warnings-list**

    The value of this variable is a list of warnings to be displayed after the current command has finished. Each element must be a list

        (type message [level [buffer-name]])

    with the same form, and the same meanings, as the argument list of `display-warning`. Immediately after running `post-command-hook` (see [Command Overview](Command-Overview.html)), the Emacs command loop displays all the warnings specified by this variable, then resets it to `nil`.

Programs which need to further customize the delayed warnings mechanism can change the variable `delayed-warnings-hook`:

*   Variable: **delayed-warnings-hook**

    This is a normal hook which is run by the Emacs command loop, after `post-command-hook`, in order to process and display delayed warnings.

    Its default value is a list of two functions:

        (collapse-delayed-warnings display-delayed-warnings)

    The function `collapse-delayed-warnings` removes repeated entries from `delayed-warnings-list`. The function `display-delayed-warnings` calls `display-warning` on each of the entries in `delayed-warnings-list`, in turn, and then sets `delayed-warnings-list` to `nil`.
