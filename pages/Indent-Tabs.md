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

Next: [Motion by Indent](Motion-by-Indent.html), Previous: [Relative Indent](Relative-Indent.html), Up: [Indentation](Indentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.17.5 Adjustable Tab Stops

This section explains the mechanism for user-specified tab stops and the mechanisms that use and set them. The name “tab stops” is used because the feature is similar to that of the tab stops on a typewriter. The feature works by inserting an appropriate number of spaces and tab characters to reach the next tab stop column; it does not affect the display of tab characters in the buffer (see [Usual Display](Usual-Display.html)). Note that the `TAB` character as input uses this tab stop feature only in a few major modes, such as Text mode. See [Tab Stops](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Stops.html#Tab-Stops) in The GNU Emacs Manual.

*   Command: **tab-to-tab-stop**

    This command inserts spaces or tabs before point, up to the next tab stop column defined by `tab-stop-list`.

<!---->

*   User Option: **tab-stop-list**

    This variable defines the tab stop columns used by `tab-to-tab-stop`. It should be either `nil`, or a list of increasing integers, which need not be evenly spaced. The list is implicitly extended to infinity through repetition of the interval between the last and penultimate elements (or `tab-width` if the list has fewer than two elements). A value of `nil` means a tab stop every `tab-width` columns.

    Use `M-x edit-tab-stops` to edit the location of tab stops interactively.