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

Next: [Character Display](Character-Display.html), Previous: [Abstract Display](Abstract-Display.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 39.21 Blinking Parentheses

This section describes the mechanism by which Emacs shows a matching open parenthesis when the user inserts a close parenthesis.

*   Variable: **blink-paren-function**

    The value of this variable should be a function (of no arguments) to be called whenever a character with close parenthesis syntax is inserted. The value of `blink-paren-function` may be `nil`, in which case nothing is done.

<!---->

*   User Option: **blink-matching-paren**

    If this variable is `nil`, then `blink-matching-open` does nothing.

<!---->

*   User Option: **blink-matching-paren-distance**

    This variable specifies the maximum distance to scan for a matching parenthesis before giving up.

<!---->

*   User Option: **blink-matching-delay**

    This variable specifies the number of seconds to keep indicating the matching parenthesis. A fraction of a second often gives good results, but the default is 1, which works on all systems.

<!---->

*   Command: **blink-matching-open**

    This function is the default value of `blink-paren-function`. It assumes that point follows a character with close parenthesis syntax and applies the appropriate effect momentarily to the matching opening character. If that character is not already on the screen, it displays the character’s context in the echo area. To avoid long delays, this function does not search farther than `blink-matching-paren-distance` characters.

    Here is an example of calling this function explicitly.

        (defun interactive-blink-matching-open ()
          "Indicate momentarily the start of parenthesized sexp before point."
          (interactive)

    <!---->

          (let ((blink-matching-paren-distance
                 (buffer-size))
                (blink-matching-paren t))
            (blink-matching-open)))
