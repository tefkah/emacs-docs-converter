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

Next: [Marker Insertion Types](Marker-Insertion-Types.html), Previous: [Creating Markers](Creating-Markers.html), Up: [Markers](Markers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 31.4 Information from Markers

This section describes the functions for accessing the components of a marker object.

*   Function: **marker-position** *marker*

    This function returns the position that `marker` points to, or `nil` if it points nowhere.

<!---->

*   Function: **marker-buffer** *marker*

    This function returns the buffer that `marker` points into, or `nil` if it points nowhere.

        (setq m (make-marker))
             ⇒ #<marker in no buffer>

    <!---->

        (marker-position m)
             ⇒ nil

    <!---->

        (marker-buffer m)
             ⇒ nil

    ```
    ```

        (set-marker m 3770 (current-buffer))
             ⇒ #<marker at 3770 in markers.texi>

    <!---->

        (marker-buffer m)
             ⇒ #<buffer markers.texi>

    <!---->

        (marker-position m)
             ⇒ 3770
