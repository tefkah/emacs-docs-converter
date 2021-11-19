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

Next: [Acknowledgments](Acknowledgments.html), Previous: [Conventions](Conventions.html), Up: [Introduction](Introduction.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 1.4 Version Information

These facilities provide information about which version of Emacs is in use.

*   Command: **emacs-version** *\&optional here*

    This function returns a string describing the version of Emacs that is running. It is useful to include this string in bug reports.

        (emacs-version)
          ⇒ "GNU Emacs 26.1 (build 1, x86_64-unknown-linux-gnu,
                     GTK+ Version 3.16) of 2017-06-01"

    If `here` is non-`nil`, it inserts the text in the buffer before point, and returns `nil`. When this function is called interactively, it prints the same information in the echo area, but giving a prefix argument makes `here` non-`nil`.

<!---->

*   Variable: **emacs-build-time**

    The value of this variable indicates the time at which Emacs was built. It uses the style of `current-time` (see [Time of Day](Time-of-Day.html)), or is `nil` if the information is not available.

        emacs-build-time
             ⇒ (20614 63694 515336 438000)

<!---->

*   Variable: **emacs-version**

    The value of this variable is the version of Emacs being run. It is a string such as `"26.1"`. A value with three numeric components, such as `"26.0.91"`, indicates an unreleased test version. (Prior to Emacs 26.1, the string includes an extra final component with the integer that is now stored in `emacs-build-number`; e.g., `"25.1.1"`.)

<!---->

*   Variable: **emacs-major-version**

    The major version number of Emacs, as an integer. For Emacs version 23.1, the value is 23.

<!---->

*   Variable: **emacs-minor-version**

    The minor version number of Emacs, as an integer. For Emacs version 23.1, the value is 1.

<!---->

*   Variable: **emacs-build-number**

    An integer that increments each time Emacs is built in the same directory (without cleaning). This is only of relevance when developing Emacs.

<!---->

*   Variable: **emacs-repository-version**

    A string that gives the repository revision from which Emacs was built. If Emacs was built outside revision control, the value is `nil`.

<!---->

*   Variable: **emacs-repository-branch**

    A string that gives the repository branch from which Emacs was built. In the most cases this is `"master"`. If Emacs was built outside revision control, the value is `nil`.

Next: [Acknowledgments](Acknowledgments.html), Previous: [Conventions](Conventions.html), Up: [Introduction](Introduction.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
