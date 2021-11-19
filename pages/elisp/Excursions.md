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

Next: [Narrowing](Narrowing.html), Previous: [Motion](Motion.html), Up: [Positions](Positions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 30.3 Excursions

It is often useful to move point temporarily within a localized portion of the program. This is called an *excursion*, and it is done with the `save-excursion` special form. This construct remembers the initial identity of the current buffer, and its value of point, and restores them after the excursion completes. It is the standard way to move point within one part of a program and avoid affecting the rest of the program, and is used thousands of times in the Lisp sources of Emacs.

If you only need to save and restore the identity of the current buffer, use `save-current-buffer` or `with-current-buffer` instead (see [Current Buffer](Current-Buffer.html)). If you need to save or restore window configurations, see the forms described in [Window Configurations](Window-Configurations.html) and in [Frame Configurations](Frame-Configurations.html).

*   Special Form: **save-excursion** *body…*

    This special form saves the identity of the current buffer and the value of point in it, evaluates `body`, and finally restores the buffer and its saved value of point. Both saved values are restored even in case of an abnormal exit via `throw` or error (see [Nonlocal Exits](Nonlocal-Exits.html)).

    The value returned by `save-excursion` is the result of the last form in `body`, or `nil` if no body forms were given.

Because `save-excursion` only saves point for the buffer that was current at the start of the excursion, any changes made to point in other buffers, during the excursion, will remain in effect afterward. This frequently leads to unintended consequences, so the byte compiler warns if you call `set-buffer` during an excursion:

    Warning: Use ‘with-current-buffer’ rather than
             save-excursion+set-buffer

To avoid such problems, you should call `save-excursion` only after setting the desired current buffer, as in the following example:

    (defun append-string-to-buffer (string buffer)
      "Append STRING to the end of BUFFER."
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (insert string))))

Likewise, `save-excursion` does not restore window-buffer correspondences altered by functions such as `switch-to-buffer`.

**Warning:** Ordinary insertion of text adjacent to the saved point value relocates the saved value, just as it relocates all markers. More precisely, the saved value is a marker with insertion type `nil`. See [Marker Insertion Types](Marker-Insertion-Types.html). Therefore, when the saved point value is restored, it normally comes before the inserted text.

*   Macro: **save-mark-and-excursion** *body…*

    This macro is like `save-excursion`, but also saves and restores the mark location and `mark-active`. This macro does what `save-excursion` did before Emacs 25.1.

Next: [Narrowing](Narrowing.html), Previous: [Motion](Motion.html), Up: [Positions](Positions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]