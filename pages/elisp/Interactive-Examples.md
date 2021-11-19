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

Next: [Generic Commands](Generic-Commands.html), Previous: [Interactive Codes](Interactive-Codes.html), Up: [Defining Commands](Defining-Commands.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.2.3 Examples of Using `interactive`

Here are some examples of `interactive`:

    (defun foo1 ()              ; foo1 takes no arguments,
        (interactive)           ;   just moves forward two words.
        (forward-word 2))
         ⇒ foo1

```
```

    (defun foo2 (n)             ; foo2 takes one argument,
        (interactive "^p")      ;   which is the numeric prefix.
                                ; under shift-select-mode,
                                ;   will activate or extend region.
        (forward-word (* 2 n)))
         ⇒ foo2

```
```

    (defun foo3 (n)             ; foo3 takes one argument,
        (interactive "nCount:") ;   which is read with the Minibuffer.
        (forward-word (* 2 n)))
         ⇒ foo3

```
```

    (defun three-b (b1 b2 b3)
      "Select three existing buffers.
    Put them into three windows, selecting the last one."

<!---->

        (interactive "bBuffer1:\nbBuffer2:\nbBuffer3:")
        (delete-other-windows)
        (split-window (selected-window) 8)
        (switch-to-buffer b1)
        (other-window 1)
        (split-window (selected-window) 8)
        (switch-to-buffer b2)
        (other-window 1)
        (switch-to-buffer b3))
         ⇒ three-b

<!---->

    (three-b "*scratch*" "declarations.texi" "*mail*")
         ⇒ nil
