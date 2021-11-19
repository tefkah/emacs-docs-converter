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

Next: [Mode Line Variables](Mode-Line-Variables.html), Previous: [Mode Line Data](Mode-Line-Data.html), Up: [Mode Line Format](Mode-Line-Format.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.4.3 The Top Level of Mode Line Control

The variable in overall control of the mode line is `mode-line-format`.

*   User Option: **mode-line-format**

    The value of this variable is a mode line construct that controls the contents of the mode-line. It is always buffer-local in all buffers.

    If you set this variable to `nil` in a buffer, that buffer does not have a mode line. (A window that is just one line tall also does not display a mode line.)

The default value of `mode-line-format` is designed to use the values of other variables such as `mode-line-position` and `mode-line-modes` (which in turn incorporates the values of the variables `mode-name` and `minor-mode-alist`). Very few modes need to alter `mode-line-format` itself. For most purposes, it is sufficient to alter some of the variables that `mode-line-format` either directly or indirectly refers to.

If you do alter `mode-line-format` itself, the new value should use the same variables that appear in the default value (see [Mode Line Variables](Mode-Line-Variables.html)), rather than duplicating their contents or displaying the information in another fashion. This way, customizations made by the user or by Lisp programs (such as `display-time` and major modes) via changes to those variables remain effective.

Here is a hypothetical example of a `mode-line-format` that might be useful for Shell mode (in reality, Shell mode does not set `mode-line-format`):

    (setq mode-line-format
      (list "-"
       'mode-line-mule-info
       'mode-line-modified
       'mode-line-frame-identification
       "%b--"

<!---->

       ;; Note that this is evaluated while making the list.
       ;; It makes a mode line construct which is just a string.
       (getenv "HOST")

<!---->

       ":"
       'default-directory
       "   "
       'global-mode-string
       "   %[("
       '(:eval (format-time-string "%F"))
       'mode-line-process
       'minor-mode-alist
       "%n"
       ")%]--"

<!---->

       '(which-function-mode ("" which-func-format "--"))
       '(line-number-mode "L%l--")
       '(column-number-mode "C%c--")
       '(-3 "%p")))

(The variables `line-number-mode`, `column-number-mode` and `which-function-mode` enable particular minor modes; as usual, these variable names are also the minor mode command names.)

Next: [Mode Line Variables](Mode-Line-Variables.html), Previous: [Mode Line Data](Mode-Line-Data.html), Up: [Mode Line Format](Mode-Line-Format.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
