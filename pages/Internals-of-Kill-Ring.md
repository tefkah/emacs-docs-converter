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

Previous: [Low-Level Kill Ring](Low_002dLevel-Kill-Ring.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.8.6 Internals of the Kill Ring

The variable `kill-ring` holds the kill ring contents, in the form of a list of strings. The most recent kill is always at the front of the list.

The `kill-ring-yank-pointer` variable points to a link in the kill ring list, whose CAR is the text to yank next. We say it identifies the front of the ring. Moving `kill-ring-yank-pointer` to a different link is called *rotating the kill ring*. We call the kill ring a “ring” because the functions that move the yank pointer wrap around from the end of the list to the beginning, or vice-versa. Rotation of the kill ring is virtual; it does not change the value of `kill-ring`.

Both `kill-ring` and `kill-ring-yank-pointer` are Lisp variables whose values are normally lists. The word “pointer” in the name of the `kill-ring-yank-pointer` indicates that the variable’s purpose is to identify one element of the list for use by the next yank command.

The value of `kill-ring-yank-pointer` is always `eq` to one of the links in the kill ring list. The element it identifies is the CAR of that link. Kill commands, which change the kill ring, also set this variable to the value of `kill-ring`. The effect is to rotate the ring so that the newly killed text is at the front.

Here is a diagram that shows the variable `kill-ring-yank-pointer` pointing to the second entry in the kill ring `("some text" "a different piece of text" "yet older text")`.

    kill-ring                  ---- kill-ring-yank-pointer
      |                       |
      |                       v
      |     --- ---          --- ---      --- ---
       --> |   |   |------> |   |   |--> |   |   |--> nil
            --- ---          --- ---      --- ---
             |                |            |
             |                |            |
             |                |             -->"yet older text"
             |                |
             |                 --> "a different piece of text"
             |
              --> "some text"

This state of affairs might occur after `C-y` (`yank`) immediately followed by `M-y` (`yank-pop`).

*   Variable: **kill-ring**

    This variable holds the list of killed text sequences, most recently killed first.

<!---->

*   Variable: **kill-ring-yank-pointer**

    This variable’s value indicates which element of the kill ring is at the front of the ring for yanking. More precisely, the value is a tail of the value of `kill-ring`, and its CAR is the kill string that `C-y` should yank.

<!---->

*   User Option: **kill-ring-max**

    The value of this variable is the maximum length to which the kill ring can grow, before elements are thrown away at the end. The default value for `kill-ring-max` is 60.

Previous: [Low-Level Kill Ring](Low_002dLevel-Kill-Ring.html), Up: [The Kill Ring](The-Kill-Ring.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
