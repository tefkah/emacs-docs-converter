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

Next: [Backward Compatibility](Backward-Compatibility.html), Up: [Records](Records.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 7.1 Record Functions

*   Function: **recordp** *object*

    This function returns `t` if `object` is a record.

        (recordp #s(a))
             ⇒ t

<!---->

*   Function: **record** *type \&rest objects*

    This function creates and returns a record whose type is `type` and remaining slots are the rest of the arguments, `objects`.

        (record 'foo 23 [bar baz] "rats")
             ⇒ #s(foo 23 [bar baz] "rats")

<!---->

*   Function: **make-record** *type length object*

    This function returns a new record with type `type` and `length` more slots, each initialized to `object`.

        (setq sleepy (make-record 'foo 9 'Z))
             ⇒ #s(foo Z Z Z Z Z Z Z Z Z)
