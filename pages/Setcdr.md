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

Next: [Rearrangement](Rearrangement.html), Previous: [Setcar](Setcar.html), Up: [Modifying Lists](Modifying-Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 5.6.2 Altering the CDR of a List

The lowest-level primitive for modifying a CDR is `setcdr`:

*   Function: **setcdr** *cons object*

    This function stores `object` as the new CDR of `cons`, replacing its previous CDR. In other words, it changes the CDR slot of `cons` to refer to `object`. It returns the value `object`.

Here is an example of replacing the CDR of a list with a different list. All but the first element of the list are removed in favor of a different sequence of elements. The first element is unchanged, because it resides in the CAR of the list, and is not reached via the CDR.

    (setq x (list 1 2 3))
         ⇒ (1 2 3)

<!---->

    (setcdr x '(4))
         ⇒ (4)

<!---->

    x
         ⇒ (1 4)

You can delete elements from the middle of a list by altering the CDRs of the cons cells in the list. For example, here we delete the second element, `b`, from the list `(a b c)`, by changing the CDR of the first cons cell:

    (setq x1 (list 'a 'b 'c))
         ⇒ (a b c)
    (setcdr x1 (cdr (cdr x1)))
         ⇒ (c)
    x1
         ⇒ (a c)

Here is the result in box notation:

                       --------------------
                      |                    |
     --------------   |   --------------   |    --------------
    | car   | cdr  |  |  | car   | cdr  |   -->| car   | cdr  |
    |   a   |   o-----   |   b   |   o-------->|   c   |  nil |
    |       |      |     |       |      |      |       |      |
     --------------       --------------        --------------

The second cons cell, which previously held the element `b`, still exists and its CAR is still `b`, but it no longer forms part of this list.

It is equally easy to insert a new element by changing CDRs:

    (setq x1 (list 'a 'b 'c))
         ⇒ (a b c)
    (setcdr x1 (cons 'd (cdr x1)))
         ⇒ (d b c)
    x1
         ⇒ (a d b c)

Here is this result in box notation:

     --------------        -------------       -------------
    | car  | cdr   |      | car  | cdr  |     | car  | cdr  |
    |   a  |   o   |   -->|   b  |   o------->|   c  |  nil |
    |      |   |   |  |   |      |      |     |      |      |
     --------- | --   |    -------------       -------------
               |      |
         -----         --------
        |                      |
        |    ---------------   |
        |   | car   | cdr   |  |
         -->|   d   |   o------
            |       |       |
             ---------------

Next: [Rearrangement](Rearrangement.html), Previous: [Setcar](Setcar.html), Up: [Modifying Lists](Modifying-Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
