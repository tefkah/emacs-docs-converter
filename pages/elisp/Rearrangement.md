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

Previous: [Setcdr](Setcdr.html), Up: [Modifying Lists](Modifying-Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 5.6.3 Functions that Rearrange Lists

Here are some functions that rearrange lists destructively by modifying the CDRs of their component cons cells. These functions are destructive because they chew up the original lists passed to them as arguments, relinking their cons cells to form a new list that is the returned value.

See `delq`, in [Sets And Lists](Sets-And-Lists.html), for another function that modifies cons cells.

*   Function: **nconc** *\&rest lists*

    This function returns a list containing all the elements of `lists`. Unlike `append` (see [Building Lists](Building-Lists.html)), the `lists` are *not* copied. Instead, the last CDR of each of the `lists` is changed to refer to the following list. The last of the `lists` is not altered. For example:

        (setq x (list 1 2 3))
             ⇒ (1 2 3)

    <!---->

        (nconc x '(4 5))
             ⇒ (1 2 3 4 5)

    <!---->

        x
             ⇒ (1 2 3 4 5)

    Since the last argument of `nconc` is not itself modified, it is reasonable to use a constant list, such as `'(4 5)`, as in the above example. For the same reason, the last argument need not be a list:

        (setq x (list 1 2 3))
             ⇒ (1 2 3)

    <!---->

        (nconc x 'z)
             ⇒ (1 2 3 . z)

    <!---->

        x
             ⇒ (1 2 3 . z)

    However, the other arguments (all but the last) should be mutable lists.

    A common pitfall is to use a constant list as a non-last argument to `nconc`. If you do this, the resulting behavior is undefined. It is possible that your program will change each time you run it! Here is what might happen (though this is not guaranteed to happen):

        (defun add-foo (x)            ; We want this function to add
          (nconc '(foo) x))           ;   foo to the front of its arg.

    ```
    ```

        (symbol-function 'add-foo)
             ⇒ (lambda (x) (nconc '(foo) x))

    ```
    ```

        (setq xx (add-foo '(1 2)))    ; It seems to work.
             ⇒ (foo 1 2)

    <!---->

        (setq xy (add-foo '(3 4)))    ; What happened?
             ⇒ (foo 1 2 3 4)

    <!---->

        (eq xx xy)
             ⇒ t

    ```
    ```

        (symbol-function 'add-foo)
             ⇒ (lambda (x) (nconc '(foo 1 2 3 4) x))

Previous: [Setcdr](Setcdr.html), Up: [Modifying Lists](Modifying-Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
