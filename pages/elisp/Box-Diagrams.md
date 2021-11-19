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

Next: [Dotted Pair Notation](Dotted-Pair-Notation.html), Up: [Cons Cell Type](Cons-Cell-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.4.6.1 Drawing Lists as Box Diagrams

A list can be illustrated by a diagram in which the cons cells are shown as pairs of boxes, like dominoes. (The Lisp reader cannot read such an illustration; unlike the textual notation, which can be understood by both humans and computers, the box illustrations can be understood only by humans.) This picture represents the three-element list `(rose violet buttercup)`:

        --- ---      --- ---      --- ---
       |   |   |--> |   |   |--> |   |   |--> nil
        --- ---      --- ---      --- ---
         |            |            |
         |            |            |
          --> rose     --> violet   --> buttercup

In this diagram, each box represents a slot that can hold or refer to any Lisp object. Each pair of boxes represents a cons cell. Each arrow represents a reference to a Lisp object, either an atom or another cons cell.

In this example, the first box, which holds the CAR of the first cons cell, refers to or holds `rose` (a symbol). The second box, holding the CDR of the first cons cell, refers to the next pair of boxes, the second cons cell. The CAR of the second cons cell is `violet`, and its CDR is the third cons cell. The CDR of the third (and last) cons cell is `nil`.

Here is another diagram of the same list, `(rose violet buttercup)`, sketched in a different manner:

     ---------------       ----------------       -------------------
    | car   | cdr   |     | car    | cdr   |     | car       | cdr   |
    | rose  |   o-------->| violet |   o-------->| buttercup |  nil  |
    |       |       |     |        |       |     |           |       |
     ---------------       ----------------       -------------------

A list with no elements in it is the *empty list*; it is identical to the symbol `nil`. In other words, `nil` is both a symbol and a list.

Here is the list `(A ())`, or equivalently `(A nil)`, depicted with boxes and arrows:

        --- ---      --- ---
       |   |   |--> |   |   |--> nil
        --- ---      --- ---
         |            |
         |            |
          --> A        --> nil

Here is a more complex illustration, showing the three-element list, `((pine needles) oak maple)`, the first element of which is a two-element list:

        --- ---      --- ---      --- ---
       |   |   |--> |   |   |--> |   |   |--> nil
        --- ---      --- ---      --- ---
         |            |            |
         |            |            |
         |             --> oak      --> maple
         |
         |     --- ---      --- ---
          --> |   |   |--> |   |   |--> nil
               --- ---      --- ---
                |            |
                |            |
                 --> pine     --> needles

The same list represented in the second box notation looks like this:

     --------------       --------------       --------------
    | car   | cdr  |     | car   | cdr  |     | car   | cdr  |
    |   o   |   o------->| oak   |   o------->| maple |  nil |
    |   |   |      |     |       |      |     |       |      |
     -- | ---------       --------------       --------------
        |
        |
        |        --------------       ----------------
        |       | car   | cdr  |     | car     | cdr  |
         ------>| pine  |   o------->| needles |  nil |
                |       |      |     |         |      |
                 --------------       ----------------

Next: [Dotted Pair Notation](Dotted-Pair-Notation.html), Up: [Cons Cell Type](Cons-Cell-Type.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
