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

Next: [Abbrev Table Properties](Abbrev-Table-Properties.html), Previous: [Standard Abbrev Tables](Standard-Abbrev-Tables.html), Up: [Abbrevs](Abbrevs.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 36.6 Abbrev Properties

Abbrevs have properties, some of which influence the way they work. You can provide them as arguments to `define-abbrev`, and manipulate them with the following functions:

*   Function: **abbrev-put** *abbrev prop val*

    Set the property `prop` of `abbrev` to value `val`.

<!---->

*   Function: **abbrev-get** *abbrev prop*

    Return the property `prop` of `abbrev`, or `nil` if the abbrev has no such property.

The following properties have special meanings:

*   `:count`

    This property counts the number of times the abbrev has been expanded. If not explicitly set, it is initialized to 0 by `define-abbrev`.

*   `:system`

    If non-`nil`, this property marks the abbrev as a system abbrev. Such abbrevs are not saved (see [Abbrev Files](Abbrev-Files.html)).

*   `:enable-function`

    If non-`nil`, this property should be a function of no arguments which returns `nil` if the abbrev should not be used and `t` otherwise.

*   `:case-fixed`

    If non-`nil`, this property indicates that the case of the abbrev’s name is significant and should only match a text with the same pattern of capitalization. It also disables the code that modifies the capitalization of the expansion.
