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

Previous: [Abbrev Properties](Abbrev-Properties.html), Up: [Abbrevs](Abbrevs.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 36.7 Abbrev Table Properties

Like abbrevs, abbrev tables have properties, some of which influence the way they work. You can provide them as arguments to `define-abbrev-table`, and manipulate them with the functions:

*   Function: **abbrev-table-put** *table prop val*

    Set the property `prop` of abbrev table `table` to value `val`.

<!---->

*   Function: **abbrev-table-get** *table prop*

    Return the property `prop` of abbrev table `table`, or `nil` if `table` has no such property.

The following properties have special meaning:

*   `:enable-function`

    This is like the `:enable-function` abbrev property except that it applies to all abbrevs in the table. It is used before even trying to find the abbrev before point, so it can dynamically modify the abbrev table.

*   `:case-fixed`

    This is like the `:case-fixed` abbrev property except that it applies to all abbrevs in the table.

*   `:regexp`

    If non-`nil`, this property is a regular expression that indicates how to extract the name of the abbrev before point, before looking it up in the table. When the regular expression matches before point, the abbrev name is expected to be in submatch 1. If this property is `nil`, the default is to use `backward-word` and `forward-word` to find the name. This property allows the use of abbrevs whose name contains characters of non-word syntax.

*   `:parents`

    This property holds a list of tables from which to inherit other abbrevs.

*   `:abbrev-table-modiff`

    This property holds a counter incremented each time a new abbrev is added to the table.