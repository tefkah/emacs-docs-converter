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

Next: [Display Feature Testing](Display-Feature-Testing.html), Previous: [Text Terminal Colors](Text-Terminal-Colors.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 29.24 X Resources

This section describes some of the functions and variables for querying and using X resources, or their equivalent on your operating system. See [X Resources](https://www.gnu.org/software/emacs/manual/html_node/emacs/X-Resources.html#X-Resources) in The GNU Emacs Manual, for more information about X resources.

*   Function: **x-get-resource** *attribute class \&optional component subclass*

    The function `x-get-resource` retrieves a resource value from the X Window defaults database.

    Resources are indexed by a combination of a *key* and a *class*. This function searches using a key of the form ‘`instance.attribute`’ (where `instance` is the name under which Emacs was invoked), and using ‘`Emacs.class`’ as the class.

    The optional arguments `component` and `subclass` add to the key and the class, respectively. You must specify both of them or neither. If you specify them, the key is ‘`instance.component.attribute`’, and the class is ‘`Emacs.class.subclass`’.

<!---->

*   Variable: **x-resource-class**

    This variable specifies the application name that `x-get-resource` should look up. The default value is `"Emacs"`. You can examine X resources for other application names by binding this variable to some other string, around a call to `x-get-resource`.

<!---->

*   Variable: **x-resource-name**

    This variable specifies the instance name that `x-get-resource` should look up. The default value is the name Emacs was invoked with, or the value specified with the ‘`-name`’ or ‘`-rn`’ switches.

To illustrate some of the above, suppose that you have the line:

    xterm.vt100.background: yellow

in your X resources file (whose name is usually `~/.Xdefaults` or `~/.Xresources`). Then:

    (let ((x-resource-class "XTerm") (x-resource-name "xterm"))
      (x-get-resource "vt100.background" "VT100.Background"))
         ⇒ "yellow"

<!---->

    (let ((x-resource-class "XTerm") (x-resource-name "xterm"))
      (x-get-resource "background" "VT100" "vt100" "Background"))
         ⇒ "yellow"

*   Variable: **inhibit-x-resources**

    If this variable is non-`nil`, Emacs does not look up X resources, and X resources do not have any effect when creating new frames.

Next: [Display Feature Testing](Display-Feature-Testing.html), Previous: [Text Terminal Colors](Text-Terminal-Colors.html), Up: [Frames](Frames.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
