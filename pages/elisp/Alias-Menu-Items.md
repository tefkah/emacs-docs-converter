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

Previous: [Menu Separators](Menu-Separators.html), Up: [Defining Menus](Defining-Menus.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 22.17.1.4 Alias Menu Items

Sometimes it is useful to make menu items that use the same command but with different enable conditions. The best way to do this in Emacs now is with extended menu items; before that feature existed, it could be done by defining alias commands and using them in menu items. Here’s an example that makes two aliases for `read-only-mode` and gives them different enable conditions:

    (defalias 'make-read-only 'read-only-mode)
    (put 'make-read-only 'menu-enable '(not buffer-read-only))
    (defalias 'make-writable 'read-only-mode)
    (put 'make-writable 'menu-enable 'buffer-read-only)

When using aliases in menus, often it is useful to display the equivalent key bindings for the real command name, not the aliases (which typically don’t have any key bindings except for the menu itself). To request this, give the alias symbol a non-`nil` `menu-alias` property. Thus,

    (put 'make-read-only 'menu-alias t)
    (put 'make-writable 'menu-alias t)

causes menu items for `make-read-only` and `make-writable` to show the keyboard bindings for `read-only-mode`.
