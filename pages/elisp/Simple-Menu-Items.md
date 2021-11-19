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

Next: [Extended Menu Items](Extended-Menu-Items.html), Up: [Defining Menus](Defining-Menus.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 22.17.1.1 Simple Menu Items

The simpler (and original) way to define a menu item is to bind some event type (it doesn’t matter what event type) to a binding like this:

    (item-string . real-binding)

The CAR, `item-string`, is the string to be displayed in the menu. It should be short—preferably one to three words. It should describe the action of the command it corresponds to. Note that not all graphical toolkits can display non-ASCII text in menus (it will work for keyboard menus and will work to a large extent with the GTK+ toolkit).

You can also supply a second string, called the help string, as follows:

    (item-string help . real-binding)

`help` specifies a help-echo string to display while the mouse is on that item in the same way as `help-echo` text properties (see [Help display](Special-Properties.html#Help-display)).

As far as `define-key` is concerned, `item-string` and `help-string` are part of the event’s binding. However, `lookup-key` returns just `real-binding`, and only `real-binding` is used for executing the key.

If `real-binding` is `nil`, then `item-string` appears in the menu but cannot be selected.

If `real-binding` is a symbol and has a non-`nil` `menu-enable` property, that property is an expression that controls whether the menu item is enabled. Every time the keymap is used to display a menu, Emacs evaluates the expression, and it enables the menu item only if the expression’s value is non-`nil`. When a menu item is disabled, it is displayed in a fuzzy fashion, and cannot be selected.

The menu bar does not recalculate which items are enabled every time you look at a menu. This is because the X toolkit requires the whole tree of menus in advance. To force recalculation of the menu bar, call `force-mode-line-update` (see [Mode Line Format](Mode-Line-Format.html)).

Next: [Extended Menu Items](Extended-Menu-Items.html), Up: [Defining Menus](Defining-Menus.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
