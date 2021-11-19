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

Next: [Menu Separators](Menu-Separators.html), Previous: [Simple Menu Items](Simple-Menu-Items.html), Up: [Defining Menus](Defining-Menus.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 22.17.1.2 Extended Menu Items

An extended-format menu item is a more flexible and also cleaner alternative to the simple format. You define an event type with a binding that’s a list starting with the symbol `menu-item`. For a non-selectable string, the binding looks like this:

    (menu-item item-name)

A string starting with two or more dashes specifies a separator line; see [Menu Separators](Menu-Separators.html).

To define a real menu item which can be selected, the extended format binding looks like this:

    (menu-item item-name real-binding
        . item-property-list)

Here, `item-name` is an expression which evaluates to the menu item string. Thus, the string need not be a constant.

The third element, `real-binding`, can be the command to execute (in which case you get a normal menu item). It can also be a keymap, which will result in a submenu. Finally, it can be `nil`, in which case you will get a non-selectable menu item. This is mostly useful when creating separator lines and the like.

The tail of the list, `item-property-list`, has the form of a property list which contains other information.

Here is a table of the properties that are supported:

*   `:enable form`

    The result of evaluating `form` determines whether the item is enabled (non-`nil` means yes). If the item is not enabled, you can’t really click on it.

*   `:visible form`

    The result of evaluating `form` determines whether the item should actually appear in the menu (non-`nil` means yes). If the item does not appear, then the menu is displayed as if this item were not defined at all.

*   `:help help`

    The value of this property, `help`, specifies a help-echo string to display while the mouse is on that item. This is displayed in the same way as `help-echo` text properties (see [Help display](Special-Properties.html#Help-display)). Note that this must be a constant string, unlike the `help-echo` property for text and overlays.

*   `:button (type . selected)`

    This property provides a way to define radio buttons and toggle buttons. The CAR, `type`, says which: it should be `:toggle` or `:radio`. The CDR, `selected`, should be a form; the result of evaluating it says whether this button is currently selected.

    A *toggle* is a menu item which is labeled as either on or off according to the value of `selected`. The command itself should toggle `selected`, setting it to `t` if it is `nil`, and to `nil` if it is `t`. Here is how the menu item to toggle the `debug-on-error` flag is defined:

        (menu-item "Debug on Error" toggle-debug-on-error
                   :button (:toggle
                            . (and (boundp 'debug-on-error)
                                   debug-on-error)))

    This works because `toggle-debug-on-error` is defined as a command which toggles the variable `debug-on-error`.

    *Radio buttons* are a group of menu items, in which at any time one and only one is selected. There should be a variable whose value says which one is selected at any time. The `selected` form for each radio button in the group should check whether the variable has the right value for selecting that button. Clicking on the button should set the variable so that the button you clicked on becomes selected.

*   `:key-sequence key-sequence`

    This property specifies which key sequence to display as keyboard equivalent. Before Emacs displays `key-sequence` in the menu, it verifies that `key-sequence` is really equivalent to this menu item, so it only has an effect if you specify a correct key sequence. Specifying `nil` for `key-sequence` is equivalent to the `:key-sequence` attribute being absent.

*   `:keys string`

    This property specifies that `string` is the string to display as the keyboard equivalent for this menu item. You can use the ‘`\\[...]`’ documentation construct in `string`.

*   `:filter filter-fn`

    This property provides a way to compute the menu item dynamically. The property value `filter-fn` should be a function of one argument; when it is called, its argument will be `real-binding`. The function should return the binding to use instead.

    Emacs can call this function at any time that it does redisplay or operates on menu data structures, so you should write it so it can safely be called at any time.

Next: [Menu Separators](Menu-Separators.html), Previous: [Simple Menu Items](Simple-Menu-Items.html), Up: [Defining Menus](Defining-Menus.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
