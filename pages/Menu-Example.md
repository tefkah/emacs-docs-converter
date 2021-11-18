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

Next: [Menu Bar](Menu-Bar.html), Previous: [Keyboard Menus](Keyboard-Menus.html), Up: [Menu Keymaps](Menu-Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 22.17.4 Menu Example

Here is a complete example of defining a menu keymap. It is the definition of the ‘`Replace`’ submenu in the ‘`Edit`’ menu in the menu bar, and it uses the extended menu item format (see [Extended Menu Items](Extended-Menu-Items.html)). First we create the keymap, and give it a name:

    (defvar menu-bar-replace-menu (make-sparse-keymap "Replace"))

Next we define the menu items:

    (define-key menu-bar-replace-menu [tags-repl-continue]
      '(menu-item "Continue Replace" multifile-continue
                  :help "Continue last tags replace operation"))
    (define-key menu-bar-replace-menu [tags-repl]
      '(menu-item "Replace in tagged files" tags-query-replace
                  :help "Interactively replace a regexp in all tagged files"))
    (define-key menu-bar-replace-menu [separator-replace-tags]
      '(menu-item "--"))
    ;; …

Note the symbols which the bindings are made for; these appear inside square brackets, in the key sequence being defined. In some cases, this symbol is the same as the command name; sometimes it is different. These symbols are treated as function keys, but they are not real function keys on the keyboard. They do not affect the functioning of the menu itself, but they are echoed in the echo area when the user selects from the menu, and they appear in the output of `where-is` and `apropos`.

The menu in this example is intended for use with the mouse. If a menu is intended for use with the keyboard, that is, if it is bound to a key sequence ending with a keyboard event, then the menu items should be bound to characters or real function keys, that can be typed with the keyboard.

The binding whose definition is `("--")` is a separator line. Like a real menu item, the separator has a key symbol, in this case `separator-replace-tags`. If one menu has two separators, they must have two different key symbols.

Here is how we make this menu appear as an item in the parent menu:

    (define-key menu-bar-edit-menu [replace]
      (list 'menu-item "Replace" menu-bar-replace-menu))

Note that this incorporates the submenu keymap, which is the value of the variable `menu-bar-replace-menu`, rather than the symbol `menu-bar-replace-menu` itself. Using that symbol in the parent menu item would be meaningless because `menu-bar-replace-menu` is not a command.

If you wanted to attach the same replace menu to a mouse click, you can do it this way:

    (define-key global-map [C-S-down-mouse-1]
       menu-bar-replace-menu)

Next: [Menu Bar](Menu-Bar.html), Previous: [Keyboard Menus](Keyboard-Menus.html), Up: [Menu Keymaps](Menu-Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
