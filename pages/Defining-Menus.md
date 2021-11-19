

Next: [Mouse Menus](Mouse-Menus.html), Up: [Menu Keymaps](Menu-Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 22.17.1 Defining Menus

A keymap acts as a menu if it has an *overall prompt string*, which is a string that appears as an element of the keymap. (See [Format of Keymaps](Format-of-Keymaps.html).) The string should describe the purpose of the menu’s commands. Emacs displays the overall prompt string as the menu title in some cases, depending on the toolkit (if any) used for displaying menus.[14](#FOOT14) Keyboard menus also display the overall prompt string.

The easiest way to construct a keymap with a prompt string is to specify the string as an argument when you call `make-keymap`, `make-sparse-keymap` (see [Creating Keymaps](Creating-Keymaps.html)), or `define-prefix-command` (see [Definition of define-prefix-command](Prefix-Keys.html#Definition-of-define_002dprefix_002dcommand)). If you do not want the keymap to operate as a menu, don’t specify a prompt string for it.

*   Function: **keymap-prompt** *keymap*

    This function returns the overall prompt string of `keymap`, or `nil` if it has none.

The menu’s items are the bindings in the keymap. Each binding associates an event type to a definition, but the event types have no significance for the menu appearance. (Usually we use pseudo-events, symbols that the keyboard cannot generate, as the event types for menu item bindings.) The menu is generated entirely from the bindings that correspond in the keymap to these events.

The order of items in the menu is the same as the order of bindings in the keymap. Since `define-key` puts new bindings at the front, you should define the menu items starting at the bottom of the menu and moving to the top, if you care about the order. When you add an item to an existing menu, you can specify its position in the menu using `define-key-after` (see [Modifying Menus](Modifying-Menus.html)).

|                                                   |    |                                           |
| :------------------------------------------------ | -- | :---------------------------------------- |
| • [Simple Menu Items](Simple-Menu-Items.html)     |    | A simple kind of menu key binding.        |
| • [Extended Menu Items](Extended-Menu-Items.html) |    | More complex menu item definitions.       |
| • [Menu Separators](Menu-Separators.html)         |    | Drawing a horizontal line through a menu. |
| • [Alias Menu Items](Alias-Menu-Items.html)       |    | Using command aliases in menu items.      |

***

#### Footnotes

##### [(14)](#DOCF14)

It is required for menus which do not use a toolkit, e.g., on a text terminal.

Next: [Mouse Menus](Mouse-Menus.html), Up: [Menu Keymaps](Menu-Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
