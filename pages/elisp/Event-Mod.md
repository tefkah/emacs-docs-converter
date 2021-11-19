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

Next: [Invoking the Input Method](Invoking-the-Input-Method.html), Previous: [Reading One Event](Reading-One-Event.html), Up: [Reading Input](Reading-Input.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.8.3 Modifying and Translating Input Events

Emacs modifies every event it reads according to `extra-keyboard-modifiers`, then translates it through `keyboard-translate-table` (if applicable), before returning it from `read-event`.

*   Variable: **extra-keyboard-modifiers**

    This variable lets Lisp programs “press” the modifier keys on the keyboard. The value is a character. Only the modifiers of the character matter. Each time the user types a keyboard key, it is altered as if those modifier keys were held down. For instance, if you bind `extra-keyboard-modifiers` to `?\C-\M-a`, then all keyboard input characters typed during the scope of the binding will have the control and meta modifiers applied to them. The character `?\C-@`, equivalent to the integer 0, does not count as a control character for this purpose, but as a character with no modifiers. Thus, setting `extra-keyboard-modifiers` to zero cancels any modification.

    When using a window system, the program can press any of the modifier keys in this way. Otherwise, only the `CTL` and `META` keys can be virtually pressed.

    Note that this variable applies only to events that really come from the keyboard, and has no effect on mouse events or any other events.

<!---->

*   Variable: **keyboard-translate-table**

    This terminal-local variable is the translate table for keyboard characters. It lets you reshuffle the keys on the keyboard without changing any command bindings. Its value is normally a char-table, or else `nil`. (It can also be a string or vector, but this is considered obsolete.)

    If `keyboard-translate-table` is a char-table (see [Char-Tables](Char_002dTables.html)), then each character read from the keyboard is looked up in this char-table. If the value found there is non-`nil`, then it is used instead of the actual input character.

    Note that this translation is the first thing that happens to a character after it is read from the terminal. Record-keeping features such as `recent-keys` and dribble files record the characters after translation.

    Note also that this translation is done before the characters are supplied to input methods (see [Input Methods](Input-Methods.html)). Use `translation-table-for-input` (see [Translation of Characters](Translation-of-Characters.html)), if you want to translate characters after input methods operate.

<!---->

*   Function: **keyboard-translate** *from to*

    This function modifies `keyboard-translate-table` to translate character code `from` into character code `to`. It creates the keyboard translate table if necessary.

Here’s an example of using the `keyboard-translate-table` to make `C-x`, `C-c` and `C-v` perform the cut, copy and paste operations:

    (keyboard-translate ?\C-x 'control-x)
    (keyboard-translate ?\C-c 'control-c)
    (keyboard-translate ?\C-v 'control-v)
    (global-set-key [control-x] 'kill-region)
    (global-set-key [control-c] 'kill-ring-save)
    (global-set-key [control-v] 'yank)

On a graphical terminal that supports extended ASCII input, you can still get the standard Emacs meanings of one of those characters by typing it with the shift key. That makes it a different character as far as keyboard translation is concerned, but it has the same usual meaning.

See [Translation Keymaps](Translation-Keymaps.html), for mechanisms that translate event sequences at the level of `read-key-sequence`.

Next: [Invoking the Input Method](Invoking-the-Input-Method.html), Previous: [Reading One Event](Reading-One-Event.html), Up: [Reading Input](Reading-Input.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
