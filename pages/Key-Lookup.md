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

Next: [Functions for Key Lookup](Functions-for-Key-Lookup.html), Previous: [Controlling Active Maps](Controlling-Active-Maps.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 22.10 Key Lookup

*Key lookup* is the process of finding the binding of a key sequence from a given keymap. The execution or use of the binding is not part of key lookup.

Key lookup uses just the event type of each event in the key sequence; the rest of the event is ignored. In fact, a key sequence used for key lookup may designate a mouse event with just its types (a symbol) instead of the entire event (a list). See [Input Events](Input-Events.html). Such a key sequence is insufficient for `command-execute` to run, but it is sufficient for looking up or rebinding a key.

When the key sequence consists of multiple events, key lookup processes the events sequentially: the binding of the first event is found, and must be a keymap; then the second event’s binding is found in that keymap, and so on until all the events in the key sequence are used up. (The binding thus found for the last event may or may not be a keymap.) Thus, the process of key lookup is defined in terms of a simpler process for looking up a single event in a keymap. How that is done depends on the type of object associated with the event in that keymap.

Let’s use the term *keymap entry* to describe the value found by looking up an event type in a keymap. (This doesn’t include the item string and other extra elements in a keymap element for a menu item, because `lookup-key` and other key lookup functions don’t include them in the returned value.) While any Lisp object may be stored in a keymap as a keymap entry, not all make sense for key lookup. Here is a table of the meaningful types of keymap entries:

*   `nil`

    `nil` means that the events used so far in the lookup form an undefined key. When a keymap fails to mention an event type at all, and has no default binding, that is equivalent to a binding of `nil` for that event type.

*   `command`

    The events used so far in the lookup form a complete key, and `command` is its binding. See [What Is a Function](What-Is-a-Function.html).

*   `array`

    The array (either a string or a vector) is a keyboard macro. The events used so far in the lookup form a complete key, and the array is its binding. See [Keyboard Macros](Keyboard-Macros.html), for more information.

*   `keymap`

    The events used so far in the lookup form a prefix key. The next event of the key sequence is looked up in `keymap`.

*   `list`

    The meaning of a list depends on what it contains:

    *   If the CAR of `list` is the symbol `keymap`, then the list is a keymap, and is treated as a keymap (see above).
    *   If the CAR of `list` is `lambda`, then the list is a lambda expression. This is presumed to be a function, and is treated as such (see above). In order to execute properly as a key binding, this function must be a command—it must have an `interactive` specification. See [Defining Commands](Defining-Commands.html).

*   `symbol`

    The function definition of `symbol` is used in place of `symbol`. If that too is a symbol, then this process is repeated, any number of times. Ultimately this should lead to an object that is a keymap, a command, or a keyboard macro.

    Note that keymaps and keyboard macros (strings and vectors) are not valid functions, so a symbol with a keymap, string, or vector as its function definition is invalid as a function. It is, however, valid as a key binding. If the definition is a keyboard macro, then the symbol is also valid as an argument to `command-execute` (see [Interactive Call](Interactive-Call.html)).

    The symbol `undefined` is worth special mention: it means to treat the key as undefined. Strictly speaking, the key is defined, and its binding is the command `undefined`; but that command does the same thing that is done automatically for an undefined key: it rings the bell (by calling `ding`) but does not signal an error.

    `undefined` is used in local keymaps to override a global key binding and make the key undefined locally. A local binding of `nil` would fail to do this because it would not override the global binding.

*   `anything else`

    If any other type of object is found, the events used so far in the lookup form a complete key, and the object is its binding, but the binding is not executable as a command.

In short, a keymap entry may be a keymap, a command, a keyboard macro, a symbol that leads to one of them, or `nil`.

Next: [Functions for Key Lookup](Functions-for-Key-Lookup.html), Previous: [Controlling Active Maps](Controlling-Active-Maps.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
