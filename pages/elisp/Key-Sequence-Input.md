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

Next: [Reading One Event](Reading-One-Event.html), Up: [Reading Input](Reading-Input.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.8.1 Key Sequence Input

The command loop reads input a key sequence at a time, by calling `read-key-sequence`. Lisp programs can also call this function; for example, `describe-key` uses it to read the key to describe.

*   Function: **read-key-sequence** *prompt \&optional continue-echo dont-downcase-last switch-frame-ok command-loop*

    This function reads a key sequence and returns it as a string or vector. It keeps reading events until it has accumulated a complete key sequence; that is, enough to specify a non-prefix command using the currently active keymaps. (Remember that a key sequence that starts with a mouse event is read using the keymaps of the buffer in the window that the mouse was in, not the current buffer.)

    If the events are all characters and all can fit in a string, then `read-key-sequence` returns a string (see [Strings of Events](Strings-of-Events.html)). Otherwise, it returns a vector, since a vector can hold all kinds of events—characters, symbols, and lists. The elements of the string or vector are the events in the key sequence.

    Reading a key sequence includes translating the events in various ways. See [Translation Keymaps](Translation-Keymaps.html).

    The argument `prompt` is either a string to be displayed in the echo area as a prompt, or `nil`, meaning not to display a prompt. The argument `continue-echo`, if non-`nil`, means to echo this key as a continuation of the previous key.

    Normally any upper case event is converted to lower case if the original event is undefined and the lower case equivalent is defined. The argument `dont-downcase-last`, if non-`nil`, means do not convert the last event to lower case. This is appropriate for reading a key sequence to be defined.

    The argument `switch-frame-ok`, if non-`nil`, means that this function should process a `switch-frame` event if the user switches frames before typing anything. If the user switches frames in the middle of a key sequence, or at the start of the sequence but `switch-frame-ok` is `nil`, then the event will be put off until after the current key sequence.

    The argument `command-loop`, if non-`nil`, means that this key sequence is being read by something that will read commands one after another. It should be `nil` if the caller will read just one key sequence.

    In the following example, Emacs displays the prompt ‘`?`’ in the echo area, and then the user types `C-x C-f`.

        (read-key-sequence "?")

    <!---->

        ---------- Echo Area ----------
        ?C-x C-f
        ---------- Echo Area ----------

             ⇒ "^X^F"

    The function `read-key-sequence` suppresses quitting: `C-g` typed while reading with this function works like any other character, and does not set `quit-flag`. See [Quitting](Quitting.html).

<!---->

*   Function: **read-key-sequence-vector** *prompt \&optional continue-echo dont-downcase-last switch-frame-ok command-loop*

    This is like `read-key-sequence` except that it always returns the key sequence as a vector, never as a string. See [Strings of Events](Strings-of-Events.html).

If an input character is upper-case (or has the shift modifier) and has no key binding, but its lower-case equivalent has one, then `read-key-sequence` converts the character to lower case. Note that `lookup-key` does not perform case conversion in this way.

When reading input results in such a *shift-translation*, Emacs sets the variable `this-command-keys-shift-translated` to a non-`nil` value. Lisp programs can examine this variable if they need to modify their behavior when invoked by shift-translated keys. For example, the function `handle-shift-selection` examines the value of this variable to determine how to activate or deactivate the region (see [handle-shift-selection](The-Mark.html)).

The function `read-key-sequence` also transforms some mouse events. It converts unbound drag events into click events, and discards unbound button-down events entirely. It also reshuffles focus events and miscellaneous window events so that they never appear in a key sequence with any other events.

When mouse events occur in special parts of a window or frame, such as a mode line or a scroll bar, the event type shows nothing special—it is the same symbol that would normally represent that combination of mouse button and modifier keys. The information about the window part is kept elsewhere in the event—in the coordinates. But `read-key-sequence` translates this information into imaginary prefix keys, all of which are symbols: `tab-line`, `header-line`, `horizontal-scroll-bar`, `menu-bar`, `tab-bar`, `mode-line`, `vertical-line`, `vertical-scroll-bar`, `left-margin`, `right-margin`, `left-fringe`, `right-fringe`, `right-divider`, and `bottom-divider`. You can define meanings for mouse clicks in special window parts by defining key sequences using these imaginary prefix keys.

For example, if you call `read-key-sequence` and then click the mouse on the window’s mode line, you get two events, like this:

    (read-key-sequence "Click on the mode line: ")
         ⇒ [mode-line
             (mouse-1
              (#<window 6 on NEWS> mode-line
               (40 . 63) 5959987))]

*   Variable: **num-input-keys**

    This variable’s value is the number of key sequences processed so far in this Emacs session. This includes key sequences read from the terminal and key sequences read from keyboard macros being executed.

Next: [Reading One Event](Reading-One-Event.html), Up: [Reading Input](Reading-Input.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
