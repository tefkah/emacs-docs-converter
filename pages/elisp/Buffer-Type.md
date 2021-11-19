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

Next: [Marker Type](Marker-Type.html), Up: [Editing Types](Editing-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.5.1 Buffer Type

A *buffer* is an object that holds text that can be edited (see [Buffers](Buffers.html)). Most buffers hold the contents of a disk file (see [Files](Files.html)) so they can be edited, but some are used for other purposes. Most buffers are also meant to be seen by the user, and therefore displayed, at some time, in a window (see [Windows](Windows.html)). But a buffer need not be displayed in any window. Each buffer has a designated position called *point* (see [Positions](Positions.html)); most editing commands act on the contents of the current buffer in the neighborhood of point. At any time, one buffer is the *current buffer*.

The contents of a buffer are much like a string, but buffers are not used like strings in Emacs Lisp, and the available operations are different. For example, you can insert text efficiently into an existing buffer, altering the buffer’s contents, whereas inserting text into a string requires concatenating substrings, and the result is an entirely new string object.

Many of the standard Emacs functions manipulate or test the characters in the current buffer; a whole chapter in this manual is devoted to describing these functions (see [Text](Text.html)).

Several other data structures are associated with each buffer:

*   a local syntax table (see [Syntax Tables](Syntax-Tables.html));
*   a local keymap (see [Keymaps](Keymaps.html)); and,
*   a list of buffer-local variable bindings (see [Buffer-Local Variables](Buffer_002dLocal-Variables.html)).
*   overlays (see [Overlays](Overlays.html)).
*   text properties for the text in the buffer (see [Text Properties](Text-Properties.html)).

The local keymap and variable list contain entries that individually override global bindings or values. These are used to customize the behavior of programs in different buffers, without actually changing the programs.

A buffer may be *indirect*, which means it shares the text of another buffer, but presents it differently. See [Indirect Buffers](Indirect-Buffers.html).

Buffers have no read syntax. They print in hash notation, showing the buffer name.

    (current-buffer)
         ⇒ #<buffer objects.texi>

Next: [Marker Type](Marker-Type.html), Up: [Editing Types](Editing-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]