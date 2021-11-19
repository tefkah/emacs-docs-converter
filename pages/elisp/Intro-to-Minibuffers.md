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

Next: [Text from Minibuffer](Text-from-Minibuffer.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.1 Introduction to Minibuffers

In most ways, a minibuffer is a normal Emacs buffer. Most operations *within* a buffer, such as editing commands, work normally in a minibuffer. However, many operations for managing buffers do not apply to minibuffers. The name of a minibuffer always has the form ‘` *Minibuf-number*`’<!-- /@w -->, and it cannot be changed. Minibuffers are displayed only in special windows used only for minibuffers; these windows always appear at the bottom of a frame. (Sometimes frames have no minibuffer window, and sometimes a special kind of frame contains nothing but a minibuffer window; see [Minibuffers and Frames](Minibuffers-and-Frames.html).)

The text in the minibuffer always starts with the *prompt string*, the text that was specified by the program that is using the minibuffer to tell the user what sort of input to type. This text is marked read-only so you won’t accidentally delete or change it. It is also marked as a field (see [Fields](Fields.html)), so that certain motion functions, including `beginning-of-line`, `forward-word`, `forward-sentence`, and `forward-paragraph`, stop at the boundary between the prompt and the actual text.

The minibuffer’s window is normally a single line; it grows automatically if the contents require more space. Whilst the minibuffer is active, you can explicitly resize its window temporarily with the window sizing commands; the window reverts to its normal size when the minibuffer is exited. When the minibuffer is not active, you can resize its window permanently by using the window sizing commands in the frame’s other window, or dragging the mode line with the mouse. (Due to details of the current implementation, for this to work `resize-mini-windows` must be `nil`.) If the frame contains just a minibuffer window, you can change its size by changing the frame’s size.

Use of the minibuffer reads input events, and that alters the values of variables such as `this-command` and `last-command` (see [Command Loop Info](Command-Loop-Info.html)). Your program should bind them around the code that uses the minibuffer, if you do not want that to change them.

Under some circumstances, a command can use a minibuffer even if there is an active minibuffer; such a minibuffer is called a *recursive minibuffer*. The first minibuffer is named ‘` *Minibuf-1*`’<!-- /@w -->. Recursive minibuffers are named by incrementing the number at the end of the name. (The names begin with a space so that they won’t show up in normal buffer lists.) Of several recursive minibuffers, the innermost (or most recently entered) is the active minibuffer. We usually call this *the* minibuffer. You can permit or forbid recursive minibuffers by setting the variable `enable-recursive-minibuffers`, or by putting properties of that name on command symbols (See [Recursive Mini](Recursive-Mini.html).)

Like other buffers, a minibuffer uses a local keymap (see [Keymaps](Keymaps.html)) to specify special key bindings. The function that invokes the minibuffer also sets up its local map according to the job to be done. See [Text from Minibuffer](Text-from-Minibuffer.html), for the non-completion minibuffer local maps. See [Completion Commands](Completion-Commands.html), for the minibuffer local maps for completion.

When a minibuffer is inactive, its major mode is `minibuffer-inactive-mode`, with keymap `minibuffer-inactive-mode-map`. This is only really useful if the minibuffer is in a separate frame. See [Minibuffers and Frames](Minibuffers-and-Frames.html).

When Emacs is running in batch mode, any request to read from the minibuffer actually reads a line from the standard input descriptor that was supplied when Emacs was started. This supports only basic input: none of the special minibuffer features (history, completion, etc.) are available in batch mode.

Next: [Text from Minibuffer](Text-from-Minibuffer.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
