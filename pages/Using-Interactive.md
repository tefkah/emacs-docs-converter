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

Next: [Interactive Codes](Interactive-Codes.html), Up: [Defining Commands](Defining-Commands.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 21.2.1 Using `interactive`

This section describes how to write the `interactive` form that makes a Lisp function an interactively-callable command, and how to examine a command’s `interactive` form.

*   Special Form: **interactive** *arg-descriptor*

    This special form declares that a function is a command, and that it may therefore be called interactively (via `M-x` or by entering a key sequence bound to it). The argument `arg-descriptor` declares how to compute the arguments to the command when the command is called interactively.

    A command may be called from Lisp programs like any other function, but then the caller supplies the arguments and `arg-descriptor` has no effect.

    The `interactive` form must be located at top-level in the function body, or in the function symbol’s `interactive-form` property (see [Symbol Properties](Symbol-Properties.html)). It has its effect because the command loop looks for it before calling the function (see [Interactive Call](Interactive-Call.html)). Once the function is called, all its body forms are executed; at this time, if the `interactive` form occurs within the body, the form simply returns `nil` without even evaluating its argument.

    By convention, you should put the `interactive` form in the function body, as the first top-level form. If there is an `interactive` form in both the `interactive-form` symbol property and the function body, the former takes precedence. The `interactive-form` symbol property can be used to add an interactive form to an existing function, or change how its arguments are processed interactively, without redefining the function.

There are three possibilities for the argument `arg-descriptor`:

*   It may be omitted or `nil`; then the command is called with no arguments. This leads quickly to an error if the command requires one or more arguments.

*   It may be a string; its contents are a sequence of elements separated by newlines, one for each argument[12](#FOOT12). Each element consists of a code character (see [Interactive Codes](Interactive-Codes.html)) optionally followed by a prompt (which some code characters use and some ignore). Here is an example:

        (interactive "P\nbFrobnicate buffer: ")

    The code letter ‘`P`’ sets the command’s first argument to the raw command prefix (see [Prefix Command Arguments](Prefix-Command-Arguments.html)). ‘`bFrobnicate buffer: `’ prompts the user with ‘`Frobnicate buffer: `’ to enter the name of an existing buffer, which becomes the second and final argument.

    The prompt string can use ‘`%`’ to include previous argument values (starting with the first argument) in the prompt. This is done using `format-message` (see [Formatting Strings](Formatting-Strings.html)). For example, here is how you could read the name of an existing buffer followed by a new name to give to that buffer:

        (interactive "bBuffer to rename: \nsRename buffer %s to: ")

    If ‘`*`’ appears at the beginning of the string, then an error is signaled if the buffer is read-only.

    If ‘`@`’ appears at the beginning of the string, and if the key sequence used to invoke the command includes any mouse events, then the window associated with the first of those events is selected before the command is run.

    If ‘`^`’ appears at the beginning of the string, and if the command was invoked through *shift-translation*, set the mark and activate the region temporarily, or extend an already active region, before the command is run. If the command was invoked without shift-translation, and the region is temporarily active, deactivate the region before the command is run. Shift-translation is controlled on the user level by `shift-select-mode`; see [Shift Selection](https://www.gnu.org/software/emacs/manual/html_node/emacs/Shift-Selection.html#Shift-Selection) in The GNU Emacs Manual.

    You can use ‘`*`’, ‘`@`’, and `^` together; the order does not matter. Actual reading of arguments is controlled by the rest of the prompt string (starting with the first character that is not ‘`*`’, ‘`@`’, or ‘`^`’).

*   It may be a Lisp expression that is not a string; then it should be a form that is evaluated to get a list of arguments to pass to the command. Usually this form will call various functions to read input from the user, most often through the minibuffer (see [Minibuffers](Minibuffers.html)) or directly from the keyboard (see [Reading Input](Reading-Input.html)).

    Providing point or the mark as an argument value is also common, but if you do this *and* read input (whether using the minibuffer or not), be sure to get the integer values of point or the mark after reading. The current buffer may be receiving subprocess output; if subprocess output arrives while the command is waiting for input, it could relocate point and the mark.

    Here’s an example of what *not* to do:

        (interactive
         (list (region-beginning) (region-end)
               (read-string "Foo: " nil 'my-history)))

    Here’s how to avoid the problem, by examining point and the mark after reading the keyboard input:

        (interactive
         (let ((string (read-string "Foo: " nil 'my-history)))
           (list (region-beginning) (region-end) string)))

    **Warning:** the argument values should not include any data types that can’t be printed and then read. Some facilities save `command-history` in a file to be read in the subsequent sessions; if a command’s arguments contain a data type that prints using ‘`#<…>`’ syntax, those facilities won’t work.

    There are, however, a few exceptions: it is ok to use a limited set of expressions such as `(point)`, `(mark)`, `(region-beginning)`, and `(region-end)`, because Emacs recognizes them specially and puts the expression (rather than its value) into the command history. To see whether the expression you wrote is one of these exceptions, run the command, then examine `(car command-history)`.

<!---->

*   Function: **interactive-form** *function*

    This function returns the `interactive` form of `function`. If `function` is an interactively callable function (see [Interactive Call](Interactive-Call.html)), the value is the command’s `interactive` form `(interactive spec)`, which specifies how to compute its arguments. Otherwise, the value is `nil`. If `function` is a symbol, its function definition is used.

***

#### Footnotes

##### [(12)](#DOCF12)

Some elements actually supply two arguments.

Next: [Interactive Codes](Interactive-Codes.html), Up: [Defining Commands](Defining-Commands.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
