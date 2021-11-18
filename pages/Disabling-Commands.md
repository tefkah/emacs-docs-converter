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

Next: [Command History](Command-History.html), Previous: [Recursive Editing](Recursive-Editing.html), Up: [Command Loop](Command-Loop.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 21.14 Disabling Commands

*Disabling a command* marks the command as requiring user confirmation before it can be executed. Disabling is used for commands which might be confusing to beginning users, to prevent them from using the commands by accident.

The low-level mechanism for disabling a command is to put a non-`nil` `disabled` property on the Lisp symbol for the command. These properties are normally set up by the user’s init file (see [Init File](Init-File.html)) with Lisp expressions such as this:

    (put 'upcase-region 'disabled t)

For a few commands, these properties are present by default (you can remove them in your init file if you wish).

If the value of the `disabled` property is a string, the message saying the command is disabled includes that string. For example:

    (put 'delete-region 'disabled
         "Text deleted this way cannot be yanked back!\n")

See [Disabling](https://www.gnu.org/software/emacs/manual/html_node/emacs/Disabling.html#Disabling) in The GNU Emacs Manual, for the details on what happens when a disabled command is invoked interactively. Disabling a command has no effect on calling it as a function from Lisp programs.

*   Command: **enable-command** *command*

    Allow `command` (a symbol) to be executed without special confirmation from now on, and alter the user’s init file (see [Init File](Init-File.html)) so that this will apply to future sessions.

<!---->

*   Command: **disable-command** *command*

    Require special confirmation to execute `command` from now on, and alter the user’s init file so that this will apply to future sessions.

<!---->

*   Variable: **disabled-command-function**

    The value of this variable should be a function. When the user invokes a disabled command interactively, this function is called instead of the disabled command. It can use `this-command-keys` to determine what the user typed to run the command, and thus find the command itself.

    The value may also be `nil`. Then all commands work normally, even disabled ones.

    By default, the value is a function that asks the user whether to proceed.

Next: [Command History](Command-History.html), Previous: [Recursive Editing](Recursive-Editing.html), Up: [Command Loop](Command-Loop.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
