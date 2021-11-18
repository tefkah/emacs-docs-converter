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

Next: [Keyboard Macros](Keyboard-Macros.html), Previous: [Disabling Commands](Disabling-Commands.html), Up: [Command Loop](Command-Loop.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 21.15 Command History

The command loop keeps a history of the complex commands that have been executed, to make it convenient to repeat these commands. A *complex command* is one for which the interactive argument reading uses the minibuffer. This includes any `M-x` command, any `M-:` command, and any command whose `interactive` specification reads an argument from the minibuffer. Explicit use of the minibuffer during the execution of the command itself does not cause the command to be considered complex.

*   Variable: **command-history**

    This variable’s value is a list of recent complex commands, each represented as a form to evaluate. It continues to accumulate all complex commands for the duration of the editing session, but when it reaches the maximum size (see [Minibuffer History](Minibuffer-History.html)), the oldest elements are deleted as new ones are added.

        command-history
        ⇒ ((switch-to-buffer "chistory.texi")
            (describe-key "^X^[")
            (visit-tags-table "~/emacs/src/")
            (find-tag "repeat-complex-command"))

This history list is actually a special case of minibuffer history (see [Minibuffer History](Minibuffer-History.html)), with one special twist: the elements are expressions rather than strings.

There are a number of commands devoted to the editing and recall of previous commands. The commands `repeat-complex-command`, and `list-command-history` are described in the user manual (see [Repetition](https://www.gnu.org/software/emacs/manual/html_node/emacs/Repetition.html#Repetition) in The GNU Emacs Manual). Within the minibuffer, the usual minibuffer history commands are available.
