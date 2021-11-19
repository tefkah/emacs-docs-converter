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

Next: [High-Level Completion](High_002dLevel-Completion.html), Previous: [Minibuffer Completion](Minibuffer-Completion.html), Up: [Completion](Completion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 20.6.3 Minibuffer Commands that Do Completion

This section describes the keymaps, commands and user options used in the minibuffer to do completion.

*   Variable: **minibuffer-completion-table**

    The value of this variable is the completion table (see [Basic Completion](Basic-Completion.html)) used for completion in the minibuffer. This is the global variable that contains what `completing-read` passes to `try-completion`. It is used by minibuffer completion commands such as `minibuffer-complete-word`.

<!---->

*   Variable: **minibuffer-completion-predicate**

    This variable’s value is the predicate that `completing-read` passes to `try-completion`. The variable is also used by the other minibuffer completion functions.

<!---->

*   Variable: **minibuffer-completion-confirm**

    This variable determines whether Emacs asks for confirmation before exiting the minibuffer; `completing-read` binds this variable, and the function `minibuffer-complete-and-exit` checks the value before exiting. If the value is `nil`, confirmation is not required. If the value is `confirm`, the user may exit with an input that is not a valid completion alternative, but Emacs asks for confirmation. If the value is `confirm-after-completion`, the user may exit with an input that is not a valid completion alternative, but Emacs asks for confirmation if the user submitted the input right after any of the completion commands in `minibuffer-confirm-exit-commands`.

<!---->

*   Variable: **minibuffer-confirm-exit-commands**

    This variable holds a list of commands that cause Emacs to ask for confirmation before exiting the minibuffer, if the `require-match` argument to `completing-read` is `confirm-after-completion`. The confirmation is requested if the user attempts to exit the minibuffer immediately after calling any command in this list.

<!---->

*   Command: **minibuffer-complete-word**

    This function completes the minibuffer contents by at most a single word. Even if the minibuffer contents have only one completion, `minibuffer-complete-word` does not add any characters beyond the first character that is not a word constituent. See [Syntax Tables](Syntax-Tables.html).

<!---->

*   Command: **minibuffer-complete**

    This function completes the minibuffer contents as far as possible.

<!---->

*   Command: **minibuffer-complete-and-exit**

    This function completes the minibuffer contents, and exits if confirmation is not required, i.e., if `minibuffer-completion-confirm` is `nil`. If confirmation *is* required, it is given by repeating this command immediately—the command is programmed to work without confirmation when run twice in succession.

<!---->

*   Command: **minibuffer-completion-help**

    This function creates a list of the possible completions of the current minibuffer contents. It works by calling `all-completions` using the value of the variable `minibuffer-completion-table` as the `collection` argument, and the value of `minibuffer-completion-predicate` as the `predicate` argument. The list of completions is displayed as text in a buffer named `*Completions*`.

<!---->

*   Function: **display-completion-list** *completions*

    This function displays `completions` to the stream in `standard-output`, usually a buffer. (See [Read and Print](Read-and-Print.html), for more information about streams.) The argument `completions` is normally a list of completions just returned by `all-completions`, but it does not have to be. Each element may be a symbol or a string, either of which is simply printed. It can also be a list of two strings, which is printed as if the strings were concatenated. The first of the two strings is the actual completion, the second string serves as annotation.

    This function is called by `minibuffer-completion-help`. A common way to use it is together with `with-output-to-temp-buffer`, like this:

        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list
            (all-completions (buffer-string) my-alist)))

<!---->

*   User Option: **completion-auto-help**

    If this variable is non-`nil`, the completion commands automatically display a list of possible completions whenever nothing can be completed because the next character is not uniquely determined.

<!---->

*   Variable: **minibuffer-local-completion-map**

    `completing-read` uses this value as the local keymap when an exact match of one of the completions is not required. By default, this keymap makes the following bindings:

    *   `?`

        `minibuffer-completion-help`

    *   `SPC`

        `minibuffer-complete-word`

    *   `TAB`

        `minibuffer-complete`

    and uses `minibuffer-local-map` as its parent keymap (see [Definition of minibuffer-local-map](Text-from-Minibuffer.html#Definition-of-minibuffer_002dlocal_002dmap)).

<!---->

*   Variable: **minibuffer-local-must-match-map**

    `completing-read` uses this value as the local keymap when an exact match of one of the completions is required. Therefore, no keys are bound to `exit-minibuffer`, the command that exits the minibuffer unconditionally. By default, this keymap makes the following bindings:

    *   `C-j`

        `minibuffer-complete-and-exit`

    *   `RET`

        `minibuffer-complete-and-exit`

    and uses `minibuffer-local-completion-map` as its parent keymap.

<!---->

*   Variable: **minibuffer-local-filename-completion-map**

    This is a sparse keymap that simply unbinds `SPC`; because filenames can contain spaces. The function `read-file-name` combines this keymap with either `minibuffer-local-completion-map` or `minibuffer-local-must-match-map`.

<!---->

*   Variable: **minibuffer-beginning-of-buffer-movement**

    If non-`nil`, the `M-<` command will move to the end of the prompt if point is after the end of the prompt. If point is at or before the end of the prompt, move to the start of the buffer. If this variable is `nil`, the command behaves like `beginning-of-buffer`.

Next: [High-Level Completion](High_002dLevel-Completion.html), Previous: [Minibuffer Completion](Minibuffer-Completion.html), Up: [Completion](Completion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
