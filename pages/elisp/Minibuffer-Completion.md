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

Next: [Completion Commands](Completion-Commands.html), Previous: [Basic Completion](Basic-Completion.html), Up: [Completion](Completion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 20.6.2 Completion and the Minibuffer

This section describes the basic interface for reading from the minibuffer with completion.

*   Function: **completing-read** *prompt collection \&optional predicate require-match initial history default inherit-input-method*

    This function reads a string in the minibuffer, assisting the user by providing completion. It activates the minibuffer with prompt `prompt`, which must be a string.

    The actual completion is done by passing the completion table `collection` and the completion predicate `predicate` to the function `try-completion` (see [Basic Completion](Basic-Completion.html)). This happens in certain commands bound in the local keymaps used for completion. Some of these commands also call `test-completion`. Thus, if `predicate` is non-`nil`, it should be compatible with `collection` and `completion-ignore-case`. See [Definition of test-completion](Basic-Completion.html#Definition-of-test_002dcompletion).

    See [Programmed Completion](Programmed-Completion.html), for detailed requirements when `collection` is a function.

    The value of the optional argument `require-match` determines how the user may exit the minibuffer:

    *   If `nil`, the usual minibuffer exit commands work regardless of the input in the minibuffer.
    *   If `t`, the usual minibuffer exit commands won’t exit unless the input completes to an element of `collection`.
    *   If `confirm`, the user can exit with any input, but is asked for confirmation if the input is not an element of `collection`.
    *   If `confirm-after-completion`, the user can exit with any input, but is asked for confirmation if the preceding command was a completion command (i.e., one of the commands in `minibuffer-confirm-exit-commands`) and the resulting input is not an element of `collection`. See [Completion Commands](Completion-Commands.html).
    *   Any other value of `require-match` behaves like `t`, except that the exit commands won’t exit if it performs completion.

    However, empty input is always permitted, regardless of the value of `require-match`; in that case, `completing-read` returns the first element of `default`, if it is a list; `""`, if `default` is `nil`; or `default`. The string or strings in `default` are also available to the user through the history commands.

    The function `completing-read` uses `minibuffer-local-completion-map` as the keymap if `require-match` is `nil`, and uses `minibuffer-local-must-match-map` if `require-match` is non-`nil`. See [Completion Commands](Completion-Commands.html).

    The argument `history` specifies which history list variable to use for saving the input and for minibuffer history commands. It defaults to `minibuffer-history`. See [Minibuffer History](Minibuffer-History.html).

    The argument `initial` is mostly deprecated; we recommend using a non-`nil` value only in conjunction with specifying a cons cell for `history`. See [Initial Input](Initial-Input.html). For default input, use `default` instead.

    If the argument `inherit-input-method` is non-`nil`, then the minibuffer inherits the current input method (see [Input Methods](Input-Methods.html)) and the setting of `enable-multibyte-characters` (see [Text Representations](Text-Representations.html)) from whichever buffer was current before entering the minibuffer.

    If the variable `completion-ignore-case` is non-`nil`, completion ignores case when comparing the input against the possible matches. See [Basic Completion](Basic-Completion.html). In this mode of operation, `predicate` must also ignore case, or you will get surprising results.

    Here’s an example of using `completing-read`:

        (completing-read
         "Complete a foo: "
         '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
         nil t "fo")

    ```
    ```

        ;; After evaluation of the preceding expression,
        ;;   the following appears in the minibuffer:

        ---------- Buffer: Minibuffer ----------
        Complete a foo: fo∗
        ---------- Buffer: Minibuffer ----------

    If the user then types `DEL DEL b RET`, `completing-read` returns `barfoo`.

    The `completing-read` function binds variables to pass information to the commands that actually do completion. They are described in the following section.

<!---->

*   Variable: **completing-read-function**

    The value of this variable must be a function, which is called by `completing-read` to actually do its work. It should accept the same arguments as `completing-read`. This can be bound to a different function to completely override the normal behavior of `completing-read`.

Next: [Completion Commands](Completion-Commands.html), Previous: [Basic Completion](Basic-Completion.html), Up: [Completion](Completion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
