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

Next: [Minibuffer History](Minibuffer-History.html), Previous: [Text from Minibuffer](Text-from-Minibuffer.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 20.3 Reading Lisp Objects with the Minibuffer

This section describes functions for reading Lisp objects with the minibuffer.

*   Function: **read-minibuffer** *prompt \&optional initial*

    This function reads a Lisp object using the minibuffer, and returns it without evaluating it. The arguments `prompt` and `initial` are used as in `read-from-minibuffer`.

    This is a simplified interface to the `read-from-minibuffer` function:

        (read-minibuffer prompt initial)
        ≡
        (let (minibuffer-allow-text-properties)
          (read-from-minibuffer prompt initial nil t))

    Here is an example in which we supply the string `"(testing)"` as initial input:

        (read-minibuffer
         "Enter an expression: " (format "%s" '(testing)))

        ;; Here is how the minibuffer is displayed:

    ```
    ```

        ---------- Buffer: Minibuffer ----------
        Enter an expression: (testing)∗
        ---------- Buffer: Minibuffer ----------

    The user can type `RET` immediately to use the initial input as a default, or can edit the input.

<!---->

*   Function: **eval-minibuffer** *prompt \&optional initial*

    This function reads a Lisp expression using the minibuffer, evaluates it, then returns the result. The arguments `prompt` and `initial` are used as in `read-from-minibuffer`.

    This function simply evaluates the result of a call to `read-minibuffer`:

        (eval-minibuffer prompt initial)
        ≡
        (eval (read-minibuffer prompt initial))

<!---->

*   Function: **edit-and-eval-command** *prompt form*

    This function reads a Lisp expression in the minibuffer, evaluates it, then returns the result. The difference between this command and `eval-minibuffer` is that here the initial `form` is not optional and it is treated as a Lisp object to be converted to printed representation rather than as a string of text. It is printed with `prin1`, so if it is a string, double-quote characters (‘`"`’) appear in the initial text. See [Output Functions](Output-Functions.html).

    In the following example, we offer the user an expression with initial text that is already a valid form:

        (edit-and-eval-command "Please edit: " '(forward-word 1))

        ;; After evaluation of the preceding expression,
        ;;   the following appears in the minibuffer:

    ```
    ```

        ---------- Buffer: Minibuffer ----------
        Please edit: (forward-word 1)∗
        ---------- Buffer: Minibuffer ----------

    Typing `RET` right away would exit the minibuffer and evaluate the expression, thus moving point forward one word.

Next: [Minibuffer History](Minibuffer-History.html), Previous: [Text from Minibuffer](Text-from-Minibuffer.html), Up: [Minibuffers](Minibuffers.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
