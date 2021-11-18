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

Next: [Suspending Emacs](Suspending-Emacs.html), Up: [Getting Out](Getting-Out.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 40.2.1 Killing Emacs

Killing Emacs means ending the execution of the Emacs process. If you started Emacs from a terminal, the parent process normally resumes control. The low-level primitive for killing Emacs is `kill-emacs`.

*   Command: **kill-emacs** *\&optional exit-data*

    This command calls the hook `kill-emacs-hook`, then exits the Emacs process and kills it.

    If `exit-data` is an integer, that is used as the exit status of the Emacs process. (This is useful primarily in batch operation; see [Batch Mode](Batch-Mode.html).)

    If `exit-data` is a string, its contents are stuffed into the terminal input buffer so that the shell (or whatever program next reads input) can read them.

    If `exit-data` is neither an integer nor a string, or is omitted, that means to use the (system-specific) exit status which indicates successful program termination.

The `kill-emacs` function is normally called via the higher-level command `C-x C-c` (`save-buffers-kill-terminal`). See [Exiting](https://www.gnu.org/software/emacs/manual/html_node/emacs/Exiting.html#Exiting) in The GNU Emacs Manual. It is also called automatically if Emacs receives a `SIGTERM` or `SIGHUP` operating system signal (e.g., when the controlling terminal is disconnected), or if it receives a `SIGINT` signal while running in batch mode (see [Batch Mode](Batch-Mode.html)).

*   Variable: **kill-emacs-hook**

    This normal hook is run by `kill-emacs`, before it kills Emacs.

    Because `kill-emacs` can be called in situations where user interaction is impossible (e.g., when the terminal is disconnected), functions on this hook should not attempt to interact with the user. If you want to interact with the user when Emacs is shutting down, use `kill-emacs-query-functions`, described below.

When Emacs is killed, all the information in the Emacs process, aside from files that have been saved, is lost. Because killing Emacs inadvertently can lose a lot of work, the `save-buffers-kill-terminal` command queries for confirmation if you have buffers that need saving or subprocesses that are running. It also runs the abnormal hook `kill-emacs-query-functions`:

*   User Option: **kill-emacs-query-functions**

    When `save-buffers-kill-terminal` is killing Emacs, it calls the functions in this hook, after asking the standard questions and before calling `kill-emacs`. The functions are called in order of appearance, with no arguments. Each function can ask for additional confirmation from the user. If any of them returns `nil`, `save-buffers-kill-emacs` does not kill Emacs, and does not run the remaining functions in this hook. Calling `kill-emacs` directly does not run this hook.

Next: [Suspending Emacs](Suspending-Emacs.html), Up: [Getting Out](Getting-Out.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
