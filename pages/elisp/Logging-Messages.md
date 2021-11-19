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

Next: [Echo Area Customization](Echo-Area-Customization.html), Previous: [Progress](Progress.html), Up: [The Echo Area](The-Echo-Area.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.4.3 Logging Messages in `*Messages*`

Almost all the messages displayed in the echo area are also recorded in the `*Messages*` buffer so that the user can refer back to them. This includes all the messages that are output with `message`. By default, this buffer is read-only and uses the major mode `messages-buffer-mode`. Nothing prevents the user from killing the `*Messages*` buffer, but the next display of a message recreates it. Any Lisp code that needs to access the `*Messages*` buffer directly and wants to ensure that it exists should use the function `messages-buffer`.

*   Function: **messages-buffer**

    This function returns the `*Messages*` buffer. If it does not exist, it creates it, and switches it to `messages-buffer-mode`.

<!---->

*   User Option: **message-log-max**

    This variable specifies how many lines to keep in the `*Messages*` buffer. The value `t` means there is no limit on how many lines to keep. The value `nil` disables message logging entirely. Here’s how to display a message and prevent it from being logged:

        (let (message-log-max)
          (message …))

To make `*Messages*` more convenient for the user, the logging facility combines successive identical messages. It also combines successive related messages for the sake of two cases: question followed by answer, and a series of progress messages.

A question followed by an answer has two messages like the ones produced by `y-or-n-p`: the first is ‘`question`’, and the second is ‘`question...answer`’. The first message conveys no additional information beyond what’s in the second, so logging the second message discards the first from the log.

A series of progress messages has successive messages like those produced by `make-progress-reporter`. They have the form ‘`base...how-far`’, where `base` is the same each time, while `how-far` varies. Logging each message in the series discards the previous one, provided they are consecutive.

The functions `make-progress-reporter` and `y-or-n-p` don’t have to do anything special to activate the message log combination feature. It operates whenever two consecutive messages are logged that share a common prefix ending in ‘`...`’.

Next: [Echo Area Customization](Echo-Area-Customization.html), Previous: [Progress](Progress.html), Up: [The Echo Area](The-Echo-Area.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]