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

Next: [Locales](Locales.html), Previous: [Coding Systems](Coding-Systems.html), Up: [Non-ASCII Characters](Non_002dASCII-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 33.11 Input Methods

*Input methods* provide convenient ways of entering non-ASCII characters from the keyboard. Unlike coding systems, which translate non-ASCII characters to and from encodings meant to be read by programs, input methods provide human-friendly commands. (See [Input Methods](https://www.gnu.org/software/emacs/manual/html_node/emacs/Input-Methods.html#Input-Methods) in The GNU Emacs Manual, for information on how users use input methods to enter text.) How to define input methods is not yet documented in this manual, but here we describe how to use them.

Each input method has a name, which is currently a string; in the future, symbols may also be usable as input method names.

*   Variable: **current-input-method**

    This variable holds the name of the input method now active in the current buffer. (It automatically becomes local in each buffer when set in any fashion.) It is `nil` if no input method is active in the buffer now.

<!---->

*   User Option: **default-input-method**

    This variable holds the default input method for commands that choose an input method. Unlike `current-input-method`, this variable is normally global.

<!---->

*   Command: **set-input-method** *input-method*

    This command activates input method `input-method` for the current buffer. It also sets `default-input-method` to `input-method`. If `input-method` is `nil`, this command deactivates any input method for the current buffer.

<!---->

*   Function: **read-input-method-name** *prompt \&optional default inhibit-null*

    This function reads an input method name with the minibuffer, prompting with `prompt`. If `default` is non-`nil`, that is returned by default, if the user enters empty input. However, if `inhibit-null` is non-`nil`, empty input signals an error.

    The returned value is a string.

<!---->

*   Variable: **input-method-alist**

    This variable defines all the supported input methods. Each element defines one input method, and should have the form:

        (input-method language-env activate-func
         title description args...)

    Here `input-method` is the input method name, a string; `language-env` is another string, the name of the language environment this input method is recommended for. (That serves only for documentation purposes.)

    `activate-func` is a function to call to activate this method. The `args`, if any, are passed as arguments to `activate-func`. All told, the arguments to `activate-func` are `input-method` and the `args`.

    `title` is a string to display in the mode line while this method is active. `description` is a string describing this method and what it is good for.

The fundamental interface to input methods is through the variable `input-method-function`. See [Reading One Event](Reading-One-Event.html), and [Invoking the Input Method](Invoking-the-Input-Method.html).

Next: [Locales](Locales.html), Previous: [Coding Systems](Coding-Systems.html), Up: [Non-ASCII Characters](Non_002dASCII-Characters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
