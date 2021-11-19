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

Next: [Synchronous Processes](Synchronous-Processes.html), Previous: [Subprocess Creation](Subprocess-Creation.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 38.2 Shell Arguments

Lisp programs sometimes need to run a shell and give it a command that contains file names that were specified by the user. These programs ought to be able to support any valid file name. But the shell gives special treatment to certain characters, and if these characters occur in the file name, they will confuse the shell. To handle these characters, use the function `shell-quote-argument`:

*   Function: **shell-quote-argument** *argument*

    This function returns a string that represents, in shell syntax, an argument whose actual contents are `argument`. It should work reliably to concatenate the return value into a shell command and then pass it to a shell for execution.

    Precisely what this function does depends on your operating system. The function is designed to work with the syntax of your system’s standard shell; if you use an unusual shell, you will need to redefine this function. See [Security Considerations](Security-Considerations.html).

        ;; This example shows the behavior on GNU and Unix systems.
        (shell-quote-argument "foo > bar")
             ⇒ "foo\\ \\>\\ bar"

        ;; This example shows the behavior on MS-DOS and MS-Windows.
        (shell-quote-argument "foo > bar")
             ⇒ "\"foo > bar\""

    Here’s an example of using `shell-quote-argument` to construct a shell command:

        (concat "diff -u "
                (shell-quote-argument oldfile)
                " "
                (shell-quote-argument newfile))

The following two functions are useful for combining a list of individual command-line argument strings into a single string, and taking a string apart into a list of individual command-line arguments. These functions are mainly intended for converting user input in the minibuffer, a Lisp string, into a list of string arguments to be passed to `make-process`, `call-process` or `start-process`, or for converting such lists of arguments into a single Lisp string to be presented in the minibuffer or echo area. Note that if a shell is involved (e.g., if using `call-process-shell-command`), arguments should still be protected by `shell-quote-argument`; `combine-and-quote-strings` is *not* intended to protect special characters from shell evaluation.

*   Function: **split-string-and-unquote** *string \&optional separators*

    This function splits `string` into substrings at matches for the regular expression `separators`, like `split-string` does (see [Creating Strings](Creating-Strings.html)); in addition, it removes quoting from the substrings. It then makes a list of the substrings and returns it.

    If `separators` is omitted or `nil`, it defaults to `"\\s-+"`, which is a regular expression that matches one or more characters with whitespace syntax (see [Syntax Class Table](Syntax-Class-Table.html)).

    This function supports two types of quoting: enclosing a whole string in double quotes `"…"`, and quoting individual characters with a backslash escape ‘`\`’. The latter is also used in Lisp strings, so this function can handle those as well.

<!---->

*   Function: **combine-and-quote-strings** *list-of-strings \&optional separator*

    This function concatenates `list-of-strings` into a single string, quoting each string as necessary. It also sticks the `separator` string between each pair of strings; if `separator` is omitted or `nil`, it defaults to `" "`. The return value is the resulting string.

    The strings in `list-of-strings` that need quoting are those that include `separator` as their substring. Quoting a string encloses it in double quotes `"…"`. In the simplest case, if you are consing a command from the individual command-line arguments, every argument that includes embedded blanks will be quoted.

Next: [Synchronous Processes](Synchronous-Processes.html), Previous: [Subprocess Creation](Subprocess-Creation.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
