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

Next: [Mode Help](Mode-Help.html), Previous: [Major Mode Conventions](Major-Mode-Conventions.html), Up: [Major Modes](Major-Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.2.2 How Emacs Chooses a Major Mode

When Emacs visits a file, it automatically selects a major mode for the buffer based on information in the file name or in the file itself. It also processes local variables specified in the file text.

*   Command: **normal-mode** *\&optional find-file*

    This function establishes the proper major mode and buffer-local variable bindings for the current buffer. It calls `set-auto-mode` (see below). As of Emacs 26.1, it no longer runs `hack-local-variables`, this now being done in `run-mode-hooks` at the initialization of major modes (see [Mode Hooks](Mode-Hooks.html)).

    If the `find-file` argument to `normal-mode` is non-`nil`, `normal-mode` assumes that the `find-file` function is calling it. In this case, it may process local variables in the ‘`-*-`’ line or at the end of the file. The variable `enable-local-variables` controls whether to do so. See [Local Variables in Files](https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Variables.html#File-Variables) in The GNU Emacs Manual, for the syntax of the local variables section of a file.

    If you run `normal-mode` interactively, the argument `find-file` is normally `nil`. In this case, `normal-mode` unconditionally processes any file local variables.

    The function calls `set-auto-mode` to choose and set a major mode. If this does not specify a mode, the buffer stays in the major mode determined by the default value of `major-mode` (see below).

    `normal-mode` uses `condition-case` around the call to the major mode command, so errors are caught and reported as a ‘`File mode specification error`’, followed by the original error message.

<!---->

*   Function: **set-auto-mode** *\&optional keep-mode-if-same*

    This function selects and sets the major mode that is appropriate for the current buffer. It bases its decision (in order of precedence) on the ‘`-*-`’<!-- /@w --> line, on any ‘`mode:`’ local variable near the end of a file, on the ‘`#!`’<!-- /@w --> line (using `interpreter-mode-alist`), on the text at the beginning of the buffer (using `magic-mode-alist`), and finally on the visited file name (using `auto-mode-alist`). See [How Major Modes are Chosen](https://www.gnu.org/software/emacs/manual/html_node/emacs/Choosing-Modes.html#Choosing-Modes) in The GNU Emacs Manual. If `enable-local-variables` is `nil`, `set-auto-mode` does not check the ‘`-*-`’<!-- /@w --> line, or near the end of the file, for any mode tag.

    There are some file types where it is not appropriate to scan the file contents for a mode specifier. For example, a tar archive may happen to contain, near the end of the file, a member file that has a local variables section specifying a mode for that particular file. This should not be applied to the containing tar file. Similarly, a tiff image file might just happen to contain a first line that seems to match the ‘`-*-`’<!-- /@w --> pattern. For these reasons, both these file extensions are members of the list `inhibit-local-variables-regexps`. Add patterns to this list to prevent Emacs searching them for local variables of any kind (not just mode specifiers).

    If `keep-mode-if-same` is non-`nil`, this function does not call the mode command if the buffer is already in the proper major mode. For instance, `set-visited-file-name` sets this to `t` to avoid killing buffer local variables that the user may have set.

<!---->

*   Function: **set-buffer-major-mode** *buffer*

    This function sets the major mode of `buffer` to the default value of `major-mode`; if that is `nil`, it uses the current buffer’s major mode (if that is suitable). As an exception, if `buffer`’s name is `*scratch*`, it sets the mode to `initial-major-mode`.

    The low-level primitives for creating buffers do not use this function, but medium-level commands such as `switch-to-buffer` and `find-file-noselect` use it whenever they create buffers.

<!---->

*   User Option: **initial-major-mode**

    The value of this variable determines the major mode of the initial `*scratch*` buffer. The value should be a symbol that is a major mode command. The default value is `lisp-interaction-mode`.

<!---->

*   Variable: **interpreter-mode-alist**

    This variable specifies major modes to use for scripts that specify a command interpreter in a ‘`#!`’ line. Its value is an alist with elements of the form `(regexp . mode)`; this says to use mode `mode` if the file specifies an interpreter which matches ``\\`regexp\\'``. For example, one of the default elements is `("python[0-9.]*" . python-mode)`.

<!---->

*   Variable: **magic-mode-alist**

    This variable’s value is an alist with elements of the form `(regexp . function)`, where `regexp` is a regular expression and `function` is a function or `nil`. After visiting a file, `set-auto-mode` calls `function` if the text at the beginning of the buffer matches `regexp` and `function` is non-`nil`; if `function` is `nil`, `auto-mode-alist` gets to decide the mode.

<!---->

*   Variable: **magic-fallback-mode-alist**

    This works like `magic-mode-alist`, except that it is handled only if `auto-mode-alist` does not specify a mode for this file.

<!---->

*   Variable: **auto-mode-alist**

    This variable contains an association list of file name patterns (regular expressions) and corresponding major mode commands. Usually, the file name patterns test for suffixes, such as ‘`.el`’ and ‘`.c`’, but this need not be the case. An ordinary element of the alist looks like `(regexp . mode-function)`.

    For example,

        (("\\`/tmp/fol/" . text-mode)
         ("\\.texinfo\\'" . texinfo-mode)
         ("\\.texi\\'" . texinfo-mode)

    <!---->

         ("\\.el\\'" . emacs-lisp-mode)
         ("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         …)

    When you visit a file whose expanded file name (see [File Name Expansion](File-Name-Expansion.html)), with version numbers and backup suffixes removed using `file-name-sans-versions` (see [File Name Components](File-Name-Components.html)), matches a `regexp`, `set-auto-mode` calls the corresponding `mode-function`. This feature enables Emacs to select the proper major mode for most files.

    If an element of `auto-mode-alist` has the form `(regexp function t)`, then after calling `function`, Emacs searches `auto-mode-alist` again for a match against the portion of the file name that did not match before. This feature is useful for uncompression packages: an entry of the form `("\\.gz\\'" function t)` can uncompress the file and then put the uncompressed file in the proper mode according to the name sans ‘`.gz`’.

    Here is an example of how to prepend several pattern pairs to `auto-mode-alist`. (You might use this sort of expression in your init file.)

        (setq auto-mode-alist
          (append
           ;; File name (within directory) starts with a dot.
           '(("/\\.[^/]*\\'" . fundamental-mode)
             ;; File name has no dot.
             ("/[^\\./]*\\'" . fundamental-mode)
             ;; File name ends in ‘.C’.
             ("\\.C\\'" . c++-mode))
           auto-mode-alist))

Next: [Mode Help](Mode-Help.html), Previous: [Major Mode Conventions](Major-Mode-Conventions.html), Up: [Major Modes](Major-Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
