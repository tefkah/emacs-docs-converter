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

Next: [Keys in Documentation](Keys-in-Documentation.html), Previous: [Documentation Basics](Documentation-Basics.html), Up: [Documentation](Documentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 24.2 Access to Documentation Strings

*   Function: **documentation-property** *symbol property \&optional verbatim*

    This function returns the documentation string recorded in `symbol`’s property list under property `property`. It is most often used to look up the documentation strings of variables, for which `property` is `variable-documentation`. However, it can also be used to look up other kinds of documentation, such as for customization groups (but for function documentation, use the `documentation` function, below).

    If the property value refers to a documentation string stored in the `DOC` file or a byte-compiled file, this function looks up that string and returns it.

    If the property value isn’t `nil`, isn’t a string, and doesn’t refer to text in a file, then it is evaluated as a Lisp expression to obtain a string.

    Finally, this function passes the string through `substitute-command-keys` to substitute key bindings (see [Keys in Documentation](Keys-in-Documentation.html)). It skips this step if `verbatim` is non-`nil`.

        (documentation-property 'command-line-processed
           'variable-documentation)
             ⇒ "Non-nil once command line has been processed"

    <!---->

        (symbol-plist 'command-line-processed)
             ⇒ (variable-documentation 188902)

    <!---->

        (documentation-property 'emacs 'group-documentation)
             ⇒ "Customization of the One True Editor."

<!---->

*   Function: **documentation** *function \&optional verbatim*

    This function returns the documentation string of `function`. It handles macros, named keyboard macros, and special forms, as well as ordinary functions.

    If `function` is a symbol, this function first looks for the `function-documentation` property of that symbol; if that has a non-`nil` value, the documentation comes from that value (if the value is not a string, it is evaluated).

    If `function` is not a symbol, or if it has no `function-documentation` property, then `documentation` extracts the documentation string from the actual function definition, reading it from a file if called for.

    Finally, unless `verbatim` is non-`nil`, this function calls `substitute-command-keys`. The result is the documentation string to return.

    The `documentation` function signals a `void-function` error if `function` has no function definition. However, it is OK if the function definition has no documentation string. In that case, `documentation` returns `nil`.

<!---->

*   Function: **face-documentation** *face*

    This function returns the documentation string of `face` as a face.

Here is an example of using the two functions, `documentation` and `documentation-property`, to display the documentation strings for several symbols in a `*Help*` buffer.

    (defun describe-symbols (pattern)
      "Describe the Emacs Lisp symbols matching PATTERN.
    All symbols that have PATTERN in their name are described
    in the *Help* buffer."
      (interactive "sDescribe symbols matching: ")
      (let ((describe-func
             (lambda (s)

<!---->

               ;; Print description of symbol.
               (if (fboundp s)             ; It is a function.
                   (princ
                    (format "%s\t%s\n%s\n\n" s
                      (if (commandp s)
                          (let ((keys (where-is-internal s)))
                            (if keys
                                (concat
                                 "Keys: "
                                 (mapconcat 'key-description
                                            keys " "))
                              "Keys: none"))
                        "Function")

<!---->

                      (or (documentation s)
                          "not documented"))))

               (if (boundp s)              ; It is a variable.

<!---->

                   (princ
                    (format "%s\t%s\n%s\n\n" s
                      (if (custom-variable-p s)
                          "Option " "Variable")

<!---->

                      (or (documentation-property
                            s 'variable-documentation)
                          "not documented"))))))
            sym-list)

```
```

        ;; Build a list of symbols that match pattern.
        (mapatoms (lambda (sym)
                    (if (string-match pattern (symbol-name sym))
                        (setq sym-list (cons sym sym-list)))))

```
```

        ;; Display the data.
        (help-setup-xref (list 'describe-symbols pattern) (interactive-p))
        (with-help-window (help-buffer)
          (mapcar describe-func (sort sym-list 'string<)))))

The `describe-symbols` function works like `apropos`, but provides more information.

    (describe-symbols "goal")

    ---------- Buffer: *Help* ----------
    goal-column     Option
    Semipermanent goal column for vertical motion, as set by …

```
```

    minibuffer-temporary-goal-position      Variable
    not documented

```
```

    set-goal-column Keys: C-x C-n
    Set the current horizontal position as a goal for C-n and C-p.

<!---->

    Those commands will move to this position in the line moved to
    rather than trying to keep the same horizontal position.
    With a non-nil argument ARG, clears out the goal column
    so that C-n and C-p resume vertical motion.
    The goal column is stored in the variable ‘goal-column’.

    (fn ARG)

```
```

    temporary-goal-column   Variable
    Current goal column for vertical motion.
    It is the column where point was at the start of the current run
    of vertical motion commands.

    When moving by visual lines via the function ‘line-move-visual’, it is a cons
    cell (COL . HSCROLL), where COL is the x-position, in pixels,
    divided by the default column width, and HSCROLL is the number of
    columns by which window is scrolled from left margin.

    When the ‘track-eol’ feature is doing its job, the value is
    ‘most-positive-fixnum’.
    ---------- Buffer: *Help* ----------

*   Function: **Snarf-documentation** *filename*

    This function is used when building Emacs, just before the runnable Emacs is dumped. It finds the positions of the documentation strings stored in the file `filename`, and records those positions into memory in the function definitions and variable property lists. See [Building Emacs](Building-Emacs.html).

    Emacs reads the file `filename` from the `emacs/etc` directory. When the dumped Emacs is later executed, the same file will be looked for in the directory `doc-directory`. Usually `filename` is `"DOC"`.

<!---->

*   Variable: **doc-directory**

    This variable holds the name of the directory which should contain the file `"DOC"` that contains documentation strings for built-in and preloaded functions and variables.

    In most cases, this is the same as `data-directory`. They may be different when you run Emacs from the directory where you built it, without actually installing it. See [Definition of data-directory](Help-Functions.html#Definition-of-data_002ddirectory).

Next: [Keys in Documentation](Keys-in-Documentation.html), Previous: [Documentation Basics](Documentation-Basics.html), Up: [Documentation](Documentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
