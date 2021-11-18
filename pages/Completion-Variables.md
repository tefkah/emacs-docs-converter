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

Next: [Programmed Completion](Programmed-Completion.html), Previous: [Reading File Names](Reading-File-Names.html), Up: [Completion](Completion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 20.6.6 Completion Variables

Here are some variables that can be used to alter the default completion behavior.

*   User Option: **completion-styles**

    The value of this variable is a list of completion style (symbols) to use for performing completion. A *completion style* is a set of rules for generating completions. Each symbol occurring this list must have a corresponding entry in `completion-styles-alist`.

<!---->

*   Variable: **completion-styles-alist**

    This variable stores a list of available completion styles. Each element in the list has the form

        (style try-completion all-completions doc)

    Here, `style` is the name of the completion style (a symbol), which may be used in the `completion-styles` variable to refer to this style; `try-completion` is the function that does the completion; `all-completions` is the function that lists the completions; and `doc` is a string describing the completion style.

    The `try-completion` and `all-completions` functions should each accept four arguments: `string`, `collection`, `predicate`, and `point`. The `string`, `collection`, and `predicate` arguments have the same meanings as in `try-completion` (see [Basic Completion](Basic-Completion.html)), and the `point` argument is the position of point within `string`. Each function should return a non-`nil` value if it performed its job, and `nil` if it did not (e.g., if there is no way to complete `string` according to the completion style).

    When the user calls a completion command like `minibuffer-complete` (see [Completion Commands](Completion-Commands.html)), Emacs looks for the first style listed in `completion-styles` and calls its `try-completion` function. If this function returns `nil`, Emacs moves to the next listed completion style and calls its `try-completion` function, and so on until one of the `try-completion` functions successfully performs completion and returns a non-`nil` value. A similar procedure is used for listing completions, via the `all-completions` functions.

    See [Completion Styles](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html#Completion-Styles) in The GNU Emacs Manual, for a description of the available completion styles.

<!---->

*   User Option: **completion-category-overrides**

    This variable specifies special completion styles and other completion behaviors to use when completing certain types of text. Its value should be an alist with elements of the form `(category . alist)`. `category` is a symbol describing what is being completed; currently, the `buffer`, `file`, and `unicode-name` categories are defined, but others can be defined via specialized completion functions (see [Programmed Completion](Programmed-Completion.html)). `alist` is an association list describing how completion should behave for the corresponding category. The following alist keys are supported:

    *   `styles`

        The value should be a list of completion styles (symbols).

    *   `cycle`

        The value should be a value for `completion-cycle-threshold` (see [Completion Options](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Options.html#Completion-Options) in The GNU Emacs Manual) for this category.

    Additional alist entries may be defined in the future.

<!---->

*   Variable: **completion-extra-properties**

    This variable is used to specify extra properties of the current completion command. It is intended to be let-bound by specialized completion commands. Its value should be a list of property and value pairs. The following properties are supported:

    *   `:annotation-function`

        The value should be a function to add annotations in the completions buffer. This function must accept one argument, a completion, and should either return `nil` or a string to be displayed next to the completion.

    *   `:exit-function`

        The value should be a function to run after performing completion. The function should accept two arguments, `string` and `status`, where `string` is the text to which the field was completed, and `status` indicates what kind of operation happened: `finished` if text is now complete, `sole` if the text cannot be further completed but completion is not finished, or `exact` if the text is a valid completion but may be further completed.

Next: [Programmed Completion](Programmed-Completion.html), Previous: [Reading File Names](Reading-File-Names.html), Up: [Completion](Completion.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
