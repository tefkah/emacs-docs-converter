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

Next: [Mode Line Top](Mode-Line-Top.html), Previous: [Mode Line Basics](Mode-Line-Basics.html), Up: [Mode Line Format](Mode-Line-Format.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.4.2 The Data Structure of the Mode Line

The mode line contents are controlled by a data structure called a *mode line construct*, made up of lists, strings, symbols, and numbers kept in buffer-local variables. Each data type has a specific meaning for the mode line appearance, as described below. The same data structure is used for constructing frame titles (see [Frame Titles](Frame-Titles.html)) and header lines (see [Header Lines](Header-Lines.html)).

A mode line construct may be as simple as a fixed string of text, but it usually specifies how to combine fixed strings with variables’ values to construct the text. Many of these variables are themselves defined to have mode line constructs as their values.

Here are the meanings of various data types as mode line constructs:

*   `string`

    A string as a mode line construct appears verbatim except for *`%`-constructs* in it. These stand for substitution of other data; see [%-Constructs](_0025_002dConstructs.html).

    If parts of the string have `face` properties, they control display of the text just as they would text in the buffer. Any characters which have no `face` properties are displayed, by default, in the face `mode-line` or `mode-line-inactive` (see [Standard Faces](https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html#Standard-Faces) in The GNU Emacs Manual). The `help-echo` and `keymap` properties in `string` have special meanings. See [Properties in Mode](Properties-in-Mode.html).

*   `symbol`

    A symbol as a mode line construct stands for its value. The value of `symbol` is used as a mode line construct, in place of `symbol`. However, the symbols `t` and `nil` are ignored, as is any symbol whose value is void.

    There is one exception: if the value of `symbol` is a string, it is displayed verbatim: the `%`-constructs are not recognized.

    Unless `symbol` is marked as risky (i.e., it has a non-`nil` `risky-local-variable` property), all text properties specified in `symbol`’s value are ignored. This includes the text properties of strings in `symbol`’s value, as well as all `:eval` and `:propertize` forms in it. (The reason for this is security: non-risky variables could be set automatically from file variables without prompting the user.)

*   *   `(string rest…)`
    *   `(list rest…)`

    A list whose first element is a string or list means to process all the elements recursively and concatenate the results. This is the most common form of mode line construct.

*   `(:eval form)`

    A list whose first element is the symbol `:eval` says to evaluate `form`, and use the result as a string to display. Make sure this evaluation cannot load any files, as doing so could cause infinite recursion.

*   `(:propertize elt props…)`

    A list whose first element is the symbol `:propertize` says to process the mode line construct `elt` recursively, then add the text properties specified by `props` to the result. The argument `props` should consist of zero or more pairs `text-property` `value`. If `elt` is or produces a string with text properties, all the characters of that string should have the same properties, or else some of them might be removed by `:propertize`.

*   `(symbol then else)`

    A list whose first element is a symbol that is not a keyword specifies a conditional. Its meaning depends on the value of `symbol`. If `symbol` has a non-`nil` value, the second element, `then`, is processed recursively as a mode line construct. Otherwise, the third element, `else`, is processed recursively. You may omit `else`; then the mode line construct displays nothing if the value of `symbol` is `nil` or void.

*   `(width rest…)`

    A list whose first element is an integer specifies truncation or padding of the results of `rest`. The remaining elements `rest` are processed recursively as mode line constructs and concatenated together. When `width` is positive, the result is space filled on the right if its width is less than `width`. When `width` is negative, the result is truncated on the right to -`width` columns if its width exceeds -`width`.

    For example, the usual way to show what percentage of a buffer is above the top of the window is to use a list like this: `(-3 "%p")`.

Next: [Mode Line Top](Mode-Line-Top.html), Previous: [Mode Line Basics](Mode-Line-Basics.html), Up: [Mode Line Format](Mode-Line-Format.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
