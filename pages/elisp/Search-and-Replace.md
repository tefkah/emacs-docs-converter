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

Next: [Standard Regexps](Standard-Regexps.html), Previous: [Match Data](Match-Data.html), Up: [Searching and Matching](Searching-and-Matching.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 34.7 Search and Replace

If you want to find all matches for a regexp in part of the buffer, and replace them, the best way is to write an explicit loop using `re-search-forward` and `replace-match`, like this:

    (while (re-search-forward "foo[ \t]+bar" nil t)
      (replace-match "foobar"))

See [Replacing the Text that Matched](Replacing-Match.html), for a description of `replace-match`.

However, replacing matches in a string is more complex, especially if you want to do it efficiently. So Emacs provides a function to do this.

*   Function: **replace-regexp-in-string** *regexp rep string \&optional fixedcase literal subexp start*

    This function copies `string` and searches it for matches for `regexp`, and replaces them with `rep`. It returns the modified copy. If `start` is non-`nil`, the search for matches starts at that index in `string`, and the returned value does not include the first `start` characters of `string`. To get the whole transformed string, concatenate the first `start` characters of `string` with the return value.

    This function uses `replace-match` to do the replacement, and it passes the optional arguments `fixedcase`, `literal` and `subexp` along to `replace-match`.

    Instead of a string, `rep` can be a function. In that case, `replace-regexp-in-string` calls `rep` for each match, passing the text of the match as its sole argument. It collects the value `rep` returns and passes that to `replace-match` as the replacement string. The match data at this point are the result of matching `regexp` against a substring of `string`.

If you want to write a command along the lines of `query-replace`, you can use `perform-replace` to do the work.

*   Function: **perform-replace** *from-string replacements query-flag regexp-flag delimited-flag \&optional repeat-count map start end backward region-noncontiguous-p*

    This function is the guts of `query-replace` and related commands. It searches for occurrences of `from-string` in the text between positions `start` and `end` and replaces some or all of them. If `start` is `nil` (or omitted), point is used instead, and the end of the buffer’s accessible portion is used for `end`. (If the optional argument `backward` is non-`nil`, the search starts at `end` and goes backward.)

    If `query-flag` is `nil`, it replaces all occurrences; otherwise, it asks the user what to do about each one.

    If `regexp-flag` is non-`nil`, then `from-string` is considered a regular expression; otherwise, it must match literally. If `delimited-flag` is non-`nil`, then only replacements surrounded by word boundaries are considered.

    The argument `replacements` specifies what to replace occurrences with. If it is a string, that string is used. It can also be a list of strings, to be used in cyclic order.

    If `replacements` is a cons cell, `(function . data)`<!-- /@w -->, this means to call `function` after each match to get the replacement text. This function is called with two arguments: `data`, and the number of replacements already made.

    If `repeat-count` is non-`nil`, it should be an integer. Then it specifies how many times to use each of the strings in the `replacements` list before advancing cyclically to the next one.

    If `from-string` contains upper-case letters, then `perform-replace` binds `case-fold-search` to `nil`, and it uses the `replacements` without altering their case.

    Normally, the keymap `query-replace-map` defines the possible user responses for queries. The argument `map`, if non-`nil`, specifies a keymap to use instead of `query-replace-map`.

    Non-`nil` `region-noncontiguous-p` means that the region between `start` and `end` is composed of noncontiguous pieces. The most common example of this is a rectangular region, where the pieces are separated by newline characters.

    This function uses one of two functions to search for the next occurrence of `from-string`. These functions are specified by the values of two variables: `replace-re-search-function` and `replace-search-function`. The former is called when the argument `regexp-flag` is non-`nil`, the latter when it is `nil`.

<!---->

*   Variable: **query-replace-map**

    This variable holds a special keymap that defines the valid user responses for `perform-replace` and the commands that use it, as well as `y-or-n-p` and `map-y-or-n-p`. This map is unusual in two ways:

    *   The key bindings are not commands, just symbols that are meaningful to the functions that use this map.
    *   Prefix keys are not supported; each key binding must be for a single-event key sequence. This is because the functions don’t use `read-key-sequence` to get the input; instead, they read a single event and look it up “by hand”.

Here are the meaningful bindings for `query-replace-map`. Several of them are meaningful only for `query-replace` and friends.

*   `act`

    Do take the action being considered—in other words, “yes”.

*   `skip`

    Do not take action for this question—in other words, “no”.

*   `exit`

    Answer this question “no”, and give up on the entire series of questions, assuming that the answers will be “no”.

*   `exit-prefix`

    Like `exit`, but add the key that was pressed to `unread-command-events` (see [Event Input Misc](Event-Input-Misc.html)).

*   `act-and-exit`

    Answer this question “yes”, and give up on the entire series of questions, assuming that subsequent answers will be “no”.

*   `act-and-show`

    Answer this question “yes”, but show the results—don’t advance yet to the next question.

*   `automatic`

    Answer this question and all subsequent questions in the series with “yes”, without further user interaction.

*   `backup`

    Move back to the previous place that a question was asked about.

*   `undo`

    Undo last replacement and move back to the place where that replacement was performed.

*   `undo-all`

    Undo all replacements and move back to the place where the first replacement was performed.

*   `edit`

    Enter a recursive edit to deal with this question—instead of any other action that would normally be taken.

*   `edit-replacement`

    Edit the replacement for this question in the minibuffer.

*   `delete-and-edit`

    Delete the text being considered, then enter a recursive edit to replace it.

*   *   `recenter`
    *   `scroll-up`
    *   `scroll-down`
    *   `scroll-other-window`
    *   `scroll-other-window-down`

    Perform the specified window scroll operation, then ask the same question again. Only `y-or-n-p` and related functions use this answer.

*   `quit`

    Perform a quit right away. Only `y-or-n-p` and related functions use this answer.

*   `help`

    Display some help, then ask again.

<!---->

*   Variable: **multi-query-replace-map**

    This variable holds a keymap that extends `query-replace-map` by providing additional keybindings that are useful in multi-buffer replacements. The additional bindings are:

    *   `automatic-all`

        Answer this question and all subsequent questions in the series with “yes”, without further user interaction, for all remaining buffers.

    *   `exit-current`

        Answer this question “no”, and give up on the entire series of questions for the current buffer. Continue to the next buffer in the sequence.

<!---->

*   Variable: **replace-search-function**

    This variable specifies a function that `perform-replace` calls to search for the next string to replace. Its default value is `search-forward`. Any other value should name a function of 3 arguments: the first 3 arguments of `search-forward` (see [String Search](String-Search.html)).

<!---->

*   Variable: **replace-re-search-function**

    This variable specifies a function that `perform-replace` calls to search for the next regexp to replace. Its default value is `re-search-forward`. Any other value should name a function of 3 arguments: the first 3 arguments of `re-search-forward` (see [Regexp Search](Regexp-Search.html)).

Next: [Standard Regexps](Standard-Regexps.html), Previous: [Match Data](Match-Data.html), Up: [Searching and Matching](Searching-and-Matching.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
