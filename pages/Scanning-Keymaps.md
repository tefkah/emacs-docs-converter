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

Next: [Menu Keymaps](Menu-Keymaps.html), Previous: [Key Binding Commands](Key-Binding-Commands.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 22.16 Scanning Keymaps

This section describes functions used to scan all the current keymaps for the sake of printing help information.

*   Function: **accessible-keymaps** *keymap \&optional prefix*

    This function returns a list of all the keymaps that can be reached (via zero or more prefix keys) from `keymap`. The value is an association list with elements of the form `(key . map)`, where `key` is a prefix key whose definition in `keymap` is `map`.

    The elements of the alist are ordered so that the `key` increases in length. The first element is always `([] . keymap)`, because the specified keymap is accessible from itself with a prefix of no events.

    If `prefix` is given, it should be a prefix key sequence; then `accessible-keymaps` includes only the submaps whose prefixes start with `prefix`. These elements look just as they do in the value of `(accessible-keymaps)`; the only difference is that some elements are omitted.

    In the example below, the returned alist indicates that the key `ESC`, which is displayed as ‘`^[`’, is a prefix key whose definition is the sparse keymap `(keymap (83 . center-paragraph) (115 . foo))`.

        (accessible-keymaps (current-local-map))
        ⇒(([] keymap
              (27 keymap   ; Note this keymap for ESC is repeated below.
                  (83 . center-paragraph)
                  (115 . center-line))
              (9 . tab-to-tab-stop))

    ```
    ```

           ("^[" keymap
            (83 . center-paragraph)
            (115 . foo)))

    In the following example, `C-h` is a prefix key that uses a sparse keymap starting with `(keymap (118 . describe-variable)…)`. Another prefix, `C-x 4`, uses a keymap which is also the value of the variable `ctl-x-4-map`. The event `mode-line` is one of several dummy events used as prefixes for mouse actions in special parts of a window.

        (accessible-keymaps (current-global-map))
        ⇒ (([] keymap [set-mark-command beginning-of-line …
                           delete-backward-char])

    <!---->

            ("^H" keymap (118 . describe-variable) …
             (8 . help-for-help))

    <!---->

            ("^X" keymap [x-flush-mouse-queue …
             backward-kill-sentence])

    <!---->

            ("^[" keymap [mark-sexp backward-sexp …
             backward-kill-word])

    <!---->

            ("^X4" keymap (15 . display-buffer) …)

    <!---->

            ([mode-line] keymap
             (S-mouse-2 . mouse-split-window-horizontally) …))

    These are not all the keymaps you would see in actuality.

<!---->

*   Function: **map-keymap** *function keymap*

    The function `map-keymap` calls `function` once for each binding in `keymap`. It passes two arguments, the event type and the value of the binding. If `keymap` has a parent, the parent’s bindings are included as well. This works recursively: if the parent has itself a parent, then the grandparent’s bindings are also included and so on.

    This function is the cleanest way to examine all the bindings in a keymap.

<!---->

*   Function: **where-is-internal** *command \&optional keymap firstonly noindirect no-remap*

    This function is a subroutine used by the `where-is` command (see [Help](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help.html#Help) in The GNU Emacs Manual). It returns a list of all key sequences (of any length) that are bound to `command` in a set of keymaps.

    The argument `command` can be any object; it is compared with all keymap entries using `eq`.

    If `keymap` is `nil`, then the maps used are the current active keymaps, disregarding `overriding-local-map` (that is, pretending its value is `nil`). If `keymap` is a keymap, then the maps searched are `keymap` and the global keymap. If `keymap` is a list of keymaps, only those keymaps are searched.

    Usually it’s best to use `overriding-local-map` as the expression for `keymap`. Then `where-is-internal` searches precisely the keymaps that are active. To search only the global map, pass the value `(keymap)` (an empty keymap) as `keymap`.

    If `firstonly` is `non-ascii`, then the value is a single vector representing the first key sequence found, rather than a list of all possible key sequences. If `firstonly` is `t`, then the value is the first key sequence, except that key sequences consisting entirely of ASCII characters (or meta variants of ASCII characters) are preferred to all other key sequences and that the return value can never be a menu binding.

    If `noindirect` is non-`nil`, `where-is-internal` doesn’t look inside menu-items to find their commands. This makes it possible to search for a menu-item itself.

    The fifth argument, `no-remap`, determines how this function treats command remappings (see [Remapping Commands](Remapping-Commands.html)). There are two cases of interest:

    *   If a command `other-command` is remapped to `command`:

        If `no-remap` is `nil`, find the bindings for `other-command` and treat them as though they are also bindings for `command`. If `no-remap` is non-`nil`, include the vector `[remap other-command]` in the list of possible key sequences, instead of finding those bindings.

    *   If `command` is remapped to `other-command`:

        If `no-remap` is `nil`, return the bindings for `other-command` rather than `command`. If `no-remap` is non-`nil`, return the bindings for `command`, ignoring the fact that it is remapped.

<!---->

*   Command: **describe-bindings** *\&optional prefix buffer-or-name*

    This function creates a listing of all current key bindings, and displays it in a buffer named `*Help*`. The text is grouped by modes—minor modes first, then the major mode, then global bindings.

    If `prefix` is non-`nil`, it should be a prefix key; then the listing includes only keys that start with `prefix`.

    When several characters with consecutive ASCII codes have the same definition, they are shown together, as ‘`firstchar..lastchar`’. In this instance, you need to know the ASCII codes to understand which characters this means. For example, in the default global map, the characters ‘`SPC .. ~`’ are described by a single line. `SPC` is ASCII 32, `~` is ASCII 126, and the characters between them include all the normal printing characters, (e.g., letters, digits, punctuation, etc.); all these characters are bound to `self-insert-command`.

    If `buffer-or-name` is non-`nil`, it should be a buffer or a buffer name. Then `describe-bindings` lists that buffer’s bindings, instead of the current buffer’s.

Next: [Menu Keymaps](Menu-Keymaps.html), Previous: [Key Binding Commands](Key-Binding-Commands.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
