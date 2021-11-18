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

Next: [Controlling Active Maps](Controlling-Active-Maps.html), Previous: [Active Keymaps](Active-Keymaps.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 22.8 Searching the Active Keymaps

Here is a pseudo-Lisp summary of how Emacs searches the active keymaps:

    (or (if overriding-terminal-local-map
            (find-in overriding-terminal-local-map))
        (if overriding-local-map
            (find-in overriding-local-map)
          (or (find-in (get-char-property (point) 'keymap))
              (find-in-any emulation-mode-map-alists)
              (find-in-any minor-mode-overriding-map-alist)
              (find-in-any minor-mode-map-alist)
              (if (get-text-property (point) 'local-map)
                  (find-in (get-char-property (point) 'local-map))
                (find-in (current-local-map)))))
        (find-in (current-global-map)))

Here, `find-in` and `find-in-any` are pseudo functions that search in one keymap and in an alist of keymaps, respectively. Note that the `set-transient-map` function works by setting `overriding-terminal-local-map` (see [Controlling Active Maps](Controlling-Active-Maps.html)).

In the above pseudo-code, if a key sequence starts with a mouse event (see [Mouse Events](Mouse-Events.html)), that event’s position is used instead of point, and the event’s buffer is used instead of the current buffer. In particular, this affects how the `keymap` and `local-map` properties are looked up. If a mouse event occurs on a string embedded with a `display`, `before-string`, or `after-string` property (see [Special Properties](Special-Properties.html)), and the string has a non-`nil` `keymap` or `local-map` property, that overrides the corresponding property in the underlying buffer text (i.e., the property specified by the underlying text is ignored).

When a key binding is found in one of the active keymaps, and that binding is a command, the search is over—the command is executed. However, if the binding is a symbol with a value or a string, Emacs replaces the input key sequences with the variable’s value or the string, and restarts the search of the active keymaps. See [Key Lookup](Key-Lookup.html).

The command which is finally found might also be remapped. See [Remapping Commands](Remapping-Commands.html).

Next: [Controlling Active Maps](Controlling-Active-Maps.html), Previous: [Active Keymaps](Active-Keymaps.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
