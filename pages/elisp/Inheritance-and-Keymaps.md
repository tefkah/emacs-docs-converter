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

Next: [Prefix Keys](Prefix-Keys.html), Previous: [Creating Keymaps](Creating-Keymaps.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 22.5 Inheritance and Keymaps

A keymap can inherit the bindings of another keymap, which we call the *parent keymap*. Such a keymap looks like this:

    (keymap elements… . parent-keymap)

The effect is that this keymap inherits all the bindings of `parent-keymap`, whatever they may be at the time a key is looked up, but can add to them or override them with `elements`.

If you change the bindings in `parent-keymap` using `define-key` or other key-binding functions, these changed bindings are visible in the inheriting keymap, unless shadowed by the bindings made by `elements`. The converse is not true: if you use `define-key` to change bindings in the inheriting keymap, these changes are recorded in `elements`, but have no effect on `parent-keymap`.

The proper way to construct a keymap with a parent is to use `set-keymap-parent`; if you have code that directly constructs a keymap with a parent, please convert the program to use `set-keymap-parent` instead.

*   Function: **keymap-parent** *keymap*

    This returns the parent keymap of `keymap`. If `keymap` has no parent, `keymap-parent` returns `nil`.

<!---->

*   Function: **set-keymap-parent** *keymap parent*

    This sets the parent keymap of `keymap` to `parent`, and returns `parent`. If `parent` is `nil`, this function gives `keymap` no parent at all.

    If `keymap` has submaps (bindings for prefix keys), they too receive new parent keymaps that reflect what `parent` specifies for those prefix keys.

Here is an example showing how to make a keymap that inherits from `text-mode-map`:

    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map text-mode-map)
      map)

A non-sparse keymap can have a parent too, but this is not very useful. A non-sparse keymap always specifies something as the binding for every numeric character code without modifier bits, even if it is `nil`, so these character’s bindings are never inherited from the parent keymap.

Sometimes you want to make a keymap that inherits from more than one map. You can use the function `make-composed-keymap` for this.

*   Function: **make-composed-keymap** *maps \&optional parent*

    This function returns a new keymap composed of the existing keymap(s) `maps`, and optionally inheriting from a parent keymap `parent`. `maps` can be a single keymap or a list of more than one. When looking up a key in the resulting new map, Emacs searches in each of the `maps` in turn, and then in `parent`, stopping at the first match. A `nil` binding in any one of `maps` overrides any binding in `parent`, but it does not override any non-`nil` binding in any other of the `maps`.

For example, here is how Emacs sets the parent of `help-mode-map`, such that it inherits from both `button-buffer-map` and `special-mode-map`:

    (defvar help-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map
          (make-composed-keymap button-buffer-map special-mode-map))
        ... map) ... )

Next: [Prefix Keys](Prefix-Keys.html), Previous: [Creating Keymaps](Creating-Keymaps.html), Up: [Keymaps](Keymaps.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
