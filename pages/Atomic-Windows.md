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

Next: [Window Point](Window-Point.html), Previous: [Side Windows](Side-Windows.html), Up: [Windows](Windows.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 28.18 Atomic Windows

Atomic windows are rectangular compositions of at least two live windows. They have the following distinctive characteristics:

*   The function `split-window` (see [Splitting Windows](Splitting-Windows.html)), when applied to a constituent of an atomic window, will try to create the new window outside of the atomic window.
*   The function `delete-window` (see [Deleting Windows](Deleting-Windows.html)), when applied to a constituent of an atomic window, will try to delete the entire atomic window instead.
*   The function `delete-other-windows` (see [Deleting Windows](Deleting-Windows.html)), when applied to a constituent of an atomic window, will try to make the atomic window fill its frame or main window (see [Side Windows](Side-Windows.html)).

This means that the basic groups of functions that alter the window structure treat an atomic window like a live one, thus preserving the internal structure of the atomic window.

Atomic windows are useful to construct and preserve window layouts that are meaningful only when all involved buffers are shown simultaneously in a specific manner, such as when showing differences between file revisions, or the same text in different languages or markups. They can also be used to permanently display information pertinent to a specific window in bars on that window’s sides.

Atomic windows are implemented with the help of the reserved `window-atom` window parameter (see [Window Parameters](Window-Parameters.html)) and an internal window (see [Basic Windows](Basic-Windows.html)) called the root window of the atomic window. All windows that are part of the same atomic window have this root window as their common ancestor and are assigned a non-`nil` `window-atom` parameter.

The following function returns the root of the atomic window a specified window is part of:

*   Function: **window-atom-root** *\&optional window*

    This functions returns the root of the atomic window `window` is a part of. The specified `window` must be a valid window and defaults to the selected one. It returns `nil` if `window` is not part of an atomic window.

The most simple approach to make a new atomic window is to take an existing internal window and apply the following function:

*   Function: **window-make-atom** *window*

    This function converts `window` into an atomic window. The specified `window` must be an internal window. All this function does is to set the `window-atom` parameter of each descendant of `window` to `t`.

To create a new atomic window from an existing live window or to add a new window to an existing atomic window, the following buffer display action function (see [Buffer Display Action Functions](Buffer-Display-Action-Functions.html)) can be used:

*   Function: **display-buffer-in-atom-window** *buffer alist*

    This function tries to display `buffer` in a new window that will be combined with an existing window to form an atomic window. If the existing window is already part of an atomic window, it adds the new window to that atomic window.

    The specified `alist` is an association list of symbols and values. The following symbols have a special meaning:

    *   `window`

        The value of such an element specifies an existing window the new window shall be combined with. If it specifies an internal window, all children of that window become part of the atomic window too. If no window is specified, the new window becomes a sibling of the selected window. The `window-atom` parameter of the existing window is set to `main` provided that window is live and its `window-atom` parameter was not already set.

    *   `side`

        The value of such an element denotes the side of the existing window where the new window shall be located. Valid values are `below`, `right`, `above` and `left`. The default is `below`. The `window-atom` parameter of the new window is set to this value.

    The return value is the new window, `nil` when creating that window failed.

Note that the value of the `window-atom` parameter does not really matter as long as it is non-`nil`. The values assigned by `display-buffer-in-atom-window` just allow for easy retrieval of the original and the new window after that function has been applied. Note also that the `window-atom` parameter is the only window parameter assigned by `display-buffer-in-atom-window`. Further parameters have to be set by the application explicitly via a `window-parameters` entry in `alist`.

Atomic windows automatically cease to exist when one of their constituents gets deleted. To dissolve an atomic window manually, reset the `window-atom` parameter of its constituents—the root of the atomic window and all its descendants.

The following code snippet, when applied to a single-window frame, first splits the selected window and makes the selected and the new window constituents of an atomic window with their parent as root. It then displays the buffer `*Messages*` in a new window at the frame’s bottom and makes that new window part of the atomic window just created.

    (let ((window (split-window-right)))
      (window-make-atom (window-parent window))
      (display-buffer-in-atom-window
       (get-buffer-create "*Messages*")
       `((window . ,(window-parent window)) (window-height . 5))))

At this moment typing `C-x 2`<!-- /@w --> in any window of that frame produces a new window at the bottom of the frame. Typing `C-x 3`<!-- /@w --> instead will put the new window at the frame’s right. In either case, typing now `C-x 1`<!-- /@w --> in any window of the atomic window will remove the new window only. Typing `C-x 0`<!-- /@w --> in any window of the atomic window will make that new window fill the frame.

Next: [Window Point](Window-Point.html), Previous: [Side Windows](Side-Windows.html), Up: [Windows](Windows.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
