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

Next: [Font Lock Mode](Font-Lock-Mode.html), Previous: [Mode Line Format](Mode-Line-Format.html), Up: [Modes](Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 23.5 Imenu

*Imenu* is a feature that lets users select a definition or section in the buffer, from a menu which lists all of them, to go directly to that location in the buffer. Imenu works by constructing a buffer index which lists the names and buffer positions of the definitions, or other named portions of the buffer; then the user can choose one of them and move point to it. Major modes can add a menu bar item to use Imenu using `imenu-add-to-menubar`.

*   Command: **imenu-add-to-menubar** *name*

    This function defines a local menu bar item named `name` to run Imenu.

The user-level commands for using Imenu are described in the Emacs Manual (see [Imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html#Imenu) in the Emacs Manual). This section explains how to customize Imenu’s method of finding definitions or buffer portions for a particular major mode.

The usual and simplest way is to set the variable `imenu-generic-expression`:

*   Variable: **imenu-generic-expression**

    This variable, if non-`nil`, is a list that specifies regular expressions for finding definitions for Imenu. Simple elements of `imenu-generic-expression` look like this:

        (menu-title regexp index)

    Here, if `menu-title` is non-`nil`, it says that the matches for this element should go in a submenu of the buffer index; `menu-title` itself specifies the name for the submenu. If `menu-title` is `nil`, the matches for this element go directly in the top level of the buffer index.

    The second item in the list, `regexp`, is a regular expression (see [Regular Expressions](Regular-Expressions.html)); anything in the buffer that it matches is considered a definition, something to mention in the buffer index. The third item, `index`, is a non-negative integer that indicates which subexpression in `regexp` matches the definition’s name.

    An element can also look like this:

        (menu-title regexp index function arguments…)

    Each match for this element creates an index item, and when the index item is selected by the user, it calls `function` with arguments consisting of the item name, the buffer position, and `arguments`.

    For Emacs Lisp mode, `imenu-generic-expression` could look like this:

        ((nil "^\\s-*(def\\(un\\|subst\\|macro\\|advice\\)\
        \\s-+\\([-A-Za-z0-9+]+\\)" 2)

    <!---->

         ("*Vars*" "^\\s-*(def\\(var\\|const\\)\
        \\s-+\\([-A-Za-z0-9+]+\\)" 2)

    <!---->

         ("*Types*"
          "^\\s-*\
        (def\\(type\\|struct\\|class\\|ine-condition\\)\
        \\s-+\\([-A-Za-z0-9+]+\\)" 2))

    Setting this variable makes it buffer-local in the current buffer.

<!---->

*   Variable: **imenu-case-fold-search**

    This variable controls whether matching against the regular expressions in the value of `imenu-generic-expression` is case-sensitive: `t`, the default, means matching should ignore case.

    Setting this variable makes it buffer-local in the current buffer.

<!---->

*   Variable: **imenu-syntax-alist**

    This variable is an alist of syntax table modifiers to use while processing `imenu-generic-expression`, to override the syntax table of the current buffer. Each element should have this form:

        (characters . syntax-description)

    The CAR, `characters`, can be either a character or a string. The element says to give that character or characters the syntax specified by `syntax-description`, which is passed to `modify-syntax-entry` (see [Syntax Table Functions](Syntax-Table-Functions.html)).

    This feature is typically used to give word syntax to characters which normally have symbol syntax, and thus to simplify `imenu-generic-expression` and speed up matching. For example, Fortran mode uses it this way:

        (setq imenu-syntax-alist '(("_$" . "w")))

    The `imenu-generic-expression` regular expressions can then use ‘`\\sw+`’ instead of ‘`\\(\\sw\\|\\s_\\)+`’. Note that this technique may be inconvenient when the mode needs to limit the initial character of a name to a smaller set of characters than are allowed in the rest of a name.

    Setting this variable makes it buffer-local in the current buffer.

Another way to customize Imenu for a major mode is to set the variables `imenu-prev-index-position-function` and `imenu-extract-index-name-function`:

*   Variable: **imenu-prev-index-position-function**

    If this variable is non-`nil`, its value should be a function that finds the next definition to put in the buffer index, scanning backward in the buffer from point. It should return `nil` if it doesn’t find another definition before point. Otherwise it should leave point at the place it finds a definition and return any non-`nil` value.

    Setting this variable makes it buffer-local in the current buffer.

<!---->

*   Variable: **imenu-extract-index-name-function**

    If this variable is non-`nil`, its value should be a function to return the name for a definition, assuming point is in that definition as the `imenu-prev-index-position-function` function would leave it.

    Setting this variable makes it buffer-local in the current buffer.

The last way to customize Imenu for a major mode is to set the variable `imenu-create-index-function`:

*   Variable: **imenu-create-index-function**

    This variable specifies the function to use for creating a buffer index. The function should take no arguments, and return an index alist for the current buffer. It is called within `save-excursion`, so where it leaves point makes no difference.

    The index alist can have three types of elements. Simple elements look like this:

        (index-name . index-position)

    Selecting a simple element has the effect of moving to position `index-position` in the buffer. Special elements look like this:

        (index-name index-position function arguments…)

    Selecting a special element performs:

        (funcall function
                 index-name index-position arguments…)

    A nested sub-alist element looks like this:

        (menu-title . sub-alist)

    It creates the submenu `menu-title` specified by `sub-alist`.

    The default value of `imenu-create-index-function` is `imenu-default-create-index-function`. This function calls the value of `imenu-prev-index-position-function` and the value of `imenu-extract-index-name-function` to produce the index alist. However, if either of these two variables is `nil`, the default function uses `imenu-generic-expression` instead.

    Setting this variable makes it buffer-local in the current buffer.

Next: [Font Lock Mode](Font-Lock-Mode.html), Previous: [Mode Line Format](Mode-Line-Format.html), Up: [Modes](Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
