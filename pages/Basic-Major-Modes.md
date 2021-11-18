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

Next: [Mode Hooks](Mode-Hooks.html), Previous: [Derived Modes](Derived-Modes.html), Up: [Major Modes](Major-Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.2.5 Basic Major Modes

Apart from Fundamental mode, there are three major modes that other major modes commonly derive from: Text mode, Prog mode, and Special mode. While Text mode is useful in its own right (e.g., for editing files ending in `.txt`), Prog mode and Special mode exist mainly to let other modes derive from them.

As far as possible, new major modes should be derived, either directly or indirectly, from one of these three modes. One reason is that this allows users to customize a single mode hook (e.g., `prog-mode-hook`) for an entire family of relevant modes (e.g., all programming language modes).

*   Command: **text-mode**

    Text mode is a major mode for editing human languages. It defines the ‘`"`’ and ‘`\`’ characters as having punctuation syntax (see [Syntax Class Table](Syntax-Class-Table.html)), and binds `M-TAB` to `ispell-complete-word` (see [Spelling](https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html#Spelling) in The GNU Emacs Manual).

    An example of a major mode derived from Text mode is HTML mode. See [SGML and HTML Modes](https://www.gnu.org/software/emacs/manual/html_node/emacs/HTML-Mode.html#HTML-Mode) in The GNU Emacs Manual.

<!---->

*   Command: **prog-mode**

    Prog mode is a basic major mode for buffers containing programming language source code. Most of the programming language major modes built into Emacs are derived from it.

    Prog mode binds `parse-sexp-ignore-comments` to `t` (see [Motion via Parsing](Motion-via-Parsing.html)) and `bidi-paragraph-direction` to `left-to-right` (see [Bidirectional Display](Bidirectional-Display.html)).

<!---->

*   Command: **special-mode**

    Special mode is a basic major mode for buffers containing text that is produced specially by Emacs, rather than directly from a file. Major modes derived from Special mode are given a `mode-class` property of `special` (see [Major Mode Conventions](Major-Mode-Conventions.html)).

    Special mode sets the buffer to read-only. Its keymap defines several common bindings, including `q` for `quit-window` and `g` for `revert-buffer` (see [Reverting](Reverting.html)).

    An example of a major mode derived from Special mode is Buffer Menu mode, which is used by the `*Buffer List*` buffer. See [Listing Existing Buffers](https://www.gnu.org/software/emacs/manual/html_node/emacs/List-Buffers.html#List-Buffers) in The GNU Emacs Manual.

In addition, modes for buffers of tabulated data can inherit from Tabulated List mode, which is in turn derived from Special mode. See [Tabulated List Mode](Tabulated-List-Mode.html).

Next: [Mode Hooks](Mode-Hooks.html), Previous: [Derived Modes](Derived-Modes.html), Up: [Major Modes](Major-Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
