

Next: [Minor Modes](Minor-Modes.html), Previous: [Hooks](Hooks.html), Up: [Modes](Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 23.2 Major Modes

Major modes specialize Emacs for editing or interacting with particular kinds of text. Each buffer has exactly one major mode at a time. Every major mode is associated with a *major mode command*, whose name should end in ‘`-mode`’. This command takes care of switching to that mode in the current buffer, by setting various buffer-local variables such as a local keymap. See [Major Mode Conventions](Major-Mode-Conventions.html). Note that unlike minor modes there is no way to “turn off” a major mode, instead the buffer must be switched to a different one. However, you can temporarily *suspend* a major mode and later *restore* the suspended mode, see below.

The least specialized major mode is called *Fundamental mode*, which has no mode-specific definitions or variable settings.

*   Command: **fundamental-mode**

    This is the major mode command for Fundamental mode. Unlike other mode commands, it does *not* run any mode hooks (see [Major Mode Conventions](Major-Mode-Conventions.html)), since you are not supposed to customize this mode.

<!---->

*   Function: **major-mode-suspend**

    This function works like `fundamental-mode`, in that it kills all buffer-local variables, but it also records the major mode in effect, so that it could subsequently be restored. This function and `major-mode-restore` (described next) are useful when you need to put a buffer under some specialized mode other than the one Emacs chooses for it automatically (see [Auto Major Mode](Auto-Major-Mode.html)), but would also like to be able to switch back to the original mode later.

<!---->

*   Function: **major-mode-restore** *\&optional avoided-modes*

    This function restores the major mode recorded by `major-mode-suspend`. If no major mode was recorded, this function calls `normal-mode` (see [normal-mode](Auto-Major-Mode.html)), but tries to force it not to choose any modes in `avoided-modes`, if that argument is non-`nil`.

The easiest way to write a major mode is to use the macro `define-derived-mode`, which sets up the new mode as a variant of an existing major mode. See [Derived Modes](Derived-Modes.html). We recommend using `define-derived-mode` even if the new mode is not an obvious derivative of another mode, as it automatically enforces many coding conventions for you. See [Basic Major Modes](Basic-Major-Modes.html), for common modes to derive from.

The standard GNU Emacs Lisp directory tree contains the code for several major modes, in files such as `text-mode.el`, `texinfo.el`, `lisp-mode.el`, and `rmail.el`. You can study these libraries to see how modes are written.

*   User Option: **major-mode**

    The buffer-local value of this variable holds the symbol for the current major mode. Its default value holds the default major mode for new buffers. The standard default value is `fundamental-mode`.

    If the default value is `nil`, then whenever Emacs creates a new buffer via a command such as `C-x b` (`switch-to-buffer`), the new buffer is put in the major mode of the previously current buffer. As an exception, if the major mode of the previous buffer has a `mode-class` symbol property with value `special`, the new buffer is put in Fundamental mode (see [Major Mode Conventions](Major-Mode-Conventions.html)).

|                                                         |    |                                                                               |
| :------------------------------------------------------ | -- | :---------------------------------------------------------------------------- |
| • [Major Mode Conventions](Major-Mode-Conventions.html) |    | Coding conventions for keymaps, etc.                                          |
| • [Auto Major Mode](Auto-Major-Mode.html)               |    | How Emacs chooses the major mode automatically.                               |
| • [Mode Help](Mode-Help.html)                           |    | Finding out how to use a mode.                                                |
| • [Derived Modes](Derived-Modes.html)                   |    | Defining a new major mode based on another major mode.                        |
| • [Basic Major Modes](Basic-Major-Modes.html)           |    | Modes that other modes are often derived from.                                |
| • [Mode Hooks](Mode-Hooks.html)                         |    | Hooks run at the end of major mode functions.                                 |
| • [Tabulated List Mode](Tabulated-List-Mode.html)       |    | Parent mode for buffers containing tabulated data.                            |
| • [Generic Modes](Generic-Modes.html)                   |    | Defining a simple major mode that supports comment syntax and Font Lock mode. |
| • [Example Major Modes](Example-Major-Modes.html)       |    | Text mode and Lisp modes.                                                     |

Next: [Minor Modes](Minor-Modes.html), Previous: [Hooks](Hooks.html), Up: [Modes](Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
