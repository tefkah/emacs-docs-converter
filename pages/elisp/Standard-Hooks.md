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

Next: [Index](Index.html), Previous: [Standard Keymaps](Standard-Keymaps.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## Appendix H Standard Hooks

The following is a list of some hook variables that let you provide functions to be called from within Emacs on suitable occasions.

Most of these variables have names ending with ‘`-hook`’. They are *normal hooks*, run by means of `run-hooks`. The value of such a hook is a list of functions; the functions are called with no arguments and their values are completely ignored. The recommended way to put a new function on such a hook is to call `add-hook`. See [Hooks](Hooks.html), for more information about using hooks.

The variables whose names end in ‘`-functions`’ are usually *abnormal hooks* (some old code may also use the deprecated ‘`-hooks`’ suffix); their values are lists of functions, but these functions are called in a special way (they are passed arguments, or their return values are used). The variables whose names end in ‘`-function`’ have single functions as their values.

This is not an exhaustive list, it only covers the more general hooks. For example, every major mode defines a hook named ‘`modename-mode-hook`’. The major mode command runs this normal hook with `run-mode-hooks` as the very last thing it does. See [Mode Hooks](Mode-Hooks.html). Most minor modes have mode hooks too.

A special feature allows you to specify expressions to evaluate if and when a file is loaded (see [Hooks for Loading](Hooks-for-Loading.html)). That feature is not exactly a hook, but does a similar job.

*   *   `activate-mark-hook`
    *   `deactivate-mark-hook`

    See [The Mark](The-Mark.html).

*   *   `after-change-functions`
    *   `before-change-functions`
    *   `first-change-hook`

    See [Change Hooks](Change-Hooks.html).

*   *   `after-change-major-mode-hook`
    *   `change-major-mode-after-body-hook`

    See [Mode Hooks](Mode-Hooks.html).

*   *   `after-init-hook`
    *   `before-init-hook`
    *   `emacs-startup-hook`
    *   `window-setup-hook`

    See [Init File](Init-File.html).

*   *   `after-insert-file-functions`
    *   `write-region-annotate-functions`
    *   `write-region-post-annotation-function`

    See [Format Conversion](Format-Conversion.html).

*   *   `after-make-frame-functions`
    *   `before-make-frame-hook`
    *   `server-after-make-frame-hook`

    See [Creating Frames](Creating-Frames.html).

*   *   `after-save-hook`
    *   `before-save-hook`
    *   `write-contents-functions`
    *   `write-file-functions`

    See [Saving Buffers](Saving-Buffers.html).

*   `after-setting-font-hook`

    Hook run after a frame’s font changes.

*   `auto-save-hook`

    See [Auto-Saving](Auto_002dSaving.html).

*   *   `before-hack-local-variables-hook`
    *   `hack-local-variables-hook`

    See [File Local Variables](File-Local-Variables.html).

*   `buffer-access-fontify-functions`

    See [Lazy Properties](Lazy-Properties.html).

*   `buffer-list-update-hook`

    Hook run when the buffer list changes (see [Buffer List](Buffer-List.html)).

*   `buffer-quit-function`

    Function to call to quit the current buffer.

*   `change-major-mode-hook`

    See [Creating Buffer-Local](Creating-Buffer_002dLocal.html).

*   `comint-password-function`

    This abnormal hook permits a derived mode to supply a password for the underlying command interpreter without prompting the user.

*   `command-line-functions`

    See [Command-Line Arguments](Command_002dLine-Arguments.html).

*   `delayed-warnings-hook`

    The command loop runs this soon after `post-command-hook` (q.v.).

*   `focus-in-hook`

*   `focus-out-hook`

    See [Input Focus](Input-Focus.html).

*   *   `delete-frame-functions`
    *   `after-delete-frame-functions`

    See [Deleting Frames](Deleting-Frames.html).

*   `delete-terminal-functions`

    See [Multiple Terminals](Multiple-Terminals.html).

*   *   `pop-up-frame-function`
    *   `split-window-preferred-function`

    See [Choosing Window Options](Choosing-Window-Options.html).

*   `echo-area-clear-hook`

    See [Echo Area Customization](Echo-Area-Customization.html).

*   *   `find-file-hook`
    *   `find-file-not-found-functions`

    See [Visiting Functions](Visiting-Functions.html).

*   `font-lock-extend-after-change-region-function`

    See [Region to Refontify](Region-to-Refontify.html).

*   `font-lock-extend-region-functions`

    See [Multiline Font Lock](Multiline-Font-Lock.html).

*   *   `font-lock-fontify-buffer-function`
    *   `font-lock-fontify-region-function`
    *   `font-lock-mark-block-function`
    *   `font-lock-unfontify-buffer-function`
    *   `font-lock-unfontify-region-function`

    See [Other Font Lock Variables](Other-Font-Lock-Variables.html).

*   `fontification-functions`

    See [Automatic Face Assignment](Auto-Faces.html).

*   `frame-auto-hide-function`

    See [Quitting Windows](Quitting-Windows.html).

*   `quit-window-hook`

    See [Quitting Windows](Quitting-Windows.html).

*   *   `kill-buffer-hook`
    *   `kill-buffer-query-functions`

    See [Killing Buffers](Killing-Buffers.html).

*   *   `kill-emacs-hook`
    *   `kill-emacs-query-functions`

    See [Killing Emacs](Killing-Emacs.html).

*   `menu-bar-update-hook`

    See [Menu Bar](Menu-Bar.html).

*   *   `minibuffer-setup-hook`
    *   `minibuffer-exit-hook`

    See [Minibuffer Misc](Minibuffer-Misc.html).

*   `mouse-leave-buffer-hook`

    Hook run when about to switch windows with a mouse command.

*   `mouse-position-function`

    See [Mouse Position](Mouse-Position.html).

*   `prefix-command-echo-keystrokes-functions`

    An abnormal hook run by prefix commands (such as `C-u`) which should return a string describing the current prefix state. For example, `C-u` produces ‘`C-u-`’ and ‘`C-u 1 2 3-`’. Each hook function is called with no arguments and should return a string describing the current prefix state, or `nil` if there’s no prefix state. See [Prefix Command Arguments](Prefix-Command-Arguments.html).

*   `prefix-command-preserve-state-hook`

    Hook run when a prefix command needs to preserve the prefix by passing the current prefix command state to the next command. For example, `C-u` needs to pass the state to the next command when the user types `C-u -` or follows `C-u` with a digit.

*   `pre-redisplay-functions`

    Hook run in each window just before redisplaying it. See [Forcing Redisplay](Forcing-Redisplay.html).

*   *   `post-command-hook`
    *   `pre-command-hook`

    See [Command Overview](Command-Overview.html).

*   `post-gc-hook`

    See [Garbage Collection](Garbage-Collection.html).

*   `post-self-insert-hook`

    See [Keymaps and Minor Modes](Keymaps-and-Minor-Modes.html).

*   *   `suspend-hook`
    *   `suspend-resume-hook`
    *   `suspend-tty-functions`
    *   `resume-tty-functions`

    See [Suspending Emacs](Suspending-Emacs.html).

*   *   `syntax-begin-function`
    *   `syntax-propertize-extend-region-functions`
    *   `syntax-propertize-function`
    *   `font-lock-syntactic-face-function`

    See [Syntactic Font Lock](Syntactic-Font-Lock.html). See [Syntax Properties](Syntax-Properties.html).

*   *   `temp-buffer-setup-hook`
    *   `temp-buffer-show-function`
    *   `temp-buffer-show-hook`

    See [Temporary Displays](Temporary-Displays.html).

*   `tty-setup-hook`

    See [Terminal-Specific](Terminal_002dSpecific.html).

*   *   `window-configuration-change-hook`
    *   `window-scroll-functions`
    *   `window-size-change-functions`

    See [Window Hooks](Window-Hooks.html).

Next: [Index](Index.html), Previous: [Standard Keymaps](Standard-Keymaps.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
