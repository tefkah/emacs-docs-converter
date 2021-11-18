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

Next: [Overlays](Overlays.html), Previous: [Selective Display](Selective-Display.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 39.8 Temporary Displays

Temporary displays are used by Lisp programs to put output into a buffer and then present it to the user for perusal rather than for editing. Many help commands use this feature.

*   Macro: **with-output-to-temp-buffer** *buffer-name body…*

    This function executes the forms in `body` while arranging to insert any output they print into the buffer named `buffer-name`, which is first created if necessary, and put into Help mode. (See the similar form `with-temp-buffer-window` below.) Finally, the buffer is displayed in some window, but that window is not selected.

    If the forms in `body` do not change the major mode in the output buffer, so that it is still Help mode at the end of their execution, then `with-output-to-temp-buffer` makes this buffer read-only at the end, and also scans it for function and variable names to make them into clickable cross-references. See [Tips for Documentation Strings](Documentation-Tips.html#Docstring-hyperlinks), in particular the item on hyperlinks in documentation strings, for more details.

    The string `buffer-name` specifies the temporary buffer, which need not already exist. The argument must be a string, not a buffer. The buffer is erased initially (with no questions asked), and it is marked as unmodified after `with-output-to-temp-buffer` exits.

    `with-output-to-temp-buffer` binds `standard-output` to the temporary buffer, then it evaluates the forms in `body`. Output using the Lisp output functions within `body` goes by default to that buffer (but screen display and messages in the echo area, although they are “output” in the general sense of the word, are not affected). See [Output Functions](Output-Functions.html).

    Several hooks are available for customizing the behavior of this construct; they are listed below.

    The value of the last form in `body` is returned.

        ---------- Buffer: foo ----------
         This is the contents of foo.
        ---------- Buffer: foo ----------

    ```
    ```

        (with-output-to-temp-buffer "foo"
            (print 20)
            (print standard-output))
        ⇒ #<buffer foo>

        ---------- Buffer: foo ----------

        20

        #<buffer foo>

        ---------- Buffer: foo ----------

<!---->

*   User Option: **temp-buffer-show-function**

    If this variable is non-`nil`, `with-output-to-temp-buffer` calls it as a function to do the job of displaying a help buffer. The function gets one argument, which is the buffer it should display.

    It is a good idea for this function to run `temp-buffer-show-hook` just as `with-output-to-temp-buffer` normally would, inside of `save-selected-window` and with the chosen window and buffer selected.

<!---->

*   Variable: **temp-buffer-setup-hook**

    This normal hook is run by `with-output-to-temp-buffer` before evaluating `body`. When the hook runs, the temporary buffer is current. This hook is normally set up with a function to put the buffer in Help mode.

<!---->

*   Variable: **temp-buffer-show-hook**

    This normal hook is run by `with-output-to-temp-buffer` after displaying the temporary buffer. When the hook runs, the temporary buffer is current, and the window it was displayed in is selected.

<!---->

*   Macro: **with-temp-buffer-window** *buffer-or-name action quit-function body…*

    This macro is similar to `with-output-to-temp-buffer`. Like that construct, it executes `body` while arranging to insert any output it prints into the buffer named `buffer-or-name` and displays that buffer in some window. Unlike `with-output-to-temp-buffer`, however, it does not automatically switch that buffer to Help mode.

    The argument `buffer-or-name` specifies the temporary buffer. It can be either a buffer, which must already exist, or a string, in which case a buffer of that name is created, if necessary. The buffer is marked as unmodified and read-only when `with-temp-buffer-window` exits.

    This macro does not call `temp-buffer-show-function`. Rather, it passes the `action` argument to `display-buffer` (see [Choosing Window](Choosing-Window.html)) in order to display the buffer.

    The value of the last form in `body` is returned, unless the argument `quit-function` is specified. In that case, it is called with two arguments: the window showing the buffer and the result of `body`. The final return value is then whatever `quit-function` returns.

    This macro uses the normal hooks `temp-buffer-window-setup-hook` and `temp-buffer-window-show-hook` in place of the analogous hooks run by `with-output-to-temp-buffer`.

The two constructs described next are mostly identical to `with-temp-buffer-window` but differ from it as specified:

*   Macro: **with-current-buffer-window** *buffer-or-name action quit-function \&rest body*

    This macro is like `with-temp-buffer-window` but unlike that makes the buffer specified by `buffer-or-name` current for running `body`.

<!---->

*   Macro: **with-displayed-buffer-window** *buffer-or-name action quit-function \&rest body*

    This macro is like `with-current-buffer-window` but unlike that displays the buffer specified by `buffer-or-name` *before* running `body`.

A window showing a temporary buffer can be fitted to the size of that buffer using the following mode:

*   User Option: **temp-buffer-resize-mode**

    When this minor mode is enabled, windows showing a temporary buffer are automatically resized to fit their buffer’s contents.

    A window is resized if and only if it has been specially created for the buffer. In particular, windows that have shown another buffer before are not resized. By default, this mode uses `fit-window-to-buffer` (see [Resizing Windows](Resizing-Windows.html)) for resizing. You can specify a different function by customizing the options `temp-buffer-max-height` and `temp-buffer-max-width` below.

<!---->

*   User Option: **temp-buffer-max-height**

    This option specifies the maximum height (in lines) of a window displaying a temporary buffer when `temp-buffer-resize-mode` is enabled. It can also be a function to be called to choose the height for such a buffer. It gets one argument, the buffer, and should return a positive integer. At the time the function is called, the window to be resized is selected.

<!---->

*   User Option: **temp-buffer-max-width**

    This option specifies the maximum width of a window (in columns) displaying a temporary buffer when `temp-buffer-resize-mode` is enabled. It can also be a function to be called to choose the width for such a buffer. It gets one argument, the buffer, and should return a positive integer. At the time the function is called, the window to be resized is selected.

The following function uses the current buffer for temporary display:

*   Function: **momentary-string-display** *string position \&optional char message*

    This function momentarily displays `string` in the current buffer at `position`. It has no effect on the undo list or on the buffer’s modification status.

    The momentary display remains until the next input event. If the next input event is `char`, `momentary-string-display` ignores it and returns. Otherwise, that event remains buffered for subsequent use as input. Thus, typing `char` will simply remove the string from the display, while typing (say) `C-f` will remove the string from the display and later (presumably) move point forward. The argument `char` is a space by default.

    The return value of `momentary-string-display` is not meaningful.

    If the string `string` does not contain control characters, you can do the same job in a more general way by creating (and then subsequently deleting) an overlay with a `before-string` property. See [Overlay Properties](Overlay-Properties.html).

    If `message` is non-`nil`, it is displayed in the echo area while `string` is displayed in the buffer. If it is `nil`, a default message says to type `char` to continue.

    In this example, point is initially located at the beginning of the second line:

        ---------- Buffer: foo ----------
        This is the contents of foo.
        ∗Second line.
        ---------- Buffer: foo ----------

    ```
    ```

        (momentary-string-display
          "**** Important Message! ****"
          (point) ?\r
          "Type RET when done reading")
        ⇒ t

    ```
    ```

        ---------- Buffer: foo ----------
        This is the contents of foo.
        **** Important Message! ****Second line.
        ---------- Buffer: foo ----------

        ---------- Echo Area ----------
        Type RET when done reading
        ---------- Echo Area ----------

Next: [Overlays](Overlays.html), Previous: [Selective Display](Selective-Display.html), Up: [Display](Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
