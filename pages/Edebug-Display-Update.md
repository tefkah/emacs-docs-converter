

Next: [Edebug Recursive Edit](Edebug-Recursive-Edit.html), Previous: [Checking Whether to Stop](Checking-Whether-to-Stop.html), Up: [The Outside Context](The-Outside-Context.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 18.2.14.2 Edebug Display Update

When Edebug needs to display something (e.g., in trace mode), it saves the current window configuration from outside Edebug (see [Window Configurations](Window-Configurations.html)). When you exit Edebug, it restores the previous window configuration.

Emacs redisplays only when it pauses. Usually, when you continue execution, the program re-enters Edebug at a breakpoint or after stepping, without pausing or reading input in between. In such cases, Emacs never gets a chance to redisplay the outside configuration. Consequently, what you see is the same window configuration as the last time Edebug was active, with no interruption.

Entry to Edebug for displaying something also saves and restores the following data (though some of them are deliberately not restored if an error or quit signal occurs).

*   Which buffer is current, and the positions of point and the mark in the current buffer, are saved and restored.

*   The outside window configuration is saved and restored if `edebug-save-windows` is non-`nil` (see [Edebug Options](Edebug-Options.html)).

    The window configuration is not restored on error or quit, but the outside selected window *is* reselected even on error or quit in case a `save-excursion` is active. If the value of `edebug-save-windows` is a list, only the listed windows are saved and restored.

    The window start and horizontal scrolling of the source code buffer are not restored, however, so that the display remains coherent within Edebug.

*   The value of point in each displayed buffer is saved and restored if `edebug-save-displayed-buffer-points` is non-`nil`.

*   The variables `overlay-arrow-position` and `overlay-arrow-string` are saved and restored, so you can safely invoke Edebug from the recursive edit elsewhere in the same buffer.

*   `cursor-in-echo-area` is locally bound to `nil` so that the cursor shows up in the window.

Next: [Edebug Recursive Edit](Edebug-Recursive-Edit.html), Previous: [Checking Whether to Stop](Checking-Whether-to-Stop.html), Up: [The Outside Context](The-Outside-Context.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
