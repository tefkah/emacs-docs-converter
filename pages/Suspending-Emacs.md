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

Previous: [Killing Emacs](Killing-Emacs.html), Up: [Getting Out](Getting-Out.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 40.2.2 Suspending Emacs

On text terminals, it is possible to *suspend Emacs*, which means stopping Emacs temporarily and returning control to its superior process, which is usually the shell. This allows you to resume editing later in the same Emacs process, with the same buffers, the same kill ring, the same undo history, and so on. To resume Emacs, use the appropriate command in the parent shell—most likely `fg`.

Suspending works only on a terminal device from which the Emacs session was started. We call that device the *controlling terminal* of the session. Suspending is not allowed if the controlling terminal is a graphical terminal. Suspending is usually not relevant in graphical environments, since you can simply switch to another application without doing anything special to Emacs.

Some operating systems (those without `SIGTSTP`, or MS-DOS) do not support suspension of jobs; on these systems, suspension actually creates a new shell temporarily as a subprocess of Emacs. Then you would exit the shell to return to Emacs.

*   Command: **suspend-emacs** *\&optional string*

    This function stops Emacs and returns control to the superior process. If and when the superior process resumes Emacs, `suspend-emacs` returns `nil` to its caller in Lisp.

    This function works only on the controlling terminal of the Emacs session; to relinquish control of other tty devices, use `suspend-tty` (see below). If the Emacs session uses more than one terminal, you must delete the frames on all the other terminals before suspending Emacs, or this function signals an error. See [Multiple Terminals](Multiple-Terminals.html).

    If `string` is non-`nil`, its characters are sent to Emacs’s superior shell, to be read as terminal input. The characters in `string` are not echoed by the superior shell; only the results appear.

    Before suspending, `suspend-emacs` runs the normal hook `suspend-hook`. After the user resumes Emacs, `suspend-emacs` runs the normal hook `suspend-resume-hook`. See [Hooks](Hooks.html).

    The next redisplay after resumption will redraw the entire screen, unless the variable `no-redraw-on-reenter` is non-`nil`. See [Refresh Screen](Refresh-Screen.html).

    Here is an example of how you could use these hooks:

        (add-hook 'suspend-hook
                  (lambda () (or (y-or-n-p "Really suspend? ")
                                 (error "Suspend canceled"))))

    <!---->

        (add-hook 'suspend-resume-hook (lambda () (message "Resumed!")
                                         (sit-for 2)))

    Here is what you would see upon evaluating `(suspend-emacs "pwd")`:

        ---------- Buffer: Minibuffer ----------
        Really suspend? y
        ---------- Buffer: Minibuffer ----------

    ```
    ```

        ---------- Parent Shell ----------
        bash$ /home/username
        bash$ fg

    ```
    ```

        ---------- Echo Area ----------
        Resumed!

    Note that ‘`pwd`’ is not echoed after Emacs is suspended. But it is read and executed by the shell.

<!---->

*   Variable: **suspend-hook**

    This variable is a normal hook that Emacs runs before suspending.

<!---->

*   Variable: **suspend-resume-hook**

    This variable is a normal hook that Emacs runs on resuming after a suspension.

<!---->

*   Function: **suspend-tty** *\&optional tty*

    If `tty` specifies a terminal device used by Emacs, this function relinquishes the device and restores it to its prior state. Frames that used the device continue to exist, but are not updated and Emacs doesn’t read input from them. `tty` can be a terminal object, a frame (meaning the terminal for that frame), or `nil` (meaning the terminal for the selected frame). See [Multiple Terminals](Multiple-Terminals.html).

    If `tty` is already suspended, this function does nothing.

    This function runs the hook `suspend-tty-functions`, passing the terminal object as an argument to each function.

<!---->

*   Function: **resume-tty** *\&optional tty*

    This function resumes the previously suspended terminal device `tty`; where `tty` has the same possible values as it does for `suspend-tty`.

    This function reopens the terminal device, re-initializes it, and redraws it with that terminal’s selected frame. It then runs the hook `resume-tty-functions`, passing the terminal object as an argument to each function.

    If the same device is already used by another Emacs terminal, this function signals an error. If `tty` is not suspended, this function does nothing.

<!---->

*   Function: **controlling-tty-p** *\&optional tty*

    This function returns non-`nil` if `tty` is the controlling terminal of the Emacs session; `tty` can be a terminal object, a frame (meaning the terminal for that frame), or `nil` (meaning the terminal for the selected frame).

<!---->

*   Command: **suspend-frame**

    This command *suspends* a frame. For GUI frames, it calls `iconify-frame` (see [Visibility of Frames](Visibility-of-Frames.html)); for frames on text terminals, it calls either `suspend-emacs` or `suspend-tty`, depending on whether the frame is displayed on the controlling terminal device or not.

Previous: [Killing Emacs](Killing-Emacs.html), Up: [Getting Out](Getting-Out.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
