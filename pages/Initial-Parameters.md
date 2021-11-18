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

Next: [Window Frame Parameters](Window-Frame-Parameters.html), Previous: [Parameter Access](Parameter-Access.html), Up: [Frame Parameters](Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.4.2 Initial Frame Parameters

You can specify the parameters for the initial startup frame by setting `initial-frame-alist` in your init file (see [Init File](Init-File.html)).

*   User Option: **initial-frame-alist**

    This variable’s value is an alist of parameter values used when creating the initial frame. You can set this variable to specify the appearance of the initial frame without altering subsequent frames. Each element has the form:

        (parameter . value)

    Emacs creates the initial frame before it reads your init file. After reading that file, Emacs checks `initial-frame-alist`, and applies the parameter settings in the altered value to the already created initial frame.

    If these settings affect the frame geometry and appearance, you’ll see the frame appear with the wrong ones and then change to the specified ones. If that bothers you, you can specify the same geometry and appearance with X resources; those do take effect before the frame is created. See [X Resources](https://www.gnu.org/software/emacs/manual/html_node/emacs/X-Resources.html#X-Resources) in The GNU Emacs Manual.

    X resource settings typically apply to all frames. If you want to specify some X resources solely for the sake of the initial frame, and you don’t want them to apply to subsequent frames, here’s how to achieve this. Specify parameters in `default-frame-alist` to override the X resources for subsequent frames; then, to prevent these from affecting the initial frame, specify the same parameters in `initial-frame-alist` with values that match the X resources.

If these parameters include `(minibuffer . nil)`, that indicates that the initial frame should have no minibuffer. In this case, Emacs creates a separate *minibuffer-only frame* as well.

*   User Option: **minibuffer-frame-alist**

    This variable’s value is an alist of parameter values used when creating an initial minibuffer-only frame (i.e., the minibuffer-only frame that Emacs creates if `initial-frame-alist` specifies a frame with no minibuffer).

<!---->

*   User Option: **default-frame-alist**

    This is an alist specifying default values of frame parameters for all Emacs frames—the first frame, and subsequent frames. When using the X Window System, you can get the same results by means of X resources in many cases.

    Setting this variable does not affect existing frames. Furthermore, functions that display a buffer in a separate frame may override the default parameters by supplying their own parameters.

If you invoke Emacs with command-line options that specify frame appearance, those options take effect by adding elements to either `initial-frame-alist` or `default-frame-alist`. Options which affect just the initial frame, such as ‘`--geometry`’ and ‘`--maximized`’, add to `initial-frame-alist`; the others add to `default-frame-alist`. see [Command Line Arguments for Emacs Invocation](https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Invocation.html#Emacs-Invocation) in The GNU Emacs Manual.

Next: [Window Frame Parameters](Window-Frame-Parameters.html), Previous: [Parameter Access](Parameter-Access.html), Up: [Frame Parameters](Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]