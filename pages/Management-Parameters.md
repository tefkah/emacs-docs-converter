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

Next: [Cursor Parameters](Cursor-Parameters.html), Previous: [Mouse Dragging Parameters](Mouse-Dragging-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 29.4.3.8 Window Management Parameters

The following frame parameters control various aspects of the frame’s interaction with the window manager or window system. They have no effect on text terminals.

*   `visibility`

    The state of visibility of the frame. There are three possibilities: `nil` for invisible, `t` for visible, and `icon` for iconified. See [Visibility of Frames](Visibility-of-Frames.html).

*   `auto-raise`

    If non-`nil`, Emacs automatically raises the frame when it is selected. Some window managers do not allow this.

*   `auto-lower`

    If non-`nil`, Emacs automatically lowers the frame when it is deselected. Some window managers do not allow this.

*   `icon-type`

    The type of icon to use for this frame. If the value is a string, that specifies a file containing a bitmap to use; `nil` specifies no icon (in which case the window manager decides what to show); any other non-`nil` value specifies the default Emacs icon.

*   `icon-name`

    The name to use in the icon for this frame, when and if the icon appears. If this is `nil`, the frame’s title is used.

*   `window-id`

    The ID number which the graphical display uses for this frame. Emacs assigns this parameter when the frame is created; changing the parameter has no effect on the actual ID number.

*   `outer-window-id`

    The ID number of the outermost window-system window in which the frame exists. As with `window-id`, changing this parameter has no actual effect.

*   `wait-for-wm`

    If non-`nil`, tell Xt to wait for the window manager to confirm geometry changes. Some window managers, including versions of Fvwm2 and KDE, fail to confirm, so Xt hangs. Set this to `nil` to prevent hanging with those window managers.

*   `sticky`

    If non-`nil`, the frame is visible on all virtual desktops on systems with virtual desktops.

*   `inhibit-double-buffering`

    If non-`nil`, the frame is drawn to the screen without double buffering. Emacs normally attempts to use double buffering, where available, to reduce flicker. Set this property if you experience display bugs or pine for that retro, flicker-y feeling.

*   `skip-taskbar`

    If non-`nil`, this tells the window manager to remove the frame’s icon from the taskbar associated with the frame’s display and inhibit switching to the frame’s window via the combination `Alt-TAB`. On MS-Windows, iconifying such a frame will "roll in" its window-system window at the bottom of the desktop. Some window managers may not honor this parameter.

*   `no-focus-on-map`

    If non-`nil`, this means that the frame does not want to receive input focus when it is mapped (see [Visibility of Frames](Visibility-of-Frames.html)). Some window managers may not honor this parameter.

*   `no-accept-focus`

    If non-`nil`, this means that the frame does not want to receive input focus via explicit mouse clicks or when moving the mouse into it either via `focus-follows-mouse` (see [Input Focus](Input-Focus.html)) or `mouse-autoselect-window` (see [Mouse Window Auto-selection](Mouse-Window-Auto_002dselection.html)). This may have the unwanted side-effect that a user cannot scroll a non-selected frame with the mouse. Some window managers may not honor this parameter.

*   `undecorated`

    If non-`nil`, this frame’s window-system window is drawn without decorations, like the title, minimize/maximize boxes and external borders. This usually means that the window cannot be dragged, resized, iconified, maximized or deleted with the mouse. If `nil`, the frame’s window is usually drawn with all the elements listed above unless their display has been suspended via window manager settings.

    Under X, Emacs uses the Motif window manager hints to turn off decorations. Some window managers may not honor these hints.

    NS builds consider the tool bar to be a decoration, and therefore hide it on an undecorated frame.

*   `override-redirect`

    If non-`nil`, this means that this is an *override redirect* frame—a frame not handled by window managers under X. Override redirect frames have no window manager decorations, can be positioned and resized only via Emacs’ positioning and resizing functions and are usually drawn on top of all other frames. Setting this parameter has no effect on MS-Windows.

*   `ns-appearance`

    Only available on macOS, if set to `dark` draw this frame’s window-system window using the “vibrant dark” theme, otherwise use the system default. The “vibrant dark” theme can be used to set the toolbar and scrollbars to a dark appearance when using an Emacs theme with a dark background.

*   `ns-transparent-titlebar`

    Only available on macOS, if non-`nil`, set the titlebar and toolbar to be transparent. This effectively sets the background color of both to match the Emacs background color.

Next: [Cursor Parameters](Cursor-Parameters.html), Previous: [Mouse Dragging Parameters](Mouse-Dragging-Parameters.html), Up: [Window Frame Parameters](Window-Frame-Parameters.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
