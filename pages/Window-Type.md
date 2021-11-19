

Next: [Frame Type](Frame-Type.html), Previous: [Marker Type](Marker-Type.html), Up: [Editing Types](Editing-Types.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 2.5.3 Window Type

A *window* describes the portion of the terminal screen that Emacs uses to display a buffer. Every window has one associated buffer, whose contents appear in the window. By contrast, a given buffer may appear in one window, no window, or several windows.

Though many windows may exist simultaneously, at any time one window is designated the *selected window*. This is the window where the cursor is (usually) displayed when Emacs is ready for a command. The selected window usually displays the current buffer (see [Current Buffer](Current-Buffer.html)), but this is not necessarily the case.

Windows are grouped on the screen into frames; each window belongs to one and only one frame. See [Frame Type](Frame-Type.html).

Windows have no read syntax. They print in hash notation, giving the window number and the name of the buffer being displayed. The window numbers exist to identify windows uniquely, since the buffer displayed in any given window can change frequently.

```lisp
(selected-window)
     ⇒ #<window 1 on objects.texi>
```

See [Windows](Windows.html), for a description of the functions that work on windows.
