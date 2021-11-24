

### 21.7 Input Events

The Emacs command loop reads a sequence of *input events* that represent keyboard or mouse activity, or system events sent to Emacs. The events for keyboard activity are characters or symbols; other events are always lists. This section describes the representation and meaning of input events in detail.

### Function: **eventp** *object*

This function returns non-`nil` if `object` is an input event or event type.

Note that any non-`nil` symbol might be used as an event or an event type; `eventp` cannot distinguish whether a symbol is intended by Lisp code to be used as an event.

|                                                     |    |                                                                           |
| :-------------------------------------------------- | -- | :------------------------------------------------------------------------ |
| • [Keyboard Events](Keyboard-Events.html)           |    | Ordinary characters – keys with symbols on them.                          |
| • [Function Keys](Function-Keys.html)               |    | Function keys – keys with names, not symbols.                             |
| • [Mouse Events](Mouse-Events.html)                 |    | Overview of mouse events.                                                 |
| • [Click Events](Click-Events.html)                 |    | Pushing and releasing a mouse button.                                     |
| • [Drag Events](Drag-Events.html)                   |    | Moving the mouse before releasing the button.                             |
| • [Button-Down Events](Button_002dDown-Events.html) |    | A button was pushed and not yet released.                                 |
| • [Repeat Events](Repeat-Events.html)               |    | Double and triple click (or drag, or down).                               |
| • [Motion Events](Motion-Events.html)               |    | Just moving the mouse, not pushing a button.                              |
| • [Focus Events](Focus-Events.html)                 |    | Moving the mouse between frames.                                          |
| • [Misc Events](Misc-Events.html)                   |    | Other events the system can generate.                                     |
| • [Event Examples](Event-Examples.html)             |    | Examples of the lists for mouse events.                                   |
| • [Classifying Events](Classifying-Events.html)     |    | Finding the modifier keys in an event symbol. Event types.                |
| • [Accessing Mouse](Accessing-Mouse.html)           |    | Functions to extract info from mouse events.                              |
| • [Accessing Scroll](Accessing-Scroll.html)         |    | Functions to get info from scroll bar events.                             |
| • [Strings of Events](Strings-of-Events.html)       |    | Special considerations for putting keyboard character events in a string. |
