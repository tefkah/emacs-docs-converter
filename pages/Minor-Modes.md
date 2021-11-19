

Next: [Mode Line Format](Mode-Line-Format.html), Previous: [Major Modes](Major-Modes.html), Up: [Modes](Modes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 23.3 Minor Modes

A *minor mode* provides optional features that users may enable or disable independently of the choice of major mode. Minor modes can be enabled individually or in combination.

Most minor modes implement features that are independent of the major mode, and can thus be used with most major modes. For example, Auto Fill mode works with any major mode that permits text insertion. A few minor modes, however, are specific to a particular major mode. For example, Diff Auto Refine mode is a minor mode that is intended to be used only with Diff mode.

Ideally, a minor mode should have its desired effect regardless of the other minor modes in effect. It should be possible to activate and deactivate minor modes in any order.

*   Variable: **minor-mode-list**

    The value of this variable is a list of all minor mode commands.

|                                                           |    |                                                 |
| :-------------------------------------------------------- | -- | :---------------------------------------------- |
| • [Minor Mode Conventions](Minor-Mode-Conventions.html)   |    | Tips for writing a minor mode.                  |
| • [Keymaps and Minor Modes](Keymaps-and-Minor-Modes.html) |    | How a minor mode can have its own keymap.       |
| • [Defining Minor Modes](Defining-Minor-Modes.html)       |    | A convenient facility for defining minor modes. |
