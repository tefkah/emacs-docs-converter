

### 21.8 Reading Input

The editor command loop reads key sequences using the function `read-key-sequence`, which uses `read-event`. These and other functions for event input are also available for use in Lisp programs. See also `momentary-string-display` in [Temporary Displays](Temporary-Displays.html), and `sit-for` in [Waiting](Waiting.html). See [Terminal Input](Terminal-Input.html), for functions and variables for controlling terminal input modes and debugging terminal input.

For higher-level input facilities, see [Minibuffers](Minibuffers.html).

|                                                               |    |                                             |
| :------------------------------------------------------------ | -- | :------------------------------------------ |
| • [Key Sequence Input](Key-Sequence-Input.html)               |    | How to read one key sequence.               |
| • [Reading One Event](Reading-One-Event.html)                 |    | How to read just one event.                 |
| • [Event Mod](Event-Mod.html)                                 |    | How Emacs modifies events as they are read. |
| • [Invoking the Input Method](Invoking-the-Input-Method.html) |    | How reading an event uses the input method. |
| • [Quoted Character Input](Quoted-Character-Input.html)       |    | Asking the user to specify a character.     |
| • [Event Input Misc](Event-Input-Misc.html)                   |    | How to reread or throw away input events.   |
