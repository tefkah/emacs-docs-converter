

Previous: [A Sample Function Description](A-Sample-Function-Description.html), Up: [Format of Descriptions](Format-of-Descriptions.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 1.3.7.2 A Sample Variable Description

A *variable* is a name that can be *bound* (or *set*) to an object. The object to which a variable is bound is called a *value*; we say also that variable holds that value. Although nearly all variables can be set by the user, certain variables exist specifically so that users can change them; these are called *user options*. Ordinary variables and user options are described using a format like that for functions, except that there are no arguments.

Here is a description of the imaginary `electric-future-map` variable.

*   Variable: **electric-future-map**

    The value of this variable is a full keymap used by Electric Command Future mode. The functions in this map allow you to edit commands you have not yet thought about executing.

User option descriptions have the same format, but ‘`Variable`’ is replaced by ‘`User Option`’.
