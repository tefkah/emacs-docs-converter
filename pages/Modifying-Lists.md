

Next: [Sets And Lists](Sets-And-Lists.html), Previous: [List Variables](List-Variables.html), Up: [Lists](Lists.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 5.6 Modifying Existing List Structure

You can modify the CAR and CDR contents of a cons cell with the primitives `setcar` and `setcdr`. These are destructive operations because they change existing list structure. Destructive operations should be applied only to mutable lists, that is, lists constructed via `cons`, `list` or similar operations. Lists created by quoting are part of the program and should not be changed by destructive operations. See [Mutability](Mutability.html).

> **Common Lisp note:** Common Lisp uses functions `rplaca` and `rplacd` to alter list structure; they change structure the same way as `setcar` and `setcdr`, but the Common Lisp functions return the cons cell while `setcar` and `setcdr` return the new CAR or CDR.

|                                       |    |                                                                                  |
| :------------------------------------ | -- | :------------------------------------------------------------------------------- |
| • [Setcar](Setcar.html)               |    | Replacing an element in a list.                                                  |
| • [Setcdr](Setcdr.html)               |    | Replacing part of the list backbone. This can be used to remove or add elements. |
| • [Rearrangement](Rearrangement.html) |    | Reordering the elements in a list; combining lists.                              |
