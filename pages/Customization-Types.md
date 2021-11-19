

Next: [Applying Customizations](Applying-Customizations.html), Previous: [Variable Definitions](Variable-Definitions.html), Up: [Customization](Customization.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 15.4 Customization Types

When you define a user option with `defcustom`, you must specify its *customization type*. That is a Lisp object which describes (1) which values are legitimate and (2) how to display the value in the customization buffer for editing.

You specify the customization type in `defcustom` with the `:type` keyword. The argument of `:type` is evaluated, but only once when the `defcustom` is executed, so it isn’t useful for the value to vary. Normally we use a quoted constant. For example:

```lisp
(defcustom diff-command "diff"
  "The command to use to run diff."
  :type '(string)
  :group 'diff)
```

In general, a customization type is a list whose first element is a symbol, one of the customization type names defined in the following sections. After this symbol come a number of arguments, depending on the symbol. Between the type symbol and its arguments, you can optionally write keyword-value pairs (see [Type Keywords](Type-Keywords.html)).

Some type symbols do not use any arguments; those are called *simple types*. For a simple type, if you do not use any keyword-value pairs, you can omit the parentheses around the type symbol. For example just `string` as a customization type is equivalent to `(string)`.

All customization types are implemented as widgets; see [Introduction](../widget/index.html#Top) in The Emacs Widget Library, for details.

|                                                   |    |                                                 |
| :------------------------------------------------ | -- | :---------------------------------------------- |
| • [Simple Types](Simple-Types.html)               |    | Simple customization types: sexp, integer, etc. |
| • [Composite Types](Composite-Types.html)         |    | Build new types from other types or data.       |
| • [Splicing into Lists](Splicing-into-Lists.html) |    | Splice elements into list with `:inline`.       |
| • [Type Keywords](Type-Keywords.html)             |    | Keyword-argument pairs in a customization type. |
| • [Defining New Types](Defining-New-Types.html)   |    | Give your type a name.                          |

Next: [Applying Customizations](Applying-Customizations.html), Previous: [Variable Definitions](Variable-Definitions.html), Up: [Customization](Customization.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
