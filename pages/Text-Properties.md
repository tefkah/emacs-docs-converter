

Next: [Substitution](Substitution.html), Previous: [Case Changes](Case-Changes.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 32.19 Text Properties

Each character position in a buffer or a string can have a *text property list*, much like the property list of a symbol (see [Property Lists](Property-Lists.html)). The properties belong to a particular character at a particular place, such as, the letter ‘`T`’ at the beginning of this sentence or the first ‘`o`’ in ‘`foo`’—if the same character occurs in two different places, the two occurrences in general have different properties.

Each property has a name and a value. Both of these can be any Lisp object, but the name is normally a symbol. Typically each property name symbol is used for a particular purpose; for instance, the text property `face` specifies the faces for displaying the character (see [Special Properties](Special-Properties.html)). The usual way to access the property list is to specify a name and ask what value corresponds to it.

If a character has a `category` property, we call it the *property category* of the character. It should be a symbol. The properties of the symbol serve as defaults for the properties of the character.

Copying text between strings and buffers preserves the properties along with the characters; this includes such diverse functions as `substring`, `insert`, and `buffer-substring`.

|                                                     |    |                                                                                    |
| :-------------------------------------------------- | -- | :--------------------------------------------------------------------------------- |
| • [Examining Properties](Examining-Properties.html) |    | Looking at the properties of one character.                                        |
| • [Changing Properties](Changing-Properties.html)   |    | Setting the properties of a range of text.                                         |
| • [Property Search](Property-Search.html)           |    | Searching for where a property changes value.                                      |
| • [Special Properties](Special-Properties.html)     |    | Particular properties with special meanings.                                       |
| • [Format Properties](Format-Properties.html)       |    | Properties for representing formatting of text.                                    |
| • [Sticky Properties](Sticky-Properties.html)       |    | How inserted text gets properties from neighboring text.                           |
| • [Lazy Properties](Lazy-Properties.html)           |    | Computing text properties in a lazy fashion only when text is examined.            |
| • [Clickable Text](Clickable-Text.html)             |    | Using text properties to make regions of text do something when you click on them. |
| • [Fields](Fields.html)                             |    | The `field` property defines fields within the buffer.                             |
| • [Not Intervals](Not-Intervals.html)               |    | Why text properties do not use Lisp-visible text intervals.                        |

Next: [Substitution](Substitution.html), Previous: [Case Changes](Case-Changes.html), Up: [Text](Text.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
