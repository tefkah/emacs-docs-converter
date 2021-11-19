

Next: [Loading](Loading.html), Previous: [Macros](Macros.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 15 Customization Settings

Users of Emacs can customize variables and faces without writing Lisp code, by using the Customize interface. See [Easy Customization](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization) in The GNU Emacs Manual. This chapter describes how to define *customization items* that users can interact with through the Customize interface.

Customization items include customizable variables, which are defined with the `defcustom` macro; customizable faces, which are defined with `defface` (described separately in [Defining Faces](Defining-Faces.html)); and *customization groups*, defined with `defgroup`, which act as containers for groups of related customization items.

|                                                           |    |                                                                       |
| :-------------------------------------------------------- | -- | :-------------------------------------------------------------------- |
| • [Common Keywords](Common-Keywords.html)                 |    | Common keyword arguments for all kinds of customization declarations. |
| • [Group Definitions](Group-Definitions.html)             |    | Writing customization group definitions.                              |
| • [Variable Definitions](Variable-Definitions.html)       |    | Declaring user options.                                               |
| • [Customization Types](Customization-Types.html)         |    | Specifying the type of a user option.                                 |
| • [Applying Customizations](Applying-Customizations.html) |    | Functions to apply customization settings.                            |
| • [Custom Themes](Custom-Themes.html)                     |    | Writing Custom themes.                                                |
