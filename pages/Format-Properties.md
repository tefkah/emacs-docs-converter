

Next: [Sticky Properties](Sticky-Properties.html), Previous: [Special Properties](Special-Properties.html), Up: [Text Properties](Text-Properties.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 32.19.5 Formatted Text Properties

These text properties affect the behavior of the fill commands. They are used for representing formatted text. See [Filling](Filling.html), and [Margins](Margins.html).

*   `hard`

    If a newline character has this property, it is a “hard” newline. The fill commands do not alter hard newlines and do not move words across them. However, this property takes effect only if the `use-hard-newlines` minor mode is enabled. See [Hard and Soft Newlines](https://www.gnu.org/software/emacs/manual/html_node/emacs/Hard-and-Soft-Newlines.html#Hard-and-Soft-Newlines) in The GNU Emacs Manual.

*   `right-margin`

    This property specifies an extra right margin for filling this part of the text.

*   `left-margin`

    This property specifies an extra left margin for filling this part of the text.

*   `justification`

    This property specifies the style of justification for filling this part of the text.
