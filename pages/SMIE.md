

Up: [Auto-Indentation](Auto_002dIndentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 23.7.1 Simple Minded Indentation Engine

SMIE is a package that provides a generic navigation and indentation engine. Based on a very simple parser using an operator precedence grammar, it lets major modes extend the sexp-based navigation of Lisp to non-Lisp languages as well as provide a simple to use but reliable auto-indentation.

Operator precedence grammar is a very primitive technology for parsing compared to some of the more common techniques used in compilers. It has the following characteristics: its parsing power is very limited, and it is largely unable to detect syntax errors, but it has the advantage of being algorithmically efficient and able to parse forward just as well as backward. In practice that means that SMIE can use it for indentation based on backward parsing, that it can provide both `forward-sexp` and `backward-sexp` functionality, and that it will naturally work on syntactically incorrect code without any extra effort. The downside is that it also means that most programming languages cannot be parsed correctly using SMIE, at least not without resorting to some special tricks (see [SMIE Tricks](SMIE-Tricks.html)).

|                                                                     |    |                                          |
| :------------------------------------------------------------------ | -- | :--------------------------------------- |
| • [SMIE setup](SMIE-setup.html)                                     |    | SMIE setup and features.                 |
| • [Operator Precedence Grammars](Operator-Precedence-Grammars.html) |    | A very simple parsing technique.         |
| • [SMIE Grammar](SMIE-Grammar.html)                                 |    | Defining the grammar of a language.      |
| • [SMIE Lexer](SMIE-Lexer.html)                                     |    | Defining tokens.                         |
| • [SMIE Tricks](SMIE-Tricks.html)                                   |    | Working around the parser’s limitations. |
| • [SMIE Indentation](SMIE-Indentation.html)                         |    | Specifying indentation rules.            |
| • [SMIE Indentation Helpers](SMIE-Indentation-Helpers.html)         |    | Helper functions for indentation rules.  |
| • [SMIE Indentation Example](SMIE-Indentation-Example.html)         |    | Sample indentation rules.                |
| • [SMIE Customization](SMIE-Customization.html)                     |    | Customizing indentation.                 |

Up: [Auto-Indentation](Auto_002dIndentation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
