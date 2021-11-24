

### 9.4 Symbol Properties

A symbol may possess any number of *symbol properties*, which can be used to record miscellaneous information about the symbol. For example, when a symbol has a `risky-local-variable` property with a non-`nil` value, that means the variable which the symbol names is a risky file-local variable (see [File Local Variables](File-Local-Variables.html)).

Each symbol’s properties and property values are stored in the symbol’s property list cell (see [Symbol Components](Symbol-Components.html)), in the form of a property list (see [Property Lists](Property-Lists.html)).

|                                                   |    |                                         |
| :------------------------------------------------ | -- | :-------------------------------------- |
| • [Symbol Plists](Symbol-Plists.html)             |    | Accessing symbol properties.            |
| • [Standard Properties](Standard-Properties.html) |    | Standard meanings of symbol properties. |
