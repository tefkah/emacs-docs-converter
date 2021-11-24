

### 12.10 Scoping Rules for Variable Bindings

When you create a local binding for a variable, that binding takes effect only within a limited portion of the program (see [Local Variables](Local-Variables.html)). This section describes exactly what this means.

Each local binding has a certain *scope* and *extent*. *Scope* refers to *where* in the textual source code the binding can be accessed. *Extent* refers to *when*, as the program is executing, the binding exists.

By default, the local bindings that Emacs creates are *dynamic bindings*. Such a binding has *dynamic scope*, meaning that any part of the program can potentially access the variable binding. It also has *dynamic extent*, meaning that the binding lasts only while the binding construct (such as the body of a `let` form) is being executed.

Emacs can optionally create *lexical bindings*. A lexical binding has *lexical scope*, meaning that any reference to the variable must be located textually within the binding construct[9](#FOOT9). It also has *indefinite extent*, meaning that under some circumstances the binding can live on even after the binding construct has finished executing, by means of special objects called *closures*.

The following subsections describe dynamic binding and lexical binding in greater detail, and how to enable lexical binding in Emacs Lisp programs.

|                                                       |    |                                                   |
| :---------------------------------------------------- | -- | :------------------------------------------------ |
| • [Dynamic Binding](Dynamic-Binding.html)             |    | The default for binding local variables in Emacs. |
| • [Dynamic Binding Tips](Dynamic-Binding-Tips.html)   |    | Avoiding problems with dynamic binding.           |
| • [Lexical Binding](Lexical-Binding.html)             |    | A different type of local variable binding.       |
| • [Using Lexical Binding](Using-Lexical-Binding.html) |    | How to enable lexical binding.                    |

***

#### Footnotes

##### [(9)](#DOCF9)

With some exceptions; for instance, a lexical binding can also be accessed from the Lisp debugger.
