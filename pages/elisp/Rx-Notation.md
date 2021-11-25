

#### 34.3.3 The `rx` Structured Regexp Notation

As an alternative to the string-based syntax, Emacs provides the structured `rx` notation based on Lisp S-expressions. This notation is usually easier to read, write and maintain than regexp strings, and can be indented and commented freely. It requires a conversion into string form since that is what regexp functions expect, but that conversion typically takes place during byte-compilation rather than when the Lisp code using the regexp is run.

Here is an `rx` regexp[19](#FOOT19) that matches a block comment in the C programming language:

```lisp
(rx "/*"                          ; Initial /*
    (zero-or-more
     (or (not (any "*"))          ;  Either non-*,
         (seq "*"                 ;  or * followed by
              (not (any "/")))))  ;  non-/
    (one-or-more "*")             ; At least one star,
    "/")                          ; and the final /
```

or, using shorter synonyms and written more compactly,

```lisp
(rx "/*"
    (* (| (not "*")
          (: "*" (not "/"))))
    (+ "*") "/")
```

In conventional string syntax, it would be written

```lisp
"/\\*\\(?:[^*]\\|\\*[^/]\\)*\\*+/"
```

The `rx` notation is mainly useful in Lisp code; it cannot be used in most interactive situations where a regexp is requested, such as when running `query-replace-regexp` or in variable customization.

|                                       |    |                                         |
| :------------------------------------ | -- | :-------------------------------------- |
| • [Rx Constructs](Rx-Constructs.html) |    | Constructs valid in rx forms.           |
| • [Rx Functions](Rx-Functions.html)   |    | Functions and macros that use rx forms. |
| • [Extending Rx](Extending-Rx.html)   |    | How to define your own rx forms.        |

***

#### Footnotes

##### [(19)](#DOCF19)

It could be written much simpler with non-greedy operators (how?), but that would make the example less interesting.