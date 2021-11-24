

### 23.6 Font Lock Mode

*Font Lock mode* is a buffer-local minor mode that automatically attaches `face` properties to certain parts of the buffer based on their syntactic role. How it parses the buffer depends on the major mode; most major modes define syntactic criteria for which faces to use in which contexts. This section explains how to customize Font Lock for a particular major mode.

Font Lock mode finds text to highlight in two ways: through syntactic parsing based on the syntax table, and through searching (usually for regular expressions). Syntactic fontification happens first; it finds comments and string constants and highlights them. Search-based fontification happens second.

|                                                                     |    |                                                                                        |
| :------------------------------------------------------------------ | -- | :------------------------------------------------------------------------------------- |
| • [Font Lock Basics](Font-Lock-Basics.html)                         |    | Overview of customizing Font Lock.                                                     |
| • [Search-based Fontification](Search_002dbased-Fontification.html) |    | Fontification based on regexps.                                                        |
| • [Customizing Keywords](Customizing-Keywords.html)                 |    | Customizing search-based fontification.                                                |
| • [Other Font Lock Variables](Other-Font-Lock-Variables.html)       |    | Additional customization facilities.                                                   |
| • [Levels of Font Lock](Levels-of-Font-Lock.html)                   |    | Each mode can define alternative levels so that the user can select more or less.      |
| • [Precalculated Fontification](Precalculated-Fontification.html)   |    | How Lisp programs that produce the buffer contents can also specify how to fontify it. |
| • [Faces for Font Lock](Faces-for-Font-Lock.html)                   |    | Special faces specifically for Font Lock.                                              |
| • [Syntactic Font Lock](Syntactic-Font-Lock.html)                   |    | Fontification based on syntax tables.                                                  |
| • [Multiline Font Lock](Multiline-Font-Lock.html)                   |    | How to coerce Font Lock into properly highlighting multiline constructs.               |
