

## 1 Introduction

Most of the GNU Emacs text editor is written in the programming language called Emacs Lisp. You can write new code in Emacs Lisp and install it as an extension to the editor. However, Emacs Lisp is more than a mere extension language; it is a full computer programming language in its own right. You can use it as you would any other programming language.

Because Emacs Lisp is designed for use in an editor, it has special features for scanning and parsing text as well as features for handling files, buffers, displays, subprocesses, and so on. Emacs Lisp is closely integrated with the editing facilities; thus, editing commands are functions that can also conveniently be called from Lisp programs, and parameters for customization are ordinary Lisp variables.

This manual attempts to be a full description of Emacs Lisp. For a beginner’s introduction to Emacs Lisp, see An Introduction to Emacs Lisp Programming, by Bob Chassell, also published by the Free Software Foundation. This manual presumes considerable familiarity with the use of Emacs for editing; see The GNU Emacs Manual for this basic information.

Generally speaking, the earlier chapters describe features of Emacs Lisp that have counterparts in many programming languages, and later chapters describe features that are peculiar to Emacs Lisp or relate specifically to editing.

This is the GNU Emacs Lisp Reference Manual, corresponding to Emacs version 27.2.

|                                           |    |                                                    |
| :---------------------------------------- | -- | :------------------------------------------------- |
| • [Caveats](Caveats.html)                 |    | Flaws and a request for help.                      |
| • [Lisp History](Lisp-History.html)       |    | Emacs Lisp is descended from Maclisp.              |
| • [Conventions](Conventions.html)         |    | How the manual is formatted.                       |
| • [Version Info](Version-Info.html)       |    | Which Emacs version is running?                    |
| • [Acknowledgments](Acknowledgments.html) |    | The authors, editors, and sponsors of this manual. |