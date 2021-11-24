

## 32 Text

This chapter describes the functions that deal with the text in a buffer. Most examine, insert, or delete text in the current buffer, often operating at point or on text adjacent to point. Many are interactive. All the functions that change the text provide for undoing the changes (see [Undo](Undo.html)).

Many text-related functions operate on a region of text defined by two buffer positions passed in arguments named `start` and `end`. These arguments should be either markers (see [Markers](Markers.html)) or numeric character positions (see [Positions](Positions.html)). The order of these arguments does not matter; it is all right for `start` to be the end of the region and `end` the beginning. For example, `(delete-region 1 10)` and `(delete-region 10 1)` are equivalent. An `args-out-of-range` error is signaled if either `start` or `end` is outside the accessible portion of the buffer. In an interactive call, point and the mark are used for these arguments.

Throughout this chapter, “text” refers to the characters in the buffer, together with their properties (when relevant). Keep in mind that point is always between two characters, and the cursor appears on the character after point.

|                                                         |    |                                                                                          |
| :------------------------------------------------------ | -- | :--------------------------------------------------------------------------------------- |
| • [Near Point](Near-Point.html)                         |    | Examining text in the vicinity of point.                                                 |
| • [Buffer Contents](Buffer-Contents.html)               |    | Examining text in a general fashion.                                                     |
| • [Comparing Text](Comparing-Text.html)                 |    | Comparing substrings of buffers.                                                         |
| • [Insertion](Insertion.html)                           |    | Adding new text to a buffer.                                                             |
| • [Commands for Insertion](Commands-for-Insertion.html) |    | User-level commands to insert text.                                                      |
| • [Deletion](Deletion.html)                             |    | Removing text from a buffer.                                                             |
| • [User-Level Deletion](User_002dLevel-Deletion.html)   |    | User-level commands to delete text.                                                      |
| • [The Kill Ring](The-Kill-Ring.html)                   |    | Where removed text sometimes is saved for later use.                                     |
| • [Undo](Undo.html)                                     |    | Undoing changes to the text of a buffer.                                                 |
| • [Maintaining Undo](Maintaining-Undo.html)             |    | How to enable and disable undo information. How to control how much information is kept. |
| • [Filling](Filling.html)                               |    | Functions for explicit filling.                                                          |
| • [Margins](Margins.html)                               |    | How to specify margins for filling commands.                                             |
| • [Adaptive Fill](Adaptive-Fill.html)                   |    | Adaptive Fill mode chooses a fill prefix from context.                                   |
| • [Auto Filling](Auto-Filling.html)                     |    | How auto-fill mode is implemented to break lines.                                        |
| • [Sorting](Sorting.html)                               |    | Functions for sorting parts of the buffer.                                               |
| • [Columns](Columns.html)                               |    | Computing horizontal positions, and using them.                                          |
| • [Indentation](Indentation.html)                       |    | Functions to insert or adjust indentation.                                               |
| • [Case Changes](Case-Changes.html)                     |    | Case conversion of parts of the buffer.                                                  |
| • [Text Properties](Text-Properties.html)               |    | Assigning Lisp property lists to text characters.                                        |
| • [Substitution](Substitution.html)                     |    | Replacing a given character wherever it appears.                                         |
| • [Registers](Registers.html)                           |    | How registers are implemented. Accessing the text or position stored in a register.      |
| • [Transposition](Transposition.html)                   |    | Swapping two portions of a buffer.                                                       |
| • [Replacing](Replacing.html)                           |    | Replacing the text of one buffer with the text of another buffer.                        |
| • [Decompression](Decompression.html)                   |    | Dealing with compressed data.                                                            |
| • [Base 64](Base-64.html)                               |    | Conversion to or from base 64 encoding.                                                  |
| • [Checksum/Hash](Checksum_002fHash.html)               |    | Computing cryptographic hashes.                                                          |
| • [GnuTLS Cryptography](GnuTLS-Cryptography.html)       |    | Cryptographic algorithms imported from GnuTLS.                                           |
| • [Parsing HTML/XML](Parsing-HTML_002fXML.html)         |    | Parsing HTML and XML.                                                                    |
| • [Parsing JSON](Parsing-JSON.html)                     |    | Parsing and generating JSON values.                                                      |
| • [JSONRPC](JSONRPC.html)                               |    | JSON Remote Procedure Call protocol                                                      |
| • [Atomic Changes](Atomic-Changes.html)                 |    | Installing several buffer changes atomically.                                            |
| • [Change Hooks](Change-Hooks.html)                     |    | Supplying functions to be run when text is changed.                                      |
