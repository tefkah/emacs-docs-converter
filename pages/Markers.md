

Next: [Text](Text.html), Previous: [Positions](Positions.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 31 Markers

A *marker* is a Lisp object used to specify a position in a buffer relative to the surrounding text. A marker changes its offset from the beginning of the buffer automatically whenever text is inserted or deleted, so that it stays with the two characters on either side of it.

|                                                             |    |                                                                 |
| :---------------------------------------------------------- | -- | :-------------------------------------------------------------- |
| • [Overview of Markers](Overview-of-Markers.html)           |    | The components of a marker, and how it relocates.               |
| • [Predicates on Markers](Predicates-on-Markers.html)       |    | Testing whether an object is a marker.                          |
| • [Creating Markers](Creating-Markers.html)                 |    | Making empty markers or markers at certain places.              |
| • [Information from Markers](Information-from-Markers.html) |    | Finding the marker’s buffer or character position.              |
| • [Marker Insertion Types](Marker-Insertion-Types.html)     |    | Two ways a marker can relocate when you insert where it points. |
| • [Moving Markers](Moving-Markers.html)                     |    | Moving the marker to a new buffer or position.                  |
| • [The Mark](The-Mark.html)                                 |    | How the mark is implemented with a marker.                      |
| • [The Region](The-Region.html)                             |    | How to access the region.                                       |
