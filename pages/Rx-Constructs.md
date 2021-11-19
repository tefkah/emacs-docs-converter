

Next: [Rx Functions](Rx-Functions.html), Up: [Rx Notation](Rx-Notation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 34.3.3.1 Constructs in `rx` regexps

The various forms in `rx` regexps are described below. The shorthand `rx` represents any `rx` form, and `rx`… means zero or more `rx` forms. Where the corresponding string regexp syntax is given, `A`, `B`, … are string regexp subexpressions.

#### Literals

*   `"some-string"`

    Match the string ‘`some-string`’ literally. There are no characters with special meaning, unlike in string regexps.

*   `?C`

    Match the character ‘`C`’ literally.

#### Sequence and alternative

*   `(seq rx…)`

*   `(sequence rx…)`

*   `(: rx…)`

*   `(and rx…)`

    Match the `rx`s in sequence. Without arguments, the expression matches the empty string.\
    Corresponding string regexp: ‘`AB…`’ (subexpressions in sequence).

*   `(or rx…)`

*   `(| rx…)`

    Match exactly one of the `rx`s. If all arguments are strings, characters, or `or` forms so constrained, the longest possible match will always be used. Otherwise, either the longest match or the first (in left-to-right order) will be used. Without arguments, the expression will not match anything at all.\
    Corresponding string regexp: ‘`A\|B\|…`’.

*   `unmatchable`

    Refuse any match. Equivalent to `(or)`. See [regexp-unmatchable](Regexp-Functions.html#regexp_002dunmatchable).

#### Repetition

Normally, repetition forms are greedy, in that they attempt to match as many times as possible. Some forms are non-greedy; they try to match as few times as possible (see [Non-greedy repetition](Regexp-Special.html#Non_002dgreedy-repetition)).

*   `(zero-or-more rx…)`

*   `(0+ rx…)`

    Match the `rx`s zero or more times. Greedy by default.\
    Corresponding string regexp: ‘`A*`’ (greedy), ‘`A*?`’ (non-greedy)

*   `(one-or-more rx…)`

*   `(1+ rx…)`

    Match the `rx`s one or more times. Greedy by default.\
    Corresponding string regexp: ‘`A+`’ (greedy), ‘`A+?`’ (non-greedy)

*   `(zero-or-one rx…)`

*   `(optional rx…)`

*   `(opt rx…)`

    Match the `rx`s once or an empty string. Greedy by default.\
    Corresponding string regexp: ‘`A?`’ (greedy), ‘`A??`’ (non-greedy).

*   `(* rx…)`

    Match the `rx`s zero or more times. Greedy.\
    Corresponding string regexp: ‘`A*`’

*   `(+ rx…)`

    Match the `rx`s one or more times. Greedy.\
    Corresponding string regexp: ‘`A+`’

*   `(? rx…)`

    Match the `rx`s once or an empty string. Greedy.\
    Corresponding string regexp: ‘`A?`’

*   `(*? rx…)`

    Match the `rx`s zero or more times. Non-greedy.\
    Corresponding string regexp: ‘`A*?`’

*   `(+? rx…)`

    Match the `rx`s one or more times. Non-greedy.\
    Corresponding string regexp: ‘`A+?`’

*   `(?? rx…)`

    Match the `rx`s or an empty string. Non-greedy.\
    Corresponding string regexp: ‘`A??`’

*   `(= n rx…)`

*   `(repeat n rx)`

    Match the `rx`s exactly `n` times.\
    Corresponding string regexp: ‘`A\{n\}`’

*   `(>= n rx…)`

    Match the `rx`s `n` or more times. Greedy.\
    Corresponding string regexp: ‘`A\{n,\}`’

*   `(** n m rx…)`

*   `(repeat n m rx…)`

    Match the `rx`s at least `n` but no more than `m` times. Greedy.\
    Corresponding string regexp: ‘`A\{n,m\}`’

The greediness of some repetition forms can be controlled using the following constructs. However, it is usually better to use the explicit non-greedy forms above when such matching is required.

*   `(minimal-match rx)`

    Match `rx`, with `zero-or-more`, `0+`, `one-or-more`, `1+`, `zero-or-one`, `opt` and `optional` using non-greedy matching.

*   `(maximal-match rx)`

    Match `rx`, with `zero-or-more`, `0+`, `one-or-more`, `1+`, `zero-or-one`, `opt` and `optional` using greedy matching. This is the default.

#### Matching single characters

*   `(any set…)`

*   `(char set…)`

*   `(in set…)`

    Match a single character from one of the `set`s. Each `set` is a character, a string representing the set of its characters, a range or a character class (see below). A range is either a hyphen-separated string like `"A-Z"`, or a cons of characters like `(?A . ?Z)`.

    Note that hyphen (`-`) is special in strings in this construct, since it acts as a range separator. To include a hyphen, add it as a separate character or single-character string.\
    Corresponding string regexp: ‘`[…]`’

*   `(not charspec)`

    Match a character not included in `charspec`. `charspec` can be a character, a single-character string, an `any`, `not`, `or`, `intersection`, `syntax` or `category` form, or a character class. If `charspec` is an `or` form, its arguments have the same restrictions as those of `intersection`; see below.\
    Corresponding string regexp: ‘`[^…]`’, ‘`\Scode`’, ‘`\Ccode`’

*   `(intersection charset…)`

    Match a character included in all of the `charset`s. Each `charset` can be a character, a single-character string, an `any` form without character classes, or an `intersection`, `or` or `not` form whose arguments are also `charset`s.

*   `not-newline`, `nonl`

    Match any character except a newline.\
    Corresponding string regexp: ‘`.`’ (dot)

*   `anychar`, `anything`

    Match any character.\
    Corresponding string regexp: ‘`.\|\n`’ (for example)

*   character class

    Match a character from a named character class:

    *   `alpha`, `alphabetic`, `letter`

        Match alphabetic characters. More precisely, match characters whose Unicode ‘`general-category`’ property indicates that they are alphabetic.

    *   `alnum`, `alphanumeric`

        Match alphabetic characters and digits. More precisely, match characters whose Unicode ‘`general-category`’ property indicates that they are alphabetic or decimal digits.

    *   `digit`, `numeric`, `num`

        Match the digits ‘`0`’–‘`9`’.

    *   `xdigit`, `hex-digit`, `hex`

        Match the hexadecimal digits ‘`0`’–‘`9`’, ‘`A`’–‘`F`’ and ‘`a`’–‘`f`’.

    *   `cntrl`, `control`

        Match any character whose code is in the range 0–31.

    *   `blank`

        Match horizontal whitespace. More precisely, match characters whose Unicode ‘`general-category`’ property indicates that they are spacing separators.

    *   `space`, `whitespace`, `white`

        Match any character that has whitespace syntax (see [Syntax Class Table](Syntax-Class-Table.html)).

    *   `lower`, `lower-case`

        Match anything lower-case, as determined by the current case table. If `case-fold-search` is non-nil, this also matches any upper-case letter.

    *   `upper`, `upper-case`

        Match anything upper-case, as determined by the current case table. If `case-fold-search` is non-nil, this also matches any lower-case letter.

    *   `graph`, `graphic`

        Match any character except whitespace, ASCII and non-ASCII control characters, surrogates, and codepoints unassigned by Unicode, as indicated by the Unicode ‘`general-category`’ property.

    *   `print`, `printing`

        Match whitespace or a character matched by `graph`.

    *   `punct`, `punctuation`

        Match any punctuation character. (At present, for multibyte characters, anything that has non-word syntax.)

    *   `word`, `wordchar`

        Match any character that has word syntax (see [Syntax Class Table](Syntax-Class-Table.html)).

    *   `ascii`

        Match any ASCII character (codes 0–127).

    *   `nonascii`

        Match any non-ASCII character (but not raw bytes).

    Corresponding string regexp: ‘`[[:class:]]`’

*   `(syntax syntax)`

    Match a character with syntax `syntax`, being one of the following names:

    | Syntax name         | Syntax character |
    | ------------------- | ---------------- |
    | `whitespace`        | `-`              |
    | `punctuation`       | `.`              |
    | `word`              | `w`              |
    | `symbol`            | `_`              |
    | `open-parenthesis`  | `(`              |
    | `close-parenthesis` | `)`              |
    | `expression-prefix` | `'`              |
    | `string-quote`      | `"`              |
    | `paired-delimiter`  | `$`              |
    | `escape`            | `\`              |
    | `character-quote`   | `/`              |
    | `comment-start`     | `<`              |
    | `comment-end`       | `>`              |
    | `string-delimiter`  | `\|`             |
    | `comment-delimiter` | `!`              |

    For details, see [Syntax Class Table](Syntax-Class-Table.html). Please note that `(syntax punctuation)` is *not* equivalent to the character class `punctuation`.\
    Corresponding string regexp: ‘`\scode`’

*   `(category category)`

    Match a character in category `category`, which is either one of the names below or its category character.

    | Category name                      | Category character |
    | ---------------------------------- | ------------------ |
    | `space-for-indent`                 | space              |
    | `base`                             | `.`                |
    | `consonant`                        | `0`                |
    | `base-vowel`                       | `1`                |
    | `upper-diacritical-mark`           | `2`                |
    | `lower-diacritical-mark`           | `3`                |
    | `tone-mark`                        | `4`                |
    | `symbol`                           | `5`                |
    | `digit`                            | `6`                |
    | `vowel-modifying-diacritical-mark` | `7`                |
    | `vowel-sign`                       | `8`                |
    | `semivowel-lower`                  | `9`                |
    | `not-at-end-of-line`               | `<`                |
    | `not-at-beginning-of-line`         | `>`                |
    | `alpha-numeric-two-byte`           | `A`                |
    | `chinese-two-byte`                 | `C`                |
    | `greek-two-byte`                   | `G`                |
    | `japanese-hiragana-two-byte`       | `H`                |
    | `indian-two-byte`                  | `I`                |
    | `japanese-katakana-two-byte`       | `K`                |
    | `strong-left-to-right`             | `L`                |
    | `korean-hangul-two-byte`           | `N`                |
    | `strong-right-to-left`             | `R`                |
    | `cyrillic-two-byte`                | `Y`                |
    | `combining-diacritic`              | `^`                |
    | `ascii`                            | `a`                |
    | `arabic`                           | `b`                |
    | `chinese`                          | `c`                |
    | `ethiopic`                         | `e`                |
    | `greek`                            | `g`                |
    | `korean`                           | `h`                |
    | `indian`                           | `i`                |
    | `japanese`                         | `j`                |
    | `japanese-katakana`                | `k`                |
    | `latin`                            | `l`                |
    | `lao`                              | `o`                |
    | `tibetan`                          | `q`                |
    | `japanese-roman`                   | `r`                |
    | `thai`                             | `t`                |
    | `vietnamese`                       | `v`                |
    | `hebrew`                           | `w`                |
    | `cyrillic`                         | `y`                |
    | `can-break`                        | `\|`               |

    For more information about currently defined categories, run the command `M-x describe-categories RET`. For how to define new categories, see [Categories](Categories.html).\
    Corresponding string regexp: ‘`\ccode`’

#### Zero-width assertions

These all match the empty string, but only in specific places.

*   `line-start`, `bol`

    Match at the beginning of a line.\
    Corresponding string regexp: ‘`^`’

*   `line-end`, `eol`

    Match at the end of a line.\
    Corresponding string regexp: ‘`$`’

*   `string-start`, `bos`, `buffer-start`, `bot`

    Match at the start of the string or buffer being matched against.\
    Corresponding string regexp: ‘`` \` ``’

*   `string-end`, `eos`, `buffer-end`, `eot`

    Match at the end of the string or buffer being matched against.\
    Corresponding string regexp: ‘`\'`’

*   `point`

    Match at point.\
    Corresponding string regexp: ‘`\=`’

*   `word-start`, `bow`

    Match at the beginning of a word.\
    Corresponding string regexp: ‘`\<`’

*   `word-end`, `eow`

    Match at the end of a word.\
    Corresponding string regexp: ‘`\>`’

*   `word-boundary`

    Match at the beginning or end of a word.\
    Corresponding string regexp: ‘`\b`’

*   `not-word-boundary`

    Match anywhere but at the beginning or end of a word.\
    Corresponding string regexp: ‘`\B`’

*   `symbol-start`

    Match at the beginning of a symbol.\
    Corresponding string regexp: ‘`\_<`’

*   `symbol-end`

    Match at the end of a symbol.\
    Corresponding string regexp: ‘`\_>`’

#### Capture groups

*   `(group rx…)`

*   `(submatch rx…)`

    Match the `rx`s, making the matched text and position accessible in the match data. The first group in a regexp is numbered 1; subsequent groups will be numbered one higher than the previous group.\
    Corresponding string regexp: ‘`\(…\)`’

*   `(group-n n rx…)`

*   `(submatch-n n rx…)`

    Like `group`, but explicitly assign the group number `n`. `n` must be positive.\
    Corresponding string regexp: ‘`\(?n:…\)`’

*   `(backref n)`

    Match the text previously matched by group number `n`. `n` must be in the range 1–9.\
    Corresponding string regexp: ‘`\n`’

#### Dynamic inclusion

*   `(literal expr)`

    Match the literal string that is the result from evaluating the Lisp expression `expr`. The evaluation takes place at call time, in the current lexical environment.

*   `(regexp expr)`

*   `(regex expr)`

    Match the string regexp that is the result from evaluating the Lisp expression `expr`. The evaluation takes place at call time, in the current lexical environment.

*   `(eval expr)`

    Match the rx form that is the result from evaluating the Lisp expression `expr`. The evaluation takes place at macro-expansion time for `rx`, at call time for `rx-to-string`, in the current global environment.

Next: [Rx Functions](Rx-Functions.html), Up: [Rx Notation](Rx-Notation.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
