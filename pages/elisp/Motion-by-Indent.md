

#### 32.17.6 Indentation-Based Motion Commands

These commands, primarily for interactive use, act based on the indentation in the text.

### Command: **back-to-indentation**

This command moves point to the first non-whitespace character in the current line (which is the line in which point is located). It returns `nil`.

### Command: **backward-to-indentation** *\&optional arg*

This command moves point backward `arg` lines and then to the first nonblank character on that line. It returns `nil`. If `arg` is omitted or `nil`, it defaults to 1.

### Command: **forward-to-indentation** *\&optional arg*

This command moves point forward `arg` lines and then to the first nonblank character on that line. It returns `nil`. If `arg` is omitted or `nil`, it defaults to 1.
