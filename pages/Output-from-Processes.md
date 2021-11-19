

Next: [Sentinels](Sentinels.html), Previous: [Signals to Processes](Signals-to-Processes.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

### 38.9 Receiving Output from Processes

The output that an asynchronous subprocess writes to its standard output stream is passed to a function called the *filter function*. The default filter function simply inserts the output into a buffer, which is called the associated buffer of the process (see [Process Buffers](Process-Buffers.html)). If the process has no buffer then the default filter discards the output.

If the subprocess writes to its standard error stream, by default the error output is also passed to the process filter function. If Emacs uses a pseudo-TTY (pty) for communication with the subprocess, then it is impossible to separate the standard output and standard error streams of the subprocess, because a pseudo-TTY has only one output channel. In that case, if you want to keep the output to those streams separate, you should redirect one of them to a file—for example, by using an appropriate shell command via `start-process-shell-command` or a similar function.

Alternatively, you could use the `:stderr` parameter with a non-`nil` value in a call to `make-process` (see [make-process](Asynchronous-Processes.html)) to make the destination of the error output separate from the standard output; in that case, Emacs will use pipes for communicating with the subprocess.

When a subprocess terminates, Emacs reads any pending output, then stops reading output from that subprocess. Therefore, if the subprocess has children that are still live and still producing output, Emacs won’t receive that output.

Output from a subprocess can arrive only while Emacs is waiting: when reading terminal input (see the function `waiting-for-user-input-p`), in `sit-for` and `sleep-for` (see [Waiting](Waiting.html)), in `accept-process-output` (see [Accepting Output](Accepting-Output.html)), and in functions which send data to processes (see [Input to Processes](Input-to-Processes.html)). This minimizes the problem of timing errors that usually plague parallel programming. For example, you can safely create a process and only then specify its buffer or filter function; no output can arrive before you finish, if the code in between does not call any primitive that waits.

*   Variable: **process-adaptive-read-buffering**

    On some systems, when Emacs reads the output from a subprocess, the output data is read in very small blocks, potentially resulting in very poor performance. This behavior can be remedied to some extent by setting the variable `process-adaptive-read-buffering` to a non-`nil` value (the default), as it will automatically delay reading from such processes, thus allowing them to produce more output before Emacs tries to read it.

|                                                       |    |                                                  |
| :---------------------------------------------------- | -- | :----------------------------------------------- |
| • [Process Buffers](Process-Buffers.html)             |    | By default, output is put in a buffer.           |
| • [Filter Functions](Filter-Functions.html)           |    | Filter functions accept output from the process. |
| • [Decoding Output](Decoding-Output.html)             |    | Filters can get unibyte or multibyte strings.    |
| • [Accepting Output](Accepting-Output.html)           |    | How to wait until process output arrives.        |
| • [Processes and Threads](Processes-and-Threads.html) |    | How processes and threads interact.              |

Next: [Sentinels](Sentinels.html), Previous: [Signals to Processes](Signals-to-Processes.html), Up: [Processes](Processes.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
