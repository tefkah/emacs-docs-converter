

Next: [Display](Display.html), Previous: [Threads](Threads.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

## 38 Processes

In the terminology of operating systems, a *process* is a space in which a program can execute. Emacs runs in a process. Emacs Lisp programs can invoke other programs in processes of their own. These are called *subprocesses* or *child processes* of the Emacs process, which is their *parent process*.

A subprocess of Emacs may be *synchronous* or *asynchronous*, depending on how it is created. When you create a synchronous subprocess, the Lisp program waits for the subprocess to terminate before continuing execution. When you create an asynchronous subprocess, it can run in parallel with the Lisp program. This kind of subprocess is represented within Emacs by a Lisp object which is also called a “process”. Lisp programs can use this object to communicate with the subprocess or to control it. For example, you can send signals, obtain status information, receive output from the process, or send input to it.

In addition to processes that run programs, Lisp programs can open connections of several types to devices or processes running on the same machine or on other machines. The supported connection types are: TCP and UDP network connections, serial port connections, and pipe connections. Each such connection is also represented by a process object.

*   Function: **processp** *object*

    This function returns `t` if `object` represents an Emacs process object, `nil` otherwise. The process object can represent a subprocess running a program or a connection of any supported type.

In addition to subprocesses of the current Emacs session, you can also access other processes running on your machine. See [System Processes](System-Processes.html).

|                                                         |    |                                                                          |
| :------------------------------------------------------ | -- | :----------------------------------------------------------------------- |
| • [Subprocess Creation](Subprocess-Creation.html)       |    | Functions that start subprocesses.                                       |
| • [Shell Arguments](Shell-Arguments.html)               |    | Quoting an argument to pass it to a shell.                               |
| • [Synchronous Processes](Synchronous-Processes.html)   |    | Details of using synchronous subprocesses.                               |
| • [Asynchronous Processes](Asynchronous-Processes.html) |    | Starting up an asynchronous subprocess.                                  |
| • [Deleting Processes](Deleting-Processes.html)         |    | Eliminating an asynchronous subprocess.                                  |
| • [Process Information](Process-Information.html)       |    | Accessing run-status and other attributes.                               |
| • [Input to Processes](Input-to-Processes.html)         |    | Sending input to an asynchronous subprocess.                             |
| • [Signals to Processes](Signals-to-Processes.html)     |    | Stopping, continuing or interrupting an asynchronous subprocess.         |
| • [Output from Processes](Output-from-Processes.html)   |    | Collecting output from an asynchronous subprocess.                       |
| • [Sentinels](Sentinels.html)                           |    | Sentinels run when process run-status changes.                           |
| • [Query Before Exit](Query-Before-Exit.html)           |    | Whether to query if exiting will kill a process.                         |
| • [System Processes](System-Processes.html)             |    | Accessing other processes running on your system.                        |
| • [Transaction Queues](Transaction-Queues.html)         |    | Transaction-based communication with subprocesses.                       |
| • [Network](Network.html)                               |    | Opening network connections.                                             |
| • [Network Servers](Network-Servers.html)               |    | Network servers let Emacs accept net connections.                        |
| • [Datagrams](Datagrams.html)                           |    | UDP network connections.                                                 |
| • [Low-Level Network](Low_002dLevel-Network.html)       |    | Lower-level but more general function to create connections and servers. |
| • [Misc Network](Misc-Network.html)                     |    | Additional relevant functions for net connections.                       |
| • [Serial Ports](Serial-Ports.html)                     |    | Communicating with serial ports.                                         |
| • [Byte Packing](Byte-Packing.html)                     |    | Using bindat to pack and unpack binary data.                             |

Next: [Display](Display.html), Previous: [Threads](Threads.html), Up: [Top](index.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]
