
$Id$

POSIX Ada Binding for Windows 95/98 and Windows NT
==================================================

This is a partial implementation of POSIX using the Win32 API.
Version 1.4 beta


changes since 1.3b
------------------

POSIX_Process_Environment 
    fix bug in For_Every_Environment_Variable

POSIX_Process_Primitives
    implementation of Wait_For_Child_Process (for any running process)

    Wait_For_Child_Process (for a group of process) is implemented by waiting
    for any child process. Under NT there is no groups, so I did consider that
    any process are running with the same group.

    implementation of block/non block in Wait_For_Child_Process
