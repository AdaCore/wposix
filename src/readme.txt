
$Id$

POSIX Ada Binding for Windows 95/98 and Windows NT
==================================================

This is a partial implementation of POSIX using the Win32 API.
Version 1.7 beta

Thanks to Sune Falck for the help provided from version 1.4. It has been
reported that this binding compile fine with GNAT 3.10 and ObjectAda 7.1.2.


changes since 1.6b
------------------

The binding is now thread safe. It means that most of the packages have been
changed to remove use of variables declared at the package level or to add a
Lock protected object as in POSIX_Win32.File_Handle.

POSIX_Calendar
	Split fix bug in seconds computing

POSIX_File_Status
	Get_File_Status : add FindClose to release the handle

POSIX_Files
	Fix Existence function

demo7
	remove dependency to TOD_Utilities package


changes since 1.5b
------------------

POSIX_File_Status
	fix bug in Get_File_Status this fix directory pathname style name 
	like "c:", "c:\" and "c:\."

POSIX_IO
	implementation of Is_A_Terminal

POSIX_Process_Times
	fix Filetime_To_Tick
	fix Elapsed_Real_Time_Of the time returned was user+kernel and not the
	wall-clock time.


changes since 1.4b
------------------

POSIX
	change release and version strings.

POSIX_File_Status
	fix bug in Is_Regular_File
	A file is regular under Windows if the attributes normal or archive is
	set. Patch sent by Sune Falck [sunef@hem.passagen.se].


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
