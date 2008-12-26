
$Id$

POSIX Ada Binding for Windows 95/98 and Windows NT/2000/XP
==========================================================

This is a partial implementation of POSIX using the Win32 API.
Version 1.13 beta

Thanks to Jean-Pierre Rosen who has made a lot of changes and make it possible
to have the 1.12 release out.

Thanks to Sune Falck for the help provided for version 1.4. It has been
reported that this binding compile fine with GNAT 3.11 and ObjectAda 7.1.2.

Thanks to Wilhelm Spickermann for a bug report.

Thanks to Frank Beard for his contribution.


This version has been tested with GNAT 3.14, GNAT 3.15.


changes since 1.12b
-------------------

POSIX.Process_Primitives
	Add support for Set_File_Action_To_Duplicate (only of standard files
	handles - stdin, stdout and stderr).

changes since 1.11b
-------------------

POSIX API should be thread safe. In many places this was not true before.

POSIX.Process_Primitives
	It is possible to launch .com and .exe (not only .exe as before).

POSIX.Calendar
	Handle milliseconds.

POSIX.Files
	Is_Symbolic_Link added.
	Is_Socket added.
	Both always return False on Win32.
	For_Every_Directory_Entry, check for pathname with directory separator.

POSIX.File_Status
	Get_File_Status new version should be better than before. Do not
	handle devices as this seems to be impossible on Win32.

POSIX.Process_Environment
	Environment_Value_Of correctly return Undefined if variable not found.

Reformat the code.

changes since 1.10b
-------------------

All POSIX packages are now child of POSIX. The old names are now renaming of
these packages. This is to conform to the latest Florist distribution under
UNIX.


changes since 1.9b
------------------

POSIX
	Complete implementation of System_Name, Node_Name, Release, Version
	and Machine [Frank Beard contribution]

POSIX_IO
	Fix wrong prototype for File_Position [reported by Frank Beard]

Remove many withed units throughout the binding sources that were not used.


changes since 1.8b
------------------

POSIX_Process_Primitives
	Fix bug in executable extension handling.

POSIX_Process_Identification
	Fix bug in Get_Login_Name


changes since 1.7b
------------------

POSIX_Process_Primitives
	Fix bug in Start_Process and Start_Process_Search

	Handle program without .exe. It is possible to launch foo or
	foo.exe. This makes the binding more compatible with UNIX.


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
