
--  $Id$

with System;
with Win32;
with Win32.Winbase;
with Win32.Winnt;
with POSIX;

package POSIX_Win32 is

   use type Win32.INT;

   POSIX_Not_Yet_Implemented : exception;

   Retcode_Error : Win32.INT := -1;

   procedure Raise_Not_Yet_Implemented (Message : in String);

   procedure Check_Retcode (RETCODE : in Win32.INT;
                            Fct     : in String);

   procedure Check_Result (RETCODE : in Win32.BOOL;
                           Fct     : in String);

   procedure Raise_Error (Message    : in String;
                          Error_Code : in POSIX.Error_Code);

   function Is_Executable (Pathname : in POSIX.POSIX_String)
                           return Boolean;

   Null_Handle  : constant Win32.Winnt.HANDLE
     := Win32.Winnt.HANDLE (System.Null_Address);

end POSIX_Win32;
