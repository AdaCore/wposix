
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with System;
with Win32;
with Win32.Winbase;
with Win32.Winnt;

with POSIX;
with POSIX_Process_Identification;
with POSIX_Process_Primitives;


package POSIX_Win32 is

   package PPP renames POSIX_Process_Primitives;
   package PPI renames POSIX_Process_Identification;

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

   --  helper functions for POSIX_Process_Primitives Wait_For_Child_Process
   procedure Add_Child    (Child  : in     PPI.Process_ID);
   procedure Remove_Child (Child  : in     PPI.Process_ID);
   function  Exist        (Child  : in     PPI.Process_ID) return Boolean;
   procedure Wait         (Status :    out PPP.Termination_Status;
                           Block  : in     Boolean);

end POSIX_Win32;
