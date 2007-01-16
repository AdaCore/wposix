
--  $Id$
--  Author : Pascal Obry
--  p.obry@wanadoo.fr

with System;
with Win32;
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
   pragma No_Return (Raise_Not_Yet_Implemented);
   --  Raise POSIX_Not_Yet_Implemented exception with Message.

   procedure Check_Retcode
     (RETCODE : in Win32.INT;
      Fct     : in String);
   --  Call Raise_Error with current error code if RETCODE is set to
   --  Retcode_Error.

   procedure Check_Result
     (RETCODE : in Win32.BOOL;
      Fct     : in String);
   --  Call Raise_Error with current error code if RETCODE is False.

   procedure Raise_Error
     (Message    : in String;
      Error_Code : in POSIX.Error_Code);
   pragma No_Return (Raise_Error);
   --  Raises POSIX_Error with Message and Error_Code.

   function Is_Executable
     (Pathname : in POSIX.POSIX_String)
     return Boolean;
   --  Returns true is Pathname is terminated by .com, .exe and .bat.

   Null_Handle  : constant Win32.Winnt.HANDLE
     := Win32.Winnt.HANDLE (System.Null_Address);

   --  helper functions for POSIX_Process_Primitives Wait_For_Child_Process

   procedure Add_Child
     (Child : in PPI.Process_ID);
   --  Add Child into the list of processes.

   procedure Remove_Child
     (Child : in PPI.Process_ID);
   --  Removes Child from the list of processes.

   function  Exist
     (Child : in PPI.Process_ID)
     return Boolean;
   --  Returns True if Child exists in the list of processes.

   procedure Wait
     (Status :    out PPP.Termination_Status;
      Block  : in     Boolean);
   --  Wait for a Child to terminate if Block is set to True and set
   --  Status. If Block is False it returns without waiting and Status
   --  containt the process ID and status. Null_Process_ID is returned if no
   --  process was terminated.

end POSIX_Win32;
