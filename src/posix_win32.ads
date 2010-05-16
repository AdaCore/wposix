------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with System;

with Win32;
with Win32.Winnt;

with POSIX;
with POSIX.Process_Identification;
with POSIX.Process_Primitives;

private package POSIX_Win32 is

   package PPP renames POSIX.Process_Primitives;
   package PPI renames POSIX.Process_Identification;

   use type Win32.DWORD;

   POSIX_Not_Yet_Implemented : exception;

   Retcode_Error : Win32.DWORD := -1;

   procedure Raise_Not_Yet_Implemented (Message : String);
   pragma No_Return (Raise_Not_Yet_Implemented);
   --  Raise POSIX_Not_Yet_Implemented exception with Message

   procedure Check_Retcode (RETCODE : Win32.DWORD; Fct : String);
   pragma Inline (Check_Retcode);
   --  Call Raise_Error with current error code if RETCODE is set to
   --  Retcode_Error.

   procedure Raise_Last_Error (Fct : String);
   pragma Inline (Raise_Last_Error);
   --  Raise an exception with the last error code

   procedure Check_Result (RETCODE : Win32.BOOL; Fct : String);
   pragma Inline (Check_Result);
   --  Call Raise_Error with current error code if RETCODE is False

   procedure Raise_Error (Message : String; Error_Code : POSIX.Error_Code);
   pragma No_Return (Raise_Error);
   --  Raises POSIX_Error with Message and Error_Code

   function Is_Executable (Pathname : POSIX.POSIX_String) return Boolean;
   --  Returns true is Pathname is terminated by .com, .exe and .bat

   Null_Handle : constant Win32.Winnt.HANDLE :=
                   Win32.Winnt.HANDLE (System.Null_Address);

   --  helper functions for POSIX_Process_Primitives Wait_For_Child_Process

   procedure Add_Child (Child : PPI.Process_ID);
   --  Add Child into the list of processes

   procedure Remove_Child (Child : PPI.Process_ID);
   --  Removes Child from the list of processes

   function Exist (Child : PPI.Process_ID) return Boolean;
   --  Returns True if Child exists in the list of processes

   procedure Wait
     (Status :    out PPP.Termination_Status;
      Block  :        Boolean);
   --  Wait for a Child to terminate if Block is set to True and set
   --  Status. If Block is False it returns without waiting and Status
   --  containt the process ID and status. Null_Process_ID is returned if no
   --  process was terminated.

   function Get_Process_Handle
     (Process : PPI.Process_ID) return Win32.Winnt.HANDLE;
   --  Returns Win32 Handle for the given process

   function To_String (SID : Win32.Winnt.PSID) return String;
   --  Convert a SID to a

end POSIX_Win32;
