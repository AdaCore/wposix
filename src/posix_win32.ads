------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with System;

with Win32;
with Win32.Winnt;

with POSIX;
with POSIX.Process_Identification;
with POSIX.Process_Primitives;
with POSIX.Process_Environment;

private package POSIX_Win32 is

   package PPP renames POSIX.Process_Primitives;
   package PPI renames POSIX.Process_Identification;
   package PPE renames POSIX.Process_Environment;

   use type Win32.DWORD;

   POSIX_Not_Yet_Implemented : exception;

   Retcode_Error : Win32.DWORD := -1;

   Users_SID : constant String := "S-1-5-32-545";
   --  This is the well-known "users" group which is available everywhere

   Everyone_SID : constant String := "S-1-1-0";
   --  This is the well-known "everyone" group which is available everywhere

   procedure Raise_Not_Yet_Implemented (Message : String) with No_Return;
   --  Raise POSIX_Not_Yet_Implemented exception with Message

   procedure Check_Retcode (RETCODE : Win32.DWORD; Fct : String) with Inline;
   --  Call Raise_Error with current error code if RETCODE is set to
   --  Retcode_Error.

   procedure Raise_Last_Error (Fct : String) with Inline;
   --  Raise an exception with the last error code

   procedure Check_Result (RETCODE : Win32.BOOL; Fct : String) with Inline;
   --  Call Raise_Error with current error code if RETCODE is False

   procedure Raise_Error
     (Message : String; Error_Code : POSIX.Error_Code) with No_Return;
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
   --  Convert a SID to a String

   function Length (Env : PPE.Environment) return Positive;
   --  Returns the size needed to create a Win32 environment block for Env

   procedure Set_Environment_Block (Block : out String; Env : PPE.Environment);
   --  Fill Block with a Win32 environment block representing Env

end POSIX_Win32;
