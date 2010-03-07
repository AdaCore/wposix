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

with Ada.Strings.Unbounded;

with POSIX.IO;
with POSIX.Process_Environment;
with POSIX.Process_Identification;
with POSIX.Signals;

private with Win32.Winbase;
private with Win32.Winnt;

package POSIX.Process_Primitives is

   --  Process Template

   type Process_Template is limited private;

   procedure Open_Template  (Template : in out Process_Template);

   procedure Close_Template (Template : in out Process_Template);

   procedure Set_Keep_Effective_IDs
     (Template : in out Process_Template);

   procedure Set_Signal_Mask
     (Template : in out Process_Template;
      Mask     :        POSIX.Signals.Signal_Set);

   procedure Set_Creation_Signal_Masking
     (Template       : in out Process_Template;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals);

   procedure Set_File_Action_To_Close
     (Template : in out Process_Template;
      File     :        POSIX.IO.File_Descriptor);

   procedure Set_File_Action_To_Open
     (Template : in out Process_Template;
      File     :        POSIX.IO.File_Descriptor;
      Name     :        POSIX.Pathname;
      Mode     :        POSIX.IO.File_Mode := POSIX.IO.Read_Only;
      Options  :        POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set);

   procedure Set_File_Action_To_Duplicate
     (Template  : in out Process_Template;
      File      :        POSIX.IO.File_Descriptor;
      From_File :        POSIX.IO.File_Descriptor);

   --  Process Creation

   procedure Start_Process
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Pathname :        POSIX.Pathname;
      Template :        Process_Template;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List);

   procedure Start_Process
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Pathname :        POSIX.Pathname;
      Template :        Process_Template;
      Env_List :        POSIX.Process_Environment.Environment;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List);

   procedure Start_Process_Search
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Filename :        POSIX.Filename;
      Template :        Process_Template;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List);

   procedure Start_Process_Search
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Filename :        POSIX.Filename;
      Template :        Process_Template;
      Env_List :        POSIX.Process_Environment.Environment;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List);

   --  Process Exit

   type Exit_Status is range 0 .. 2**8-1;

   Normal_Exit              : constant Exit_Status := 0;
   Failed_Creation_Exit     : constant Exit_Status := 41;
   Unhandled_Exception_Exit : constant Exit_Status := 42;

   procedure Exit_Process (Status : Exit_Status := Normal_Exit);

   --  Termination Status

   type Termination_Status is private;
   type Termination_Cause is (Exited, Terminated_By_Signal, Stopped_By_Signal);

   function Status_Available (Status : Termination_Status) return Boolean;

   function Process_ID_Of
     (Status : Termination_Status) return Process_Identification.Process_ID;

   function Termination_Cause_Of
     (Status : Termination_Status) return Termination_Cause;

   function Exit_Status_Of
     (Status : Termination_Status) return Exit_Status;

   function Termination_Signal_Of
     (Status : Termination_Status) return POSIX.Signals.Signal;

   function Stopping_Signal_Of
     (Status : Termination_Status) return POSIX.Signals.Signal;

   --  Wait for Process Termination

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Child          :        POSIX.Process_Identification.Process_ID;
      Block          :        Boolean := True;
      Trace_Stopped  :        Boolean := True;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals);

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Group          :        POSIX.Process_Identification.Process_Group_ID;
      Block          :        Boolean := True;
      Trace_Stopped  :        Boolean := True;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals);

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Block          :        Boolean := True;
      Trace_Stopped  :        Boolean := True;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals);

private

   use Ada.Strings.Unbounded;
   use POSIX;
   use POSIX.Process_Identification;

   type File_Action is (Open, Close, Duplicate);

   type File_Request;
   type File_Request_Access is access File_Request;

   type File_Request (Action : File_Action) is record
      Next : File_Request_Access;
      File : POSIX.IO.File_Descriptor;

      case Action is
         when Open =>
            Name    : Unbounded_String;
            Mode    : POSIX.IO.File_Mode;
            Options : POSIX.IO.Open_Option_Set;
            OHandle : Win32.Winnt.HANDLE;

         when Close =>
            CHandle : Win32.Winnt.HANDLE;

         when Duplicate =>
            From_File : POSIX.IO.File_Descriptor;
      end case;
   end record;

   type Process_Template is record
      Is_Open                 : Boolean := False;
      Keep_Effective_IDs      : Boolean;
      Signal_Mask             : POSIX.Signals.Signal_Set;
      Signal_Creation_Masking : POSIX.Signal_Masking;
      File_Request_List       : File_Request_Access;
      Last_File_Request       : File_Request_Access;
      Process_Informations    : Win32.Winbase.LPPROCESS_INFORMATION;
   end record;

   type Exit_Stat is mod 2 ** Integer'Size;

   type Termination_Status is record
      Pid         :  Process_ID := Null_Process_ID;
      Exit_Status :  Exit_Stat;
   end record;

end POSIX.Process_Primitives;
