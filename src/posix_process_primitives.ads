
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Ada.Strings.Unbounded;

with POSIX;
with POSIX_IO;
with POSIX_Permissions;
with POSIX_Process_Environment;
with POSIX_Process_Identification;
with POSIX_Signals;

with Win32.Winbase;
with Win32.Winnt;

package POSIX_Process_Primitives is

   --  Process Template

   type Process_Template is limited private;

   procedure Open_Template  (Template : in out Process_Template);

   procedure Close_Template (Template : in out Process_Template);

   procedure Set_Keep_Effective_IDs
     (Template : in out Process_Template);

   procedure Set_Signal_Mask
     (Template : in out Process_Template;
      Mask     : in     POSIX_Signals.Signal_Set);

   procedure Set_Creation_Signal_Masking
     (Template       : in out Process_Template;
      Masked_Signals : in     POSIX.Signal_Masking
        := POSIX.RTS_Signals);

   procedure Set_File_Action_To_Close
     (Template : in out Process_Template;
      File     : in     POSIX_IO.File_Descriptor);

   procedure Set_File_Action_To_Open
     (Template : in out Process_Template;
      File     : in     POSIX_IO.File_Descriptor;
      Name     : in     POSIX.Pathname;
      Mode     : in     POSIX_IO.File_Mode
        := POSIX_IO.Read_Only;
      Options  : in     POSIX_IO.Open_Option_Set
        := POSIX_IO.Empty_Set);

   procedure Set_File_Action_To_Duplicate
     (Template  : in out Process_Template;
      File      : in     POSIX_IO.File_Descriptor;
      From_File : in     POSIX_IO.File_Descriptor);


   --  Process Creation

   procedure Start_Process
     (Child    :      out POSIX_Process_Identification.Process_ID;
      Pathname :   in     POSIX.Pathname;
      Template :   in     Process_Template;
      Arg_List :   in     POSIX.POSIX_String_List
        := POSIX.Empty_String_List);

   procedure Start_Process
     (Child    :      out POSIX_Process_Identification.Process_ID;
      Pathname :   in     POSIX.Pathname;
      Template :   in     Process_Template;
      Env_List :   in     POSIX_Process_Environment.Environment;
      Arg_List :   in     POSIX.POSIX_String_List
        := POSIX.Empty_String_List);

   procedure Start_Process_Search
     (Child    :      out POSIX_Process_Identification.Process_ID;
      Filename :   in     POSIX.Filename;
      Template :   in     Process_Template;
      Arg_List :   in     POSIX.POSIX_String_List
        := POSIX.Empty_String_List);

   procedure Start_Process_Search
     (Child    :      out POSIX_Process_Identification.Process_ID;
      Filename :   in     POSIX.Filename;
      Template :   in     Process_Template;
      Env_List :   in     POSIX_Process_Environment.Environment;
      Arg_List :   in     POSIX.POSIX_String_List
        := POSIX.Empty_String_List);



   --  Process Exit

   type Exit_Status is range 0 .. 2**8-1;

   Normal_Exit              : constant Exit_Status := 0;
   Failed_Creation_Exit     : constant Exit_Status := 41;
   Unhandled_Exception_Exit : constant Exit_Status := 42;

   procedure Exit_Process (Status : in Exit_Status := Normal_Exit);



   --  Termination Status

   type Termination_Status is private;
   type Termination_Cause is (Exited, Terminated_By_Signal, Stopped_By_Signal);

   function Status_Available (Status : Termination_Status)
                              return Boolean;

   function Process_ID_Of (Status : Termination_Status)
                           return POSIX_Process_Identification.Process_ID;

   function Termination_Cause_Of (Status : Termination_Status)
                                  return Termination_Cause;

   function Exit_Status_Of (Status : Termination_Status)
                            return Exit_Status;

   function Termination_Signal_Of (Status : Termination_Status)
                                   return POSIX_Signals.Signal;

   function Stopping_Signal_Of (Status : Termination_Status)
                                return POSIX_Signals.Signal;



   --  Wait for Process Termination

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Child          : in     POSIX_Process_Identification.Process_ID;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking
        := POSIX.RTS_Signals);
   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Group          : in     POSIX_Process_Identification.Process_Group_ID;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking
        := POSIX.RTS_Signals);

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking
        := POSIX.RTS_Signals);

private

   use Ada.Strings.Unbounded;
   use POSIX;
   use POSIX_Process_Identification;

   type File_Action is (Open, Close, Duplicate);

   type File_Request;
   type File_Request_Access is access File_Request;

   type File_Request (Action : File_Action) is
      record
         Next : File_Request_Access;
         File : POSIX_IO.File_Descriptor;
         case Action is
            when Open =>
               Name    : Unbounded_String;
               Mode    : POSIX_IO.File_Mode;
               Options : POSIX_IO.Open_Option_Set;
               OHandle : Win32.Winnt.HANDLE;
            when Close =>
               CHandle : Win32.Winnt.HANDLE;
            when Duplicate =>
               From_File : POSIX_IO.File_Descriptor;
         end case;
      end record;

   type Process_Template is
      record
         Is_Open                 : Boolean := False;
         Keep_Effective_IDs      : Boolean;
         Signal_Mask             : POSIX_Signals.Signal_Set;
         Signal_Creation_Masking : POSIX.Signal_Masking;
         File_Request_List       : File_Request_Access;
         Last_File_Request       : File_Request_Access;
         Process_Informations    : Win32.Winbase.LPPROCESS_INFORMATION;
      end record;

   type Exit_Stat is mod 2 ** Integer'Size;
   type Termination_Status is
      record
         Pid         :  Process_ID := Null_Process_ID;
         Exit_Status :  Exit_Stat;
      end record;

end POSIX_Process_Primitives;
