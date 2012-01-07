------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;

with POSIX_Win32.File_Handle;

package body POSIX.Process_Primitives is

   use POSIX;
   use type Win32.ULONG;

   ---------------------------------------
   -- Process_ID_To_PROCESS_INFORMATION --
   ---------------------------------------

   function Process_ID_To_PROCESS_INFORMATION is new Ada.Unchecked_Conversion
       (Process_Identification.Process_ID, Win32.Winbase.PROCESS_INFORMATION);

   ---------------------------------------
   -- PROCESS_INFORMATION_To_Process_ID --
   ---------------------------------------

   function PROCESS_INFORMATION_To_Process_ID is new Ada.Unchecked_Conversion
     (Win32.Winbase.PROCESS_INFORMATION, Process_Identification.Process_ID);

   Security_Handles_Inherited : aliased Win32.Winbase.SECURITY_ATTRIBUTES :=
                                  (Win32.Winbase.SECURITY_ATTRIBUTES'Size / 8,
                                   System.Null_Address, Win32.TRUE);

   procedure Check_Open
     (Template : Process_Template;
      Message  : String);
   --  ???

   procedure Check_File (File : POSIX.IO.File_Descriptor);
   --  ???

   procedure Insert
     (Request_Access :        File_Request_Access;
      Into           : in out Process_Template);
   --  ???

   procedure Execute_Template
     (Template :        Process_Template;
      SI       : in out Win32.Winbase.STARTUPINFO);
   --  ???

   procedure Start_Process
     (Child       :    out POSIX.Process_Identification.Process_ID;
      Pathname    :        POSIX.Pathname;
      Template    :        Process_Template;
      Env_List    :        POSIX.Process_Environment.Environment;
      Arg_List    :        POSIX.POSIX_String_List := POSIX.Empty_String_List;
      Environment :        Boolean);
   --  ???

   ----------------
   -- Check_File --
   ----------------

   procedure Check_File (File : POSIX.IO.File_Descriptor) is
   begin
      case File is
         when POSIX.IO.Standard_Input |
              POSIX.IO.Standard_Output |
              POSIX.IO.Standard_Error =>
            null;
         when others =>
            POSIX_Win32.Raise_Error
              ("(POSIX implementation restriction) Check_File",
               Invalid_Argument);
      end case;
   end Check_File;

   ----------------
   -- Check_Open --
   ----------------

   procedure Check_Open
     (Template : Process_Template;
      Message  : String) is
   begin
      if not Template.Is_Open then
         POSIX_Win32.Raise_Error (Message, Invalid_Argument);
      end if;
   end Check_Open;

   --------------------
   -- Close_Template --
   --------------------

   procedure Close_Template (Template : in out Process_Template) is

      P      : File_Request_Access;
      Result : Win32.BOOL;
      pragma Unreferenced (Result);

      procedure Free is new Ada.Unchecked_Deallocation
        (File_Request, File_Request_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Win32.Winbase.PROCESS_INFORMATION,
         Win32.Winbase.LPPROCESS_INFORMATION);

   begin
      Check_Open (Template, "Close_Template");

      while Template.File_Request_List /= null loop
         P := Template.File_Request_List;

         case P.Action is

            when Open =>
               case P.File is
                  when POSIX.IO.Standard_Input |
                       POSIX.IO.Standard_Output |
                       POSIX.IO.Standard_Error =>
                     Result := Win32.Winbase.CloseHandle (P.OHandle);
                  when others =>
                     null; -- not handled
               end case;

            when Close =>
               case P.File is
                  when POSIX.IO.Standard_Input |
                       POSIX.IO.Standard_Output |
                       POSIX.IO.Standard_Error =>
                     Result := Win32.Winbase.CloseHandle (P.CHandle);
                  when others =>
                     null; -- not handled
               end case;

            when Duplicate =>
               null;
         end case;

         Template.File_Request_List := P.Next;
         Free (P);
      end loop;

      Template.Is_Open := False;

      --  Close the handle of the Process and Primary Thread

      Result := Win32.Winbase.CloseHandle
        (Template.Process_Informations.hProcess);

      Result := Win32.Winbase.CloseHandle
        (Template.Process_Informations.hThread);

      Free (Template.Process_Informations);
   end Close_Template;

   ----------------------
   -- Execute_Template --
   ----------------------

   procedure Execute_Template
     (Template :        Process_Template;
      SI       : in out Win32.Winbase.STARTUPINFO)
   is

      Null_Filename : constant String := "nul";

      P             : File_Request_Access;

      function Mode_To_File_Access
        (Mode : POSIX.IO.File_Mode) return Win32.DWORD;
      --  ???

      function Create_File
        (Name : String;
         Mode : POSIX.IO.File_Mode) return Win32.Winnt.HANDLE;
      --  ???

      function Open_File
        (Name : String;
         Mode : POSIX.IO.File_Mode) return Win32.Winnt.HANDLE;
      --  ???

      -----------------
      -- Create_File --
      -----------------

      function Create_File
        (Name : String;
         Mode : POSIX.IO.File_Mode) return Win32.Winnt.HANDLE
      is
         use type Win32.INT;
         use type System.Address;
         L_Name : constant String := Name & ASCII.NUL;
         H      : Win32.Winnt.HANDLE;
      begin
         H := Win32.Winbase.CreateFile
           (Win32.Addr (L_Name),
            Mode_To_File_Access (Mode),
            Win32.Winnt.FILE_SHARE_WRITE,
            Security_Handles_Inherited'Access,
            Win32.Winbase.CREATE_ALWAYS,
            Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
            POSIX_Win32.Null_Handle);

         if H = Win32.Winbase.INVALID_HANDLE_VALUE then
            POSIX_Win32.Raise_Last_Error ("Template : Create_File " & Name);
         end if;
         return H;
      end Create_File;

      -------------------------
      -- Mode_To_File_Access --
      -------------------------

      function Mode_To_File_Access
        (Mode : POSIX.IO.File_Mode) return Win32.DWORD is
      begin
         case Mode is
            when POSIX.IO.Read_Only =>
               return Win32.Winnt.GENERIC_READ;
            when  POSIX.IO.Write_Only | POSIX.IO.Read_Write =>
               return Win32.Winnt.GENERIC_READ + Win32.Winnt.GENERIC_WRITE;
         end case;
      end Mode_To_File_Access;

      ---------------
      -- Open_File --
      ---------------

      function Open_File
        (Name : String;
         Mode : POSIX.IO.File_Mode) return Win32.Winnt.HANDLE
      is
         use type Win32.INT;
         use type System.Address;
         L_Name : constant String := Name & ASCII.NUL;
         H      : Win32.Winnt.HANDLE;
      begin
         H := Win32.Winbase.CreateFile
           (Win32.Addr (L_Name),
            Mode_To_File_Access (Mode),
            Win32.Winnt.FILE_SHARE_READ,
            Security_Handles_Inherited'Access,
            Win32.Winbase.OPEN_EXISTING,
            Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
            POSIX_Win32.Null_Handle);

         if H = Win32.Winbase.INVALID_HANDLE_VALUE then
            POSIX_Win32.Raise_Last_Error ("Template : Open_File " & Name);
         end if;
         return H;
      end Open_File;

   begin
      if not Template.Keep_Effective_IDs then
         Set_User_ID (Get_Real_User_ID);
         Set_Group_ID (Get_Real_Group_ID);
      end if;
      --  POSIX.Signals.Set_Blocked_Signals (Template.Signal_Mask, Junk1);

      P := Template.File_Request_List;

      while P /= null loop

         case P.Action is

            when Open =>
               case P.File is
                  when POSIX.IO.Standard_Input =>
                     SI.hStdInput := Open_File (To_String (P.Name), P.Mode);
                     P.OHandle := SI.hStdInput;

                  when POSIX.IO.Standard_Output =>
                     SI.hStdOutput := Create_File (To_String (P.Name), P.Mode);
                     P.OHandle := SI.hStdOutput;

                  when POSIX.IO.Standard_Error =>
                     SI.hStdError := Create_File (To_String (P.Name), P.Mode);
                     P.OHandle := SI.hStdError;

                  when others =>
                     POSIX_Win32.Raise_Not_Yet_Implemented
                       ("Execute Template Open for non Std file");
               end case;

            when Close =>
               case P.File is
                  when POSIX.IO.Standard_Input =>
                     SI.hStdInput := Open_File
                       (Null_Filename, POSIX.IO.Read_Only);
                     P.CHandle := SI.hStdInput;

                  when POSIX.IO.Standard_Output =>
                     SI.hStdOutput := Create_File
                       (Null_Filename, POSIX.IO.Write_Only);
                     P.CHandle := SI.hStdOutput;

                  when POSIX.IO.Standard_Error =>
                     SI.hStdError := Create_File
                       (Null_Filename, POSIX.IO.Write_Only);
                     P.CHandle := SI.hStdError;

                  when others =>
                     POSIX_Win32.Raise_Not_Yet_Implemented
                       ("Execute Template Close for non Std file");
               end case;

            when Duplicate =>
               case P.File is
                  when POSIX.IO.Standard_Input =>
                     SI.hStdInput
                       := POSIX_Win32.File_Handle.Get (P.From_File);

                  when POSIX.IO.Standard_Output =>
                     SI.hStdOutput
                       := POSIX_Win32.File_Handle.Get (P.From_File);

                  when POSIX.IO.Standard_Error =>
                     SI.hStdError := POSIX_Win32.File_Handle.Get (P.From_File);

                  when others =>
                     POSIX_Win32.Raise_Not_Yet_Implemented
                       ("Execute Template Duplicate for non Std file");
               end case;
         end case;
         P := P.Next;
      end loop;
   end Execute_Template;

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process (Status : Exit_Status := Normal_Exit) is
   begin
      Win32.Winbase.ExitProcess (Win32.UINT (Status));
   end Exit_Process;

   --------------------
   -- Exit_Status_Of --
   --------------------

   function Exit_Status_Of
     (Status : Termination_Status) return Exit_Status is
   begin
      if not Status_Available (Status)
        or else Termination_Cause_Of (Status) /= Exited
      then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Exit_Status (Status.Exit_Status);
   end Exit_Status_Of;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Request_Access :        File_Request_Access;
      Into           : in out Process_Template) is
   begin
      if Into.File_Request_List = null then
         Into.File_Request_List := Request_Access;
         Into.Last_File_Request := Request_Access;
      else
         Into.Last_File_Request.Next := Request_Access;
         Into.Last_File_Request      := Request_Access;
      end if;
   end Insert;

   -------------------
   -- Open_Template --
   -------------------

   procedure Open_Template (Template : in out Process_Template) is
   begin
      if Template.Is_Open then
         Close_Template (Template);
      end if;

      Template.Keep_Effective_IDs := False;
      --  POSIX.Signals.Delete_All_Signals (Template.Signal_Mask);
      Template.Is_Open := True;
      Template.Process_Informations := new
        Win32.Winbase.PROCESS_INFORMATION;
      Template.File_Request_List := null;
      Template.Last_File_Request := null;
   end Open_Template;

   -------------------
   -- Process_ID_Of --
   -------------------

   function Process_ID_Of (Status : Termination_Status) return Process_ID is
   begin
      if not Status_Available (Status) then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Status.Pid;
   end  Process_ID_Of;

   ---------------------------------
   -- Set_Creation_Signal_Masking --
   ---------------------------------

   procedure Set_Creation_Signal_Masking
     (Template       : in out Process_Template;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      Check_Open (Template, "Set_Creation_Signal_Masking");
      Template.Signal_Creation_Masking := Masked_Signals;
   end Set_Creation_Signal_Masking;

   ------------------------------
   -- Set_File_Action_To_Close --
   ------------------------------

   procedure Set_File_Action_To_Close
     (Template : in out Process_Template;
      File     :        POSIX.IO.File_Descriptor)
   is
      New_Action : File_Request_Access;
   begin
      Check_Open (Template, "Set_File_Action_To_Close");
      New_Action := new File_Request'
        (Next    => null,
         File    => File,
         Action  => Close,
         CHandle => POSIX_Win32.Null_Handle);
      Insert (New_Action, Into => Template);
   end Set_File_Action_To_Close;

   ----------------------------------
   -- Set_File_Action_To_Duplicate --
   ----------------------------------

   procedure Set_File_Action_To_Duplicate
     (Template  : in out Process_Template;
      File      :        POSIX.IO.File_Descriptor;
      From_File :        POSIX.IO.File_Descriptor)
   is
      New_Action : File_Request_Access;
   begin
      Check_Open (Template, "Set_File_Action_To_Duplicate");
      New_Action := new File_Request'
        (Next      => null,
         File      => File,
         Action    => Duplicate,
         From_File => From_File);
      Insert (New_Action, Into => Template);
   end Set_File_Action_To_Duplicate;

   -----------------------------
   -- Set_File_Action_To_Open --
   -----------------------------

   procedure Set_File_Action_To_Open
     (Template : in out Process_Template;
      File     :        POSIX.IO.File_Descriptor;
      Name     :        POSIX.Pathname;
      Mode     :        POSIX.IO.File_Mode       := POSIX.IO.Read_Only;
      Options  :        POSIX.IO.Open_Option_Set := POSIX.IO.Empty_Set)
   is
      New_Action : File_Request_Access;
   begin
      Check_Open (Template, "Set_File_Action_To_Open");
      Check_File (File);
      New_Action := new File_Request'
        (Next    => null,
         File    => File,
         Action  => Open,
         Name    => To_Unbounded_String (POSIX.To_String (Name)),
         Mode    => Mode,
         Options => Options,
         OHandle => POSIX_Win32.Null_Handle);
      Insert (New_Action, Into => Template);
   end Set_File_Action_To_Open;

   ----------------------------
   -- Set_Keep_Effective_IDs --
   ----------------------------

   procedure Set_Keep_Effective_IDs (Template : in out Process_Template) is
   begin
      Check_Open (Template, "Set_Keep_Effective_IDs");
      Template.Keep_Effective_IDs := True;
   end Set_Keep_Effective_IDs;

   ---------------------
   -- Set_Signal_Mask --
   ---------------------

   procedure Set_Signal_Mask
     (Template : in out Process_Template;
      Mask     :        POSIX.Signals.Signal_Set) is
   begin
      Check_Open (Template, "Set_Signal_Mask");
      Template.Signal_Mask := Mask;
   end Set_Signal_Mask;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Child       :    out POSIX.Process_Identification.Process_ID;
      Pathname    :        POSIX.Pathname;
      Template    :        Process_Template;
      Env_List    :        POSIX.Process_Environment.Environment;
      Arg_List    :        POSIX.POSIX_String_List := POSIX.Empty_String_List;
      Environment :        Boolean)
   is
      use type Win32.BOOL;

      Env                  : String (1 .. POSIX_Win32.Length (Env_List));
      --  The Win32 environment block

      Startup_Informations : aliased Win32.Winbase.STARTUPINFO;
      Process_Informations : aliased Win32.Winbase.PROCESS_INFORMATION;

      Result : Win32.BOOL;

      Arguments      : Unbounded_String;
      Argument_Count : Natural := 0;

      procedure Concat
        (Item :        POSIX_String;
         Quit : in out Boolean);
      --  ???

      ------------
      -- Concat --
      ------------

      procedure Concat
        (Item :        POSIX_String;
         Quit : in out Boolean) is
      begin
         --  the first argument is by convention the program name and we don't
         --  want to add it to the command line.
         if Argument_Count = 0 then
            Arguments := To_Unbounded_String (POSIX.To_String (Pathname));
         else
            Append (Arguments, ' ' & POSIX.To_String (Item));
         end if;

         Argument_Count := Argument_Count + 1;
         Quit := False;
      end Concat;

      --------------------------
      -- Concat_All_Arguments --
      --------------------------

      procedure Concat_All_Arguments is new POSIX.For_Every_Item (Concat);

      Env_Pointer : Win32.LPVOID;

   begin
      Check_Open (Template, "Start_Process");

      Startup_Informations.cb          := Win32.Winbase.STARTUPINFO'Size / 8;
      Startup_Informations.lpReserved  := null;
      Startup_Informations.lpDesktop   := null;
      Startup_Informations.lpTitle     := null;
      Startup_Informations.dwFlags     := Win32.Winbase.STARTF_USESTDHANDLES;
      Startup_Informations.cbReserved2 := 0;
      Startup_Informations.lpReserved2 := null;

      --  Initialize standard file handles as we inherit them

      Startup_Informations.hStdInput
        := POSIX_Win32.File_Handle.Get (POSIX.IO.Standard_Input);

      Startup_Informations.hStdOutput
        := POSIX_Win32.File_Handle.Get (POSIX.IO.Standard_Output);

      Startup_Informations.hStdError
        := POSIX_Win32.File_Handle.Get (POSIX.IO.Standard_Error);

      Execute_Template (Template, Startup_Informations);

      Concat_All_Arguments (Arg_List);

      if Environment then
         POSIX_Win32.Set_Environment_Block (Env, Env_List);
         Env_Pointer := Env (Env'First)'Address;
      else
         Env_Pointer := System.Null_Address;
      end if;

      Run_Process : declare
         use Win32.Winbase;
         Args : constant String := To_String (Arguments) & ASCII.NUL;
      begin
         Result := CreateProcess
           (lpApplicationName    => null,
            lpCommandLine        => Win32.Addr (Args),
            lpProcessAttributes  => Security_Handles_Inherited'Access,
            lpThreadAttributes   => null,
            bInheritHandles      => Win32.TRUE,
            dwCreationFlags      => NORMAL_PRIORITY_CLASS,
            lpEnvironment        => Env_Pointer,
            lpCurrentDirectory   => null,
            lpStartupInfo        => Startup_Informations'Unchecked_Access,
            lpProcessInformation => Process_Informations'Unchecked_Access
            );
      end Run_Process;

      if Result = Win32.FALSE then
         POSIX_Win32.Raise_Error ("Create_Process", POSIX.Not_Enough_Space);
      end if;

      Template.Process_Informations.all := Process_Informations;
      Child := PROCESS_INFORMATION_To_Process_ID (Process_Informations);

      --  record all child process for Wait_For_Child_Process functions
      POSIX_Win32.Add_Child (Child);
   end Start_Process;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Pathname :        POSIX.Pathname;
      Template :        Process_Template;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
      Null_Environment : POSIX.Process_Environment.Environment;
   begin
      Start_Process (Child, Pathname, Template,
                     Null_Environment, Arg_List,
                     Environment => False);
   end Start_Process;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Pathname :        POSIX.Pathname;
      Template :        Process_Template;
      Env_List :        POSIX.Process_Environment.Environment;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List) is
   begin
      Start_Process
        (Child, Pathname, Template, Env_List, Arg_List, Environment => True);
   end Start_Process;

   --------------------------
   -- Start_Process_Search --
   --------------------------

   procedure Start_Process_Search
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Filename :        POSIX.Filename;
      Template :        Process_Template;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
      Null_Environment : POSIX.Process_Environment.Environment;
      Max_Len    : constant := 500;
      Pathname   : String (1 .. Max_Len) := (others => '.');
      pragma Warnings (Off, Pathname);

      L_Filename : constant String := POSIX.To_String (Filename) & ASCII.NUL;
      Ext_Var    : constant String := ".exe" & ASCII.NUL;
      lpFilePart : aliased Win32.LPSTR;

      Result     : Win32.DWORD;
   begin
      Check_Open (Template, "Start_Process_Search");

      Result := Win32.Winbase.SearchPath
        (null,
         Win32.Addr (L_Filename),
         Win32.Addr (Ext_Var),
         Win32.DWORD (Pathname'Length),
         Win32.Addr (Pathname),
         lpFilePart'Unchecked_Access);

      if Result = 0 then
         Start_Process
           (Child, Filename, Template, Null_Environment, Arg_List,
            Environment => False);
      else
         Start_Process
           (Child,
            POSIX.To_POSIX_String (Pathname (1 .. Positive (Result))),
            Template, Null_Environment, Arg_List,
            Environment => False);
      end if;
   end Start_Process_Search;

   --------------------------
   -- Start_Process_Search --
   --------------------------

   procedure Start_Process_Search
     (Child    :    out POSIX.Process_Identification.Process_ID;
      Filename :        POSIX.Filename;
      Template :        Process_Template;
      Env_List :        POSIX.Process_Environment.Environment;
      Arg_List :        POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
      Max_Len    : constant := 500;
      Pathname   : String (1 .. Max_Len);
      pragma Warnings (Off, Pathname);

      L_Filename : constant String := POSIX.To_String (Filename) & ASCII.NUL;
      Ext_Var    : constant String := ".exe" & ASCII.NUL;
      lpFilePart : aliased Win32.LPSTR;
      Result     : Win32.DWORD;
   begin
      Check_Open (Template, "Start_Process_Search");

      Search_Filename_In_Env : declare
         Old_Environment : POSIX.Process_Environment.Environment;
      begin
         POSIX.Process_Environment.Copy_From_Current_Environment
           (Old_Environment);

         POSIX.Process_Environment.Copy_To_Current_Environment
           (Env_List);

         Result := Win32.Winbase.SearchPath
           (null,
            Win32.Addr (L_Filename),
            Win32.Addr (Ext_Var),
            Win32.DWORD (Pathname'Length),
            Win32.Addr (Pathname),
            lpFilePart'Unchecked_Access);

         POSIX.Process_Environment.Copy_To_Current_Environment
           (Old_Environment);
      end Search_Filename_In_Env;

      if Result = 0 then
         Start_Process
           (Child, Filename, Template, Env_List, Arg_List,
            Environment => True);
      else
         Start_Process
           (Child,
            POSIX.To_POSIX_String (Pathname (1 .. Positive (Result))),
            Template, Env_List, Arg_List,
            Environment => True);
      end if;
   end Start_Process_Search;

   ----------------------
   -- Status_Available --
   ----------------------

   function Status_Available (Status : Termination_Status) return Boolean is
   begin
      return Status.Pid /= Null_Process_ID;
   end Status_Available;

   ------------------------
   -- Stopping_Signal_Of --
   ------------------------

   function Stopping_Signal_Of
     (Status : Termination_Status) return POSIX.Signals.Signal is
   begin
      if not Status_Available (Status)
        or else Termination_Cause_Of (Status) /= Stopped_By_Signal
      then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return POSIX.Signals.Signal_Abort;
   end Stopping_Signal_Of;

   --------------------------
   -- Termination_Cause_Of --
   --------------------------

   function Termination_Cause_Of
     (Status : Termination_Status) return Termination_Cause is
   begin
      if not Status_Available (Status) then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Exited;
   end Termination_Cause_Of;

   ---------------------------
   -- Termination_Signal_Of --
   ---------------------------

   function Termination_Signal_Of
     (Status : Termination_Status) return POSIX.Signals.Signal is
   begin
      if not Status_Available (Status)
        or else Termination_Cause_Of (Status) /= Terminated_By_Signal
      then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return POSIX.Signals.Signal_Terminate;
   end Termination_Signal_Of;

   ----------------------------
   -- Wait_For_Child_Process --
   ----------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Child          :        POSIX.Process_Identification.Process_ID;
      Block          :        Boolean := True;
      Trace_Stopped  :        Boolean := True;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      pragma Warnings (Off, Trace_Stopped);
      pragma Warnings (Off, Masked_Signals);

      Retcode  : Win32.DWORD;
      Result   : Win32.BOOL;
      pragma Unreferenced (Result);
      Process_Informations : Win32.Winbase.PROCESS_INFORMATION;

      Process_Status : aliased Win32.DWORD;

   begin
      if not POSIX_Win32.Exist (Child) then
         POSIX_Win32.Raise_Error ("No Child Process", POSIX.No_Child_Process);
      end if;

      Process_Informations := Process_ID_To_PROCESS_INFORMATION (Child);

      if Child = Null_Process_ID then
         Set_Error_Code (No_Child_Process);
         raise POSIX_Error;
      elsif Process_Informations.dwProcessId = -1 then
         Status.Exit_Status := Exit_Stat (Failed_Creation_Exit);
         return;
      end if;

      if Block then
         Retcode := Win32.Winbase.WaitForSingleObject
           (Process_Informations.hProcess, Win32.Winbase.INFINITE);
      else
         Retcode := Win32.Winbase.WaitForSingleObject
           (Process_Informations.hProcess, 0);
         if Retcode = Win32.Winbase.WAIT_TIMEOUT then
            Status := Termination_Status'(Null_Process_ID, 0);
            return;
         end if;
      end if;

      Status.Pid := Child;

      Result := Win32.Winbase.GetExitCodeProcess
        (Process_Informations.hProcess,
         Process_Status'Unchecked_Access);
      Status.Exit_Status := Exit_Stat (Process_Status);

      --  remove child from the list of child process
      POSIX_Win32.Remove_Child (Child);
   end Wait_For_Child_Process;

   ----------------------------
   -- Wait_For_Child_Process --
   ----------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Group          :        POSIX.Process_Identification.Process_Group_ID;
      Block          :        Boolean := True;
      Trace_Stopped  :        Boolean := True;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      pragma Warnings (Off, Group);
      pragma Warnings (Off, Trace_Stopped);
      pragma Warnings (Off, Masked_Signals);
   begin
      POSIX_Win32.Wait (Status, Block);
   end Wait_For_Child_Process;

   ----------------------------
   -- Wait_For_Child_Process --
   ----------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Block          :        Boolean := True;
      Trace_Stopped  :        Boolean := True;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      pragma Warnings (Off, Trace_Stopped);
      pragma Warnings (Off, Masked_Signals);
   begin
      POSIX_Win32.Wait (Status, Block);
   end Wait_For_Child_Process;

end POSIX.Process_Primitives;
