
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;

with POSIX_Win32;
with Win32;

package body POSIX_Process_Primitives is

   use POSIX;
   use type Win32.ULONG;

   ---------------------------------------
   -- Process_ID_To_PROCESS_INFORMATION --
   ---------------------------------------

   function Process_ID_To_PROCESS_INFORMATION is
      new Ada.Unchecked_Conversion (POSIX_Process_Identification.Process_ID,
                                    Win32.Winbase.PROCESS_INFORMATION);

   ---------------------------------------
   -- PROCESS_INFORMATION_To_Process_ID --
   ---------------------------------------

   function PROCESS_INFORMATION_To_Process_ID is
      new Ada.Unchecked_Conversion (Win32.Winbase.PROCESS_INFORMATION,
                                    POSIX_Process_Identification.Process_ID);

   Security_Handles_Inherited : aliased Win32.Winbase.SECURITY_ATTRIBUTES
     := (Win32.Winbase.SECURITY_ATTRIBUTES'Size / 8,
         System.Null_Address,
         Win32.TRUE);

   --  Process Template

   -------------------
   -- Open_Template --
   -------------------

   procedure Open_Template (Template : in out Process_Template) is
   begin
      if Template.Is_Open then
         Close_Template (Template);
      end if;
      Template.Keep_Effective_IDs := False;
      --  POSIX_Signals.Delete_All_Signals (Template.Signal_Mask);
      Template.Is_Open := True;
      Template.Process_Informations := new
        Win32.Winbase.PROCESS_INFORMATION;
      Template.File_Request_List := null;
      Template.Last_File_Request := null;
   end Open_Template;


   ----------------
   -- Check_Open --
   ----------------

   procedure Check_Open (Template : in Process_Template;
                         Message  : in String) is
   begin
      if not Template.Is_Open then
         POSIX_Win32.Raise_Error (Message, Invalid_Argument);
      end if;
   end Check_Open;


   ----------------
   -- Check_File --
   ----------------

   procedure Check_File (File : in POSIX_IO.File_Descriptor) is
   begin
      case File is
         when POSIX_IO.Standard_Input |
           POSIX_IO.Standard_Output |
           POSIX_IO.Standard_Error =>
            null;
         when others =>
            POSIX_Win32.Raise_Error
              ("(POSIX implementation restriction) Check_File",
               Invalid_Argument);
      end case;
   end Check_File;


   --------------------
   -- Close_Template --
   --------------------

   procedure Close_Template (Template : in out Process_Template) is

      P      : File_Request_Access;
      Result : Win32.BOOL;

      procedure Free is
        new Ada.Unchecked_Deallocation (File_Request, File_Request_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation (Win32.Winbase.PROCESS_INFORMATION,
                                        Win32.Winbase.LPPROCESS_INFORMATION);


   begin
      Check_Open (Template, "Close_Template");

      while Template.File_Request_List /= null loop
         P := Template.File_Request_List;

         case P.Action is

            when Open =>
               case P.File is
                  when
                    POSIX_IO.Standard_Input |
                    POSIX_IO.Standard_Output |
                    POSIX_IO.Standard_Error =>
                     Result := Win32.Winbase.CloseHandle (P.OHandle);
                  when others =>
                     null; -- not handled
               end case;

            when Close =>
               case P.File is
                  when
                    POSIX_IO.Standard_Input |
                    POSIX_IO.Standard_Output |
                    POSIX_IO.Standard_Error =>
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

      --  close the handle of the Process and Primary Thread.
      Result := Win32.Winbase.CloseHandle
        (Template.Process_Informations.hProcess);

      Result := Win32.Winbase.CloseHandle
        (Template.Process_Informations.hThread);

      Free (Template.Process_Informations);
   end Close_Template;


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
      Mask     : in     POSIX_Signals.Signal_Set) is
   begin
      Check_Open (Template, "Set_Signal_Mask");
      Template.Signal_Mask := Mask;
   end Set_Signal_Mask;


   ---------------------------------
   -- Set_Creation_Signal_Masking --
   ---------------------------------

   procedure Set_Creation_Signal_Masking
     (Template       : in out Process_Template;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      Check_Open (Template, "Set_Creation_Signal_Masking");
      Template.Signal_Creation_Masking := Masked_Signals;
   end Set_Creation_Signal_Masking;


   ------------
   -- Insert --
   ------------

   procedure Insert (Request_Access : in     File_Request_Access;
                     Into           : in out Process_Template)
   is
   begin
      if Into.File_Request_List = null then
         Into.File_Request_List := Request_Access;
         Into.Last_File_Request := Request_Access;
      else
         Into.Last_File_Request.Next := Request_Access;
         Into.Last_File_Request      := Request_Access;
      end if;
   end Insert;

   -----------------------------
   -- Set_File_Action_To_Open --
   -----------------------------

   procedure Set_File_Action_To_Open
     (Template : in out Process_Template;
      File     : in     POSIX_IO.File_Descriptor;
      Name     : in     POSIX.Pathname;
      Mode     : in     POSIX_IO.File_Mode       := POSIX_IO.Read_Only;
      Options  : in     POSIX_IO.Open_Option_Set := POSIX_IO.Empty_Set)
   is
      New_Action : File_Request_Access;
   begin
      Check_Open (Template, "Set_File_Action_To_Open");
      Check_File (File);
      New_Action :=
       new File_Request'(Next    => null,
                         File    => File,
                         Action  => Open,
                         Name    => To_Unbounded_String
                                     (POSIX.To_String (Name)),
                         Mode    => Mode,
                         Options => Options,
                         OHandle => POSIX_Win32.Null_Handle);
      Insert (New_Action, Into => Template);
   end Set_File_Action_To_Open;


   ------------------------------
   -- Set_File_Action_To_Close --
   ------------------------------

   procedure Set_File_Action_To_Close
     (Template : in out Process_Template;
      File     : in     POSIX_IO.File_Descriptor)
   is
      New_Action : File_Request_Access;
   begin
      Check_Open (Template, "Set_File_Action_To_Close");
      New_Action := new File_Request'(Next    => null,
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
      File      : in     POSIX_IO.File_Descriptor;
      From_File : in     POSIX_IO.File_Descriptor)
   is
      New_Action : File_Request_Access;
   begin
      Check_Open (Template, "Set_File_Action_To_Duplicate");
      New_Action :=
       new File_Request'(Next      => null,
                         File      => File,
                         Action    => Duplicate,
                         From_File => From_File);
      Insert (New_Action, Into => Template);
   end Set_File_Action_To_Duplicate;


   ----------------------
   -- Execute_Template --
   ----------------------

   procedure Execute_Template (Template : in     Process_Template;
                               SI       : in out Win32.Winbase.STARTUPINFO)
   is

      Null_Filename : constant String := "nul";

      P             : File_Request_Access;

      -------------------------
      -- Mode_To_File_Access --
      -------------------------

      function Mode_To_File_Access (Mode : in POSIX_IO.File_Mode)
                                    return Win32.DWORD
      is
         use type Win32.ULONG;
      begin
         case Mode is
            when POSIX_IO.Read_Only =>
               return Win32.Winnt.GENERIC_READ;
            when  POSIX_IO.Write_Only | POSIX_IO.Read_Write =>
               return Win32.Winnt.GENERIC_READ + Win32.Winnt.GENERIC_WRITE;
         end case;
      end Mode_To_File_Access;

      ---------------
      -- Open_File --
      ---------------

      function Open_File (Name : in String;
                          Mode : in POSIX_IO.File_Mode)
                          return Win32.Winnt.HANDLE
      is
         use type Win32.INT;
         use type System.Address;
         L_Name      : constant String := Name & ASCII.Nul;
         H           : Win32.Winnt.HANDLE;
      begin
         H := Win32.Winbase.CreateFile
           (Win32.Addr (L_Name),
            Mode_To_File_Access (Mode),
            Win32.Winnt.FILE_SHARE_READ,
            Security_Handles_Inherited'Access,
            Win32.Winbase.OPEN_EXISTING,
            Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
            System.Null_Address);
         if H = Win32.Winbase.INVALID_HANDLE_VALUE then
            POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error,
                                       "Template : Open_File " & Name);
         end if;
         return H;
      end Open_File;

      -----------------
      -- Create_File --
      -----------------

      function Create_File (Name : in String;
                            Mode : in POSIX_IO.File_Mode)
                            return Win32.Winnt.HANDLE
      is
         use type Win32.INT;
         use type System.Address;
         L_Name : constant String := Name & ASCII.Nul;
         H : Win32.Winnt.HANDLE;
      begin
         H := Win32.Winbase.CreateFile
           (Win32.Addr (L_Name),
            Mode_To_File_Access (Mode),
            Win32.Winnt.FILE_SHARE_WRITE,
            Security_Handles_Inherited'Access,
            Win32.Winbase.CREATE_ALWAYS,
            Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
            System.Null_Address);
         if H = Win32.Winbase.INVALID_HANDLE_VALUE then
            POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error,
                                       "Template : Create_File " & Name);
         end if;
         return H;
      end Create_File;

   begin
      if not Template.Keep_Effective_IDs then
         Set_User_ID (Get_Real_User_ID);
         Set_Group_ID (Get_Real_Group_ID);
      end if;
      --  POSIX_Signals.Set_Blocked_Signals (Template.Signal_Mask, Junk1);

      P := Template.File_Request_List;

      while P /= null loop

         case P.Action is

            when Open =>
               case P.File is
                  when POSIX_IO.Standard_Input =>
                     SI.hStdInput := Open_File (To_String (P.Name),
                                                P.Mode);
                     P.OHandle := SI.hStdInput;
                  when POSIX_IO.Standard_Output =>
                     SI.hStdOutput := Create_File (To_String (P.Name),
                                                   P.Mode);
                     P.OHandle := SI.hStdOutput;
                  when POSIX_IO.Standard_Error =>
                     SI.hStdError := Create_File (To_String (P.Name),
                                                  P.Mode);
                     P.OHandle := SI.hStdError;
                  when others =>
                     POSIX_Win32.Raise_Not_Yet_Implemented
                       ("Execute Template Open for non Std file");
               end case;

            when Close =>
               case P.File is
                  when POSIX_IO.Standard_Input =>
                     SI.hStdInput := Open_File (Null_Filename,
                                                POSIX_IO.Read_Only);
                     P.CHandle := SI.hStdInput;
                  when POSIX_IO.Standard_Output =>
                     SI.hStdOutput := Create_File (Null_Filename,
                                                   POSIX_IO.Write_Only);
                     P.CHandle := SI.hStdOutput;
                  when POSIX_IO.Standard_Error =>
                     SI.hStdError := Create_File (Null_Filename,
                                                  POSIX_IO.Write_Only);
                     P.CHandle := SI.hStdError;
                  when others =>
                     POSIX_Win32.Raise_Not_Yet_Implemented
                       ("Execute Template Close for non Std file");
               end case;

            when Duplicate =>
               POSIX_Win32.Raise_Not_Yet_Implemented
                 ("Execute Template Duplicate for non Std file");
         end case;
         P := P.Next;
      end loop;
   end Execute_Template;


   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Child       :    out POSIX_Process_Identification.Process_ID;
      Pathname    : in     POSIX.Pathname;
      Template    : in     Process_Template;
      Env_List    : in     POSIX_Process_Environment.Environment;
      Arg_List    : in     POSIX.POSIX_String_List := POSIX.Empty_String_List;
      Environment : in Boolean)
   is
      use type Win32.BOOL;
      use type Win32.ULONG;

      --  add .exe suffix if it is not set
      function Add_Exe (S : in String) return String is
      begin
         if S'Length > 4 and then
           S (S'Last - 3 .. S'Last) /= ".exe"
         then
            return S & ".exe";
         else
            return S;
         end if;
      end Add_Exe;

      Startup_Informations : aliased Win32.Winbase.STARTUPINFO;
      Process_Informations : aliased Win32.Winbase.PROCESS_INFORMATION;

      L_Pathname : constant String
        := Add_Exe (POSIX.To_String (Pathname)) & ASCII.Nul;

      Result : Win32.BOOL;

      Arguments      : Unbounded_String;
      Argument_Count : Natural := 0;

      ------------
      -- Concat --
      ------------

      procedure Concat (Item : in POSIX_String;
                        Quit : in out Boolean) is
      begin
         --  the first argument is by convention the program name and we don't
         --  want to add it to the command line.
         if Argument_Count /= 0 then
            Arguments := Arguments & POSIX.To_String (Item) & ' ';
         end if;
         Argument_Count := Argument_Count + 1;
         Quit := False;
      end Concat;

      --------------------------
      -- Concat_All_Arguments --
      --------------------------

      procedure Concat_All_Arguments is new POSIX.For_Every_Item (Concat);

      ---------------
      -- To_LPVOID --
      ---------------

      function To_LPVOID is
        new Ada.Unchecked_Conversion (POSIX_Process_Environment.Environment,
                                      Win32.LPVOID);

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

      Execute_Template (Template, Startup_Informations);

      Concat_All_Arguments (Arg_List);

      if Environment then
         Env_Pointer := To_LPVOID (Env_List);
      else
         Env_Pointer := System.Null_Address;
      end if;

      Run_Process :
      declare
         use type Win32.ULONG;
         use Win32.Winbase;
         L_Command : constant String :=
           POSIX.To_String (Pathname) & ' ' &
           To_String (Arguments) &
           ASCII.Nul;
      begin
         Result := CreateProcess
           (LpApplicationName    => Win32.Addr (L_Pathname),
            LpCommandLine        => Win32.Addr (L_Command),
            LpProcessAttributes  => Security_Handles_Inherited'Access,
            LpThreadAttributes   => null,
            BInheritHandles      => Win32.TRUE,
            DwCreationFlags      => NORMAL_PRIORITY_CLASS,
            LpEnvironment        => Env_Pointer,
            LpCurrentDirectory   => null,
            LpStartupInfo        => Startup_Informations'Unchecked_Access,
            LpProcessInformation => Process_Informations'Unchecked_Access
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
     (Child    :    out POSIX_Process_Identification.Process_ID;
      Pathname : in     POSIX.Pathname;
      Template : in     Process_Template;
      Arg_List : in     POSIX.POSIX_String_List :=
        POSIX.Empty_String_List)
   is
      Null_Environment : POSIX_Process_Environment.Environment;
   begin
      Start_Process (Child, Pathname, Template,
                     Null_Environment, Arg_List,
                     Environment => False);
   end Start_Process;


   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Child    :    out POSIX_Process_Identification.Process_ID;
      Pathname : in     POSIX.Pathname;
      Template : in     Process_Template;
      Env_List : in     POSIX_Process_Environment.Environment;
      Arg_List : in     POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
   begin
      Start_Process (Child, Pathname, Template, Env_List, Arg_List,
                     Environment => True);
   end Start_Process;


   lpFilePart : aliased Win32.LPSTR;

   --------------------------
   -- Start_Process_Search --
   --------------------------

   procedure Start_Process_Search
     (Child    :    out POSIX_Process_Identification.Process_ID;
      Filename : in     POSIX.Filename;
      Template : in     Process_Template;
      Arg_List : in     POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
      Null_Environment : POSIX_Process_Environment.Environment;
      Max_Len    : constant := 500;
      Pathname   : String (1 .. Max_Len) := (others => '.');
      pragma Warnings (Off, Pathname);
      L_Filename : constant String := POSIX.To_String (Filename) & ASCII.Nul;
      Ext_Var    : constant String := ".exe" & ASCII.Nul;

      Result     : Win32.DWORD;
   begin
      Check_Open (Template, "Start_Process_Search");

      Result := Win32.Winbase.SearchPath (null,
                                          Win32.Addr (L_Filename),
                                          Win32.Addr (Ext_Var),
                                          Win32.DWORD (Pathname'Length),
                                          Win32.Addr (Pathname),
                                          lpFilePart'Access);

      if Result = 0 then
         Start_Process (Child, Filename,
                        Template, Null_Environment, Arg_List,
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
     (Child    :    out POSIX_Process_Identification.Process_ID;
      Filename : in     POSIX.Filename;
      Template : in     Process_Template;
      Env_List : in     POSIX_Process_Environment.Environment;
      Arg_List : in     POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
      Max_Len    : constant := 500;
      Pathname   : String (1 .. Max_Len);
      pragma Warnings (Off, Pathname);
      L_Filename : constant String := POSIX.To_String (Filename) & ASCII.Nul;
      Ext_Var    : constant String := ".exe" & ASCII.Nul;
      Result     : Win32.DWORD;
   begin
      Check_Open (Template, "Start_Process_Search");

      Search_Filename_In_Env :
      declare
         Old_Environment : POSIX_Process_Environment.Environment;
      begin
         POSIX_Process_Environment.Copy_From_Current_Environment
           (Old_Environment);

         POSIX_Process_Environment.Copy_To_Current_Environment
           (Env_List);

         Result := Win32.Winbase.SearchPath (null,
                                             Win32.Addr (L_Filename),
                                             Win32.Addr (Ext_Var),
                                             Win32.DWORD (Pathname'Length),
                                             Win32.Addr (Pathname),
                                             lpFilePart'Access);

         POSIX_Process_Environment.Copy_To_Current_Environment
           (Old_Environment);
      end Search_Filename_In_Env;

      if Result = 0 then
         Start_Process (Child, Filename,
                        Template, Env_List, Arg_List,
                        Environment => True);
      else
         Start_Process
           (Child,
            POSIX.To_POSIX_String (Pathname (1 .. Positive (Result))),
            Template, Env_List, Arg_List,
            Environment => True);
      end if;
   end Start_Process_Search;


   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process (Status : in Exit_Status := Normal_Exit) is
   begin
      Win32.Winbase.ExitProcess (Win32.UINT (Status));
   end Exit_Process;


   ----------------------
   -- Status_Available --
   ----------------------

   function Status_Available (Status : in Termination_Status)
                              return Boolean is
   begin
      return Status.Pid /= Null_Process_ID;
   end Status_Available;


   -------------------
   -- Process_ID_Of --
   -------------------

   function Process_ID_Of (Status : in Termination_Status)
                           return Process_ID is
   begin
      if not Status_Available (Status) then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Status.Pid;
   end  Process_ID_Of;


   --------------------------
   -- Termination_Cause_Of --
   --------------------------

   function Termination_Cause_Of (Status : in Termination_Status)
                                  return Termination_Cause is
   begin
      if not Status_Available (Status) then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Exited;
   end Termination_Cause_Of;


   --------------------
   -- Exit_Status_Of --
   --------------------

   function Exit_Status_Of (Status : in Termination_Status)
                            return Exit_Status is
   begin
      if not Status_Available (Status)
        or Termination_Cause_Of (Status) /= Exited then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Exit_Status (Status.Exit_Status);
   end Exit_Status_Of;


   ---------------------------
   -- Termination_Signal_Of --
   ---------------------------

   function Termination_Signal_Of (Status : Termination_Status)
                                   return POSIX_Signals.Signal is
   begin
      if not Status_Available (Status)
        or Termination_Cause_Of (Status) /= Terminated_By_Signal then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return POSIX_Signals.Signal_Terminate;
   end Termination_Signal_Of;


   ------------------------
   -- Stopping_Signal_Of --
   ------------------------

   function Stopping_Signal_Of (Status : Termination_Status)
                                return POSIX_Signals.Signal is
   begin
      if not Status_Available (Status)
        or Termination_Cause_Of (Status) /= Stopped_By_Signal then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return POSIX_Signals.Signal_Abort;
   end Stopping_Signal_Of;


   Process_Status : aliased Win32.DWORD;

   ----------------------------
   -- Wait_For_Child_Process --
   ----------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Child          : in     POSIX_Process_Identification.Process_ID;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      use type Win32.ULONG;
      Retcode  : Win32.DWORD;
      Result   : Win32.BOOL;
      Process_Informations : Win32.Winbase.PROCESS_INFORMATION;
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
         Process_Status'Access);
      Status.Exit_Status := Exit_Stat (Process_Status);

      --  remove child from the list of child process
      POSIX_Win32.Remove_Child (Child);
   end Wait_For_Child_Process;


   ----------------------------
   -- Wait_For_Child_Process --
   ----------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Group          : in     POSIX_Process_Identification.Process_Group_ID;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      POSIX_Win32.Wait (Status, Block);
   end Wait_For_Child_Process;


   ----------------------------
   -- Wait_For_Child_Process --
   ----------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      POSIX_Win32.Wait (Status, Block);
   end Wait_For_Child_Process;

end POSIX_Process_Primitives;
