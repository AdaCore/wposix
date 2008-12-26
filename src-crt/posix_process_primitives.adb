
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;
with POSIX_Win32;

with Win32;
with Win32.Stdlib;
with Win32.Winbase;
with Win32.Winnt;

package body POSIX_Process_Primitives is

   use POSIX;


   function Process_ID_To_PROCESS_INFORMATION is
      new Ada.Unchecked_Conversion (POSIX_Process_Identification.Process_ID,
                                    Win32.Winbase.PROCESS_INFORMATION);

   function PROCESS_INFORMATION_To_Process_ID is
      new Ada.Unchecked_Conversion (Win32.Winbase.PROCESS_INFORMATION,
                                    POSIX_Process_Identification.Process_ID);

   --  Process Template

   procedure Open_Template (Template: in out Process_Template) is
   begin
      if Template.Is_Open then
         Close_Template (Template);
      end if;
      Template.Keep_Effective_IDs := False;
      POSIX_Signals.Delete_All_Signals (Template.Signal_Mask);
      Template.Is_Open := True;
   end Open_Template;

                      ------------------------------

   procedure Check_Open (Template : in Process_Template;
                         Message  : in String) is
   begin
      if not Template.Is_Open then
         POSIX_Win32.Raise_Error (Message, Invalid_Argument);
      end if;
   end Check_Open;

                      ------------------------------

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

                      ------------------------------

   procedure Close_Template (Template: in out Process_Template) is
      P : File_Request_Ptr;
      procedure Free is
        new Ada.Unchecked_Deallocation (File_Request, File_Request_Ptr);
   begin
      Check_Open (Template, "Close_Template");
      while Template.File_Request_List /= null loop
         P := Template.File_Request_List;
         Template.File_Request_List := P.Next;
         Free (P);
      end loop;
      Template.Is_Open := False;
   end Close_Template;

                      ------------------------------

   procedure Set_Keep_Effective_IDs (Template: in out Process_Template) is
   begin
      Check_Open (Template, "Set_Keep_Effective_IDs");
      Template.Keep_Effective_IDs := True;
   end Set_Keep_Effective_IDs;

                      ------------------------------

   procedure Set_Signal_Mask
     (Template : in out Process_Template;
      Mask     : in     POSIX_Signals.Signal_Set) is
   begin
      Check_Open (Template, "Set_Signal_Mask");
      Template.Signal_Mask := Mask;
   end Set_Signal_Mask;

                      ------------------------------

   procedure Set_Creation_Signal_Masking
     (Template       : in out Process_Template;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      Check_Open (Template, "Set_Creation_Signal_Masking");
      Template.Signal_Creation_Masking := Masked_Signals;
   end Set_Creation_Signal_Masking;

                      ------------------------------

   procedure Set_File_Action_To_Open
     (Template : in out Process_Template;
      File     : in     POSIX_IO.File_Descriptor;
      Name     : in     POSIX.Pathname;
      Mode     : in     POSIX_IO.File_Mode       := POSIX_IO.Read_Only;
      Options  : in     POSIX_IO.Open_Option_Set := POSIX_IO.Empty_Set) is
   begin
      Check_Open (Template, "Set_File_Action_To_Open");
      Check_File (File);
      Template.File_Request_List :=
       new File_Request'(Next    => Template.File_Request_List,
                         File    => File,
                         Action  => Open,
                         Name    =>
                           To_Unbounded_String (POSIX.To_String (Name)),
                         Mode    => Mode,
                         Options => Options);
   end Set_File_Action_To_Open;

                      ------------------------------

   procedure Set_File_Action_To_Close
     (Template : in out Process_Template;
      File     : in     POSIX_IO.File_Descriptor) is
   begin
      Check_Open (Template, "Set_File_Action_To_Close");
      Template.File_Request_List :=
       new File_Request'(Next   => Template.File_Request_List,
                         File   => File,
                         Action => Close);
   end Set_File_Action_To_Close;

                      ------------------------------

   procedure Set_File_Action_To_Duplicate
     (Template  : in out Process_Template;
      File      : in     POSIX_IO.File_Descriptor;
      From_File : in     POSIX_IO.File_Descriptor) is
   begin
      Check_Open (Template, "Set_File_Action_To_Duplicate");
      Template.File_Request_List :=
       new File_Request'(Next      => Template.File_Request_List,
                         File      => File,
                         Action    => Duplicate,
                         From_File => From_File);
   end Set_File_Action_To_Duplicate;

                      ------------------------------

   procedure Execute_Template (Template : in     Process_Template;
                               SI       : in out Win32.Winbase.STARTUPINFO)
   is

      Null_Filename : constant String := "nul";

      P             : File_Request_Ptr;
      Reverse_List  : File_Request_Ptr;
      Forward_List  : File_Request_Ptr;
      Junk1         : POSIX_Signals.Signal_Set;

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

      function Open_File (Name : in String;
                          Mode : in POSIX_IO.File_Mode)
                          return Win32.Winnt.HANDLE
      is
         use type System.Address;
         L_Name      : constant String := Name & ASCII.Nul;
         H           : Win32.Winnt.HANDLE;
      begin
         H := Win32.Winbase.CreateFile
           (Win32.Addr (L_Name),
            Mode_To_File_Access (Mode),
            0,
            POSIX_Win32.Null_Inherited_Security_Attributes'Unchecked_Access,
            Win32.Winbase.OPEN_EXISTING,
            Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
            System.Null_Address);
         if H = Win32.Winbase.INVALID_HANDLE_VALUE then
            POSIX_Win32.Check_Retcode (-1, "Template : Open_File ");
         end if;
         return H;
      end Open_File;

      function Create_File (Name : in String;
                            Mode : in POSIX_IO.File_Mode)
                            return Win32.Winnt.HANDLE
      is
         use type System.Address;
         L_Name : constant String := Name & ASCII.Nul;
         H : Win32.Winnt.HANDLE;
      begin
         H := Win32.Winbase.CreateFile
           (Win32.Addr (L_Name),
            Mode_To_File_Access (Mode),
            0,
            POSIX_Win32.Null_Inherited_Security_Attributes'Unchecked_Access,
            Win32.Winbase.CREATE_ALWAYS,
            Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
            System.Null_Address);
         if H = Win32.Winbase.INVALID_HANDLE_VALUE then
            POSIX_Win32.Check_Retcode (-1, "Template : Create_File ");
         end if;
         return H;
      end Create_File;

   begin
      if not Template.Keep_Effective_IDs then
         Set_User_ID (Get_Real_User_ID);
         Set_Group_ID (Get_Real_Group_ID);
      end if;
      POSIX_Signals.Set_Blocked_Signals (Template.Signal_Mask, Junk1);

      --  The list of file actions is in reverse order.
      Reverse_List := Template.File_Request_List;
      while Reverse_List /= null loop
         P := Reverse_List;
         Reverse_List := P.Next;
         P.Next := Forward_List;
         Forward_List := P;
      end loop;

      P := Forward_List;

      while P /= null loop

         case P.Action is

            when Open =>
               case P.File is
                  when POSIX_IO.Standard_Input =>
                     SI.HStdInput := Open_File (To_String (P.Name),
                                                P.Mode);
                  when POSIX_IO.Standard_Output =>
                     SI.HStdOutput := Create_File (To_String (P.Name),
                                                   P.Mode);
                  when POSIX_IO.Standard_Error =>
                     SI.HStdError := Create_File (To_String (P.Name),
                                                  P.Mode);
                  when others =>
                     null; -- not handled
               end case;

            when Close =>
               case P.File is
                  when POSIX_IO.Standard_Input =>
                     SI.HStdInput := Open_File (Null_Filename,
                                                POSIX_IO.Read_Only);
                  when POSIX_IO.Standard_Output =>
                     SI.HStdOutput := Create_File (Null_Filename,
                                                   POSIX_IO.Write_Only);
                  when POSIX_IO.Standard_Error =>
                     SI.HStdError := Create_File (Null_Filename,
                                                  POSIX_IO.Write_Only);
                  when others =>
                     null; -- not handled
               end case;

            when Duplicate =>
               null;
         end case;
         P := P.Next;
      end loop;
   end Execute_Template;

                      ------------------------------

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

      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;

      Startup_Informations : aliased Win32.Winbase.STARTUPINFO;
      Process_Informations : aliased Win32.Winbase.PROCESS_INFORMATION;

      Result : Win32.BOOL;

      Arguments : Unbounded_String;

      procedure Concat (Item : in POSIX_String;
                        Quit : in out Boolean) is
      begin
         Arguments := Arguments & POSIX.To_String (Item) & ' ';
         Quit := False;
      end Concat;

      procedure Concat_All_Arguments is new POSIX.For_Every_Item (Concat);

      function To_LPVOID is
        new Ada.Unchecked_Conversion (POSIX_Process_Environment.Environment,
                                      Win32.LPVOID);

      Env_Pointer : Win32.LPVOID;

   begin
      Check_Open (Template, "Start_Process");

      Startup_Informations.Cb          := Win32.Winbase.STARTUPINFO'Size / 8;
      Startup_Informations.LpReserved  := null;
      Startup_Informations.LpDesktop   := null;
      Startup_Informations.LpTitle     := null;
      Startup_Informations.DwFlags     := Win32.Winbase.STARTF_USESTDHANDLES;
      Startup_Informations.CbReserved2 := 0;
      Startup_Informations.LpReserved2 := null;

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
         L_Arguments : constant String := To_String (Arguments) & ASCII.Nul;
      begin
         Result := CreateProcess
           (LpApplicationName    => Win32.Addr (L_Pathname),
            LpCommandLine        => Win32.Addr (L_Arguments),
            LpProcessAttributes  => null,
            LpThreadAttributes   => null,
            BInheritHandles      => Win32.TRUE,
            DwCreationFlags      => DETACHED_PROCESS + NORMAL_PRIORITY_CLASS,
            LpEnvironment        => Env_Pointer,
            LpCurrentDirectory   => null,
            LpStartupInfo        => Startup_Informations'Unchecked_access,
            LpProcessInformation => Process_Informations'Unchecked_access
            );
      end Run_Process;

      if Result = Win32.FALSE then
         Process_Informations.DwProcessId := -1;
      end if;

      Child := PROCESS_INFORMATION_To_Process_ID (Process_Informations);
   end Start_Process;

                      ------------------------------

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

                      ------------------------------

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

                      ------------------------------

   procedure Start_Process_Search
     (Child    :    out POSIX_Process_Identification.Process_ID;
      Filename : in     POSIX.Filename;
      Template : in     Process_Template;
      Arg_List : in     POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
      Null_Environment : POSIX_Process_Environment.Environment;
      Max_Len    : constant := 500;
      Pathname   : String (1 .. Max_Len);
      L_Filename : constant String := POSIX.To_String (Filename) & ASCII.Nul;
      Env_Var    : constant String := "PATH" & ASCII.Nul;
   begin
      Check_Open (Template, "Start_Process_Search");
      Win32.Stdlib.Searchenv (Win32.Addr (L_Filename),
                              Win32.Addr (Env_Var),
                              Win32.Addr (Pathname));
      if Pathname (1) = ASCII.Nul then
         Start_Process (Child, Filename,
                        Template, Null_Environment, Arg_List,
                        Environment => False);
      else
         Start_Process (Child, POSIX.To_POSIX_String (Pathname),
                        Template, Null_Environment, Arg_List,
                        Environment => False);
      end if;
   end Start_Process_Search;

                      ------------------------------

   procedure Start_Process_Search
     (Child    :    out POSIX_Process_Identification.Process_ID;
      Filename : in     POSIX.Filename;
      Template : in     Process_Template;
      Env_List : in     POSIX_Process_Environment.Environment;
      Arg_List : in     POSIX.POSIX_String_List := POSIX.Empty_String_List)
   is
      Max_Len    : constant := 500;
      Pathname   : String (1 .. Max_Len);
      L_Filename : constant String := POSIX.To_String (Filename) & ASCII.Nul;
      Env_Var    : constant String := "PATH" & ASCII.Nul;
   begin
      Check_Open (Template, "Start_Process_Search");

      Search_Filename_In_Env:
      declare
         Old_Environment : POSIX_Process_Environment.Environment;
      begin
         POSIX_Process_Environment.Copy_From_Current_Environment
           (Old_Environment);

         POSIX_Process_Environment.Copy_To_Current_Environment
           (Env_List);

         Win32.Stdlib.Searchenv (Win32.Addr (L_Filename),
                                 Win32.Addr (Env_Var),
                                 Win32.Addr (Pathname));

         POSIX_Process_Environment.Copy_To_Current_Environment
           (Old_Environment);
      end Search_Filename_In_Env;

      if Pathname (1) = ASCII.Nul then
         Start_Process (Child, Filename,
                        Template, Env_List, Arg_List,
                        Environment => True);
      else
         Start_Process (Child, POSIX.To_POSIX_String (Pathname),
                        Template, Env_List, Arg_List,
                        Environment => True);
      end if;
   end Start_Process_Search;

                      ------------------------------

   procedure Exit_Process (Status : in Exit_Status := Normal_Exit) is
   begin
      Win32.Stdlib.Exit_Program (Win32.INT (Status));
   end Exit_Process;

                      ------------------------------

   function Status_Available (Status : in Termination_Status)
                              return Boolean is
   begin
      return Status.Pid /= Null_Process_ID;
   end Status_Available;

                      ------------------------------

   function Process_ID_Of (Status : in Termination_Status)
                           return Process_ID is
   begin
      if not Status_Available (Status) then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Status.Pid;
   end  Process_ID_Of;

                      ------------------------------

   function Termination_Cause_Of (Status : in Termination_Status)
                                  return Termination_Cause is
   begin
      if not Status_Available (Status) then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return Exited;
   end Termination_Cause_Of;

                      ------------------------------

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

                      ------------------------------

   function Termination_Signal_Of (Status: Termination_Status)
                                   return POSIX_Signals.Signal is
   begin
      if not Status_Available (Status)
        or Termination_Cause_Of (Status) /= Terminated_By_Signal then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return POSIX_Signals.Signal_Terminate;
   end Termination_Signal_Of;

                      ------------------------------

   function Stopping_Signal_Of (Status: Termination_Status)
                                return POSIX_Signals.Signal is
   begin
      if not Status_Available (Status)
        or Termination_Cause_Of (Status) /= Stopped_By_Signal then
         Set_Error_Code (Invalid_Argument);
         raise POSIX_Error;
      end if;
      return POSIX_Signals.Signal_Abort;
   end Stopping_Signal_Of;

                      ------------------------------

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
      Process_Informations := Process_ID_To_PROCESS_INFORMATION (Child);

      if Child = Null_Process_ID then
         Set_Error_Code (No_Child_Process);
         raise POSIX_Error;
      elsif Process_Informations.DwProcessId = -1 then
         Status.Exit_Status := Exit_Stat (Failed_Creation_Exit);
         return;
      end if;

      Retcode := Win32.Winbase.WaitForSingleObject
        (Process_Informations.Hprocess,
         Win32.Winbase.INFINITE);

      Status.Pid := Child;

      declare
         Process_Status : aliased Win32.DWORD;
      begin
         Result := Win32.Winbase.GetExitCodeProcess
           (Process_Informations.Hprocess,
            Process_Status'Unchecked_Access);
         Status.Exit_Status := Exit_Stat (Process_Status);
      end;
   end Wait_For_Child_Process;

                      ------------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Group          : in     POSIX_Process_Identification.Process_Group_ID;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Wait_For_Child_Process (Group)");
   end Wait_For_Child_Process;

                      ------------------------------

   procedure Wait_For_Child_Process
     (Status         :    out Termination_Status;
      Block          : in     Boolean := True;
      Trace_Stopped  : in     Boolean := True;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Wait_For_Child_Process (all)");
   end Wait_For_Child_Process;

end POSIX_Process_Primitives;
