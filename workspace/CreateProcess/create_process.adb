
with Ada.Text_IO;

with System;
with Win32;
with Win32.Winbase;
with Win32.Winnt;

procedure Create_Process is

   use Ada.Text_IO;
   use Win32;
   use Win32.Winbase;
   use Win32.Winnt;

   use type Win32.ULONG;

   Directory : constant String := "c:\Projets\POSIX\CreateProcess";

   App_Name : constant String  := Directory & "\io.exe" & ASCII.Nul;
   Cmd_Line : constant String := Directory & "\io.exe p1 p2" & ASCII.Nul;

   Startup_Informations : aliased STARTUPINFO;
   SI : STARTUPINFO renames Startup_Informations;

   PI : aliased PROCESS_INFORMATION;

   Null_Security_Attributes : aliased Win32.Winbase.SECURITY_ATTRIBUTES :=
     (NLength              => Win32.Winbase.SECURITY_ATTRIBUTES'Size / 8,
      LpSecurityDescriptor => System.Null_Address,
      BInheritHandle       => Win32.TRUE);

   Result : Win32.BOOL;

   function Open_File (Name : in String)
                       return HANDLE
   is
      use type System.Address;
      L_Name : constant String := Name & ASCII.Nul;
      H : HANDLE;
   begin
      H := CreateFile (Addr (L_Name),
                       GENERIC_READ + GENERIC_WRITE,
                       0,
                       Null_Security_Attributes'Unchecked_Access,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL,
                       System.Null_Address);
      if H = INVALID_HANDLE_VALUE then
         Put_Line ("can't create " & Name);
      end if;
      return H;
   end Open_File;

   function Create_File (Name : in String)
                       return HANDLE
   is
      use type System.Address;
      L_Name : constant String := Name & ASCII.Nul;
      H : HANDLE;
   begin
      H := CreateFile (Addr (L_Name),
                       GENERIC_READ + GENERIC_WRITE,
                       0,
                       Null_Security_Attributes'Unchecked_Access,
                       CREATE_ALWAYS,
                       FILE_ATTRIBUTE_NORMAL,
                       System.Null_Address);
      if H = INVALID_HANDLE_VALUE then
         Put_Line ("can't create " & Name);
      end if;
      return H;
   end Create_File;

begin

   --  Startup Informations
   SI.Cb          := STARTUPINFO'Size / 8;
   SI.LpReserved  := null;
   SI.LpDesktop   := null;
   SI.LpTitle     := null;
   SI.DwFlags     := STARTF_USESTDHANDLES;
   SI.CbReserved2 := 0;
   SI.LpReserved2 := null;
   SI.HStdInput   := Open_File (Directory & "\input");
   SI.HStdOutput  := Create_File (Directory & "\output");
   SI.HStdError   := Create_File (Directory & "\error"); -- can be "\nul"

   Result := CreateProcess
     (LpApplicationName => Addr (App_Name),
      LpCommandLine     => Addr (Cmd_Line),
      LpProcessAttributes =>  Null,
      LpThreadAttributes => Null,
      BInheritHandles =>  Win32.TRUE,
      DwCreationFlags => DETACHED_PROCESS + NORMAL_PRIORITY_CLASS,
      LpEnvironment => System.Null_Address,
      LpCurrentDirectory => null,
      LpStartupInfo => SI'Unchecked_Access,
      LpProcessInformation => PI'Unchecked_Access
      );

   if Result = Win32.TRUE then
      Put_Line ("CreateProcess ok");
      declare
         D : DWORD;
      begin
         D := WaitForSingleObject (PI.HProcess, INFINITE);
         case D is
            when WAIT_FAILED =>
               Put_Line ("WAIT FAILED");
            when WAIT_ABANDONED =>
               Put_Line ("WAIT ABANDONED");
            when WAIT_OBJECT_0 =>
               Put_Line ("WAIT OBJECT 0");
            when WAIT_TIMEOUT =>
               Put_Line ("WAIT TIMEOUT");
            when others =>
               Put_Line ("WAIT error unknown");
         end case;
      end;
   else
      declare
         E : DWORD := GetLastError;
      begin
         Put_Line ("CreatteProcess not ok : " & DWORD'Image (E));
      end;
   end if;

   Result := CloseHandle (SI.HStdInput);
   Result := CloseHandle (SI.HStdOutput);
   Result := CloseHandle (SI.HStdError);

end Create_Process;
