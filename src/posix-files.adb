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

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with Win32.AccCtrl;
with Win32.Aclapi;
with Win32.Sddl;
with Win32.Winnt;
with Win32.Winerror;

with POSIX_Win32;
with POSIX_Win32.Permissions;

with POSIX.File_Status;

package body POSIX.Files is

   function To_SYSTEMTIME is new Ada.Unchecked_Conversion
     (POSIX.Calendar.POSIX_Time, Win32.Winbase.SYSTEMTIME);

   -------------------
   -- Accessibility --
   -------------------

   function Accessibility
     (Pathname    : POSIX.Pathname;
      Access_Mode : Access_Mode_Set) return POSIX.Error_Code
   is
      use POSIX.Permissions;
      F_Perms : POSIX.Permissions.Permission_Set;
      Retcode : POSIX.Error_Code := POSIX.No_Error;
   begin
      if Is_File_Present (Pathname) then
         F_Perms := POSIX.File_Status.Permission_Set_Of
           (POSIX.File_Status.Get_File_Status (Pathname));

         if (Access_Mode (Read_Ok) and then not F_Perms (Owner_Read))
           or else
             (Access_Mode (Write_Ok) and then not F_Perms (Owner_Write))
           or else
             (Access_Mode (Execute_Ok) and then not F_Perms (Owner_Execute))
         then
            Retcode := POSIX.Permission_Denied;
         end if;

      else
         Retcode := POSIX.No_Such_File_Or_Directory;
      end if;
      return Retcode;
   end Accessibility;

   ----------------------------
   -- Change_Owner_And_Group --
   ----------------------------

   procedure Change_Owner_And_Group
     (Pathname : POSIX.Pathname;
      Owner    : POSIX.Process_Identification.User_ID;
      Group    : POSIX.Process_Identification.Group_ID)
   is
      use type Win32.Winnt.SECURITY_INFORMATION;

      C_Pathname :  constant String := To_String (Pathname) & ASCII.NUL;
      SUSID      : constant String :=
                     Process_Identification.Image (Owner) & ASCII.NUL;
      SGSID      : constant String :=
                     Process_Identification.Image (Group) & ASCII.NUL;
      USID       : aliased Win32.Winnt.PSID;
      GSID       : aliased Win32.Winnt.PSID;
      Res        : Win32.BOOL;
      Ret        : Win32.DWORD;
   begin
      Res := Win32.Sddl.ConvertStringSidToSid
        (Win32.Addr (SUSID), USID'Access);
      POSIX_Win32.Check_Result
        (Res, "Change_Owner_And_Group.ConvertStringSidToSid (Owner)");

      Res := Win32.Sddl.ConvertStringSidToSid
        (Win32.Addr (SGSID), GSID'Access);
      POSIX_Win32.Check_Result
        (Res, "Change_Owner_And_Group.ConvertStringSidToSid (Group)");

      Ret := Win32.Aclapi.SetNamedSecurityInfo
        (Win32.Addr (C_Pathname),
         Win32.AccCtrl.SE_FILE_OBJECT,
         Win32.Winnt.OWNER_SECURITY_INFORMATION +
           Win32.Winnt.GROUP_SECURITY_INFORMATION,
         USID,
         GSID,
         null,
         null);

      POSIX_Win32.Check_Retcode (Ret, "Change_Owner_And_Group");
   end Change_Owner_And_Group;

   ------------------------
   -- Change_Permissions --
   ------------------------

   procedure Change_Permissions
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set)
   is
      use type Win32.DWORD;
      use type Win32.Winnt.SECURITY_INFORMATION;
      use POSIX.Permissions;

      package PWP renames POSIX_Win32.Permissions;

      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.NUL;
      CESID      : constant String := POSIX_Win32.Everyone_SID & ASCII.NUL;
      ESID       : aliased Win32.Winnt.PSID;
      OSID, GSID : aliased Win32.Winnt.PSID;
      DACL       : aliased Win32.Winnt.PACL;
      SD         : Win32.Winnt.SECURITY_DESCRIPTOR;
      EA         : array (POSIX_Win32.Permissions.UGO) of
                     aliased Win32.AccCtrl.EXPLICIT_ACCESS;
      Ret        : Win32.DWORD;
      Res        : Win32.BOOL;
   begin
      --  Create well-known group for everyone

      Res := Win32.Sddl.ConvertStringSidToSid
        (Win32.Addr (CESID), ESID'Access);

      POSIX_Win32.Check_Result
        (Res, "Change_Permissions.ConvertStringSidToSid");

      --  Get owner and primary group SID

      Ret := Win32.Aclapi.GetNamedSecurityInfo
        (Win32.Addr (L_Pathname),
         Win32.AccCtrl.SE_FILE_OBJECT,
         Win32.Winnt.OWNER_SECURITY_INFORMATION +
           Win32.Winnt.GROUP_SECURITY_INFORMATION,
         OSID'Access,
         GSID'Access,
         null,
         null,
         SD'Address);

      POSIX_Win32.Check_Retcode
        (Ret, "Change_Permissions.GetNamedSecurityInfo");

      --  Create the explict access set

      EA :=
        (PWP.U =>
           (grfAccessPermissions => 0,
            grfAccessMode        => Win32.AccCtrl.SET_ACCESS,
            grfInheritance       => Win32.AccCtrl.NO_INHERITANCE,
            vTrustee             =>
              (pMultipleTrustee         => null,
               MultipleTrusteeOperation => Win32.AccCtrl.NO_MULTIPLE_TRUSTEE,
               TrusteeForm              => Win32.AccCtrl.TRUSTEE_IS_SID,
               TrusteeType              => Win32.AccCtrl.TRUSTEE_IS_USER,
               ptstrName                => Win32.AccCtrl.To_LPSTR (OSID))),
         PWP.G =>
           (grfAccessPermissions => 0,
            grfAccessMode        => Win32.AccCtrl.SET_ACCESS,
            grfInheritance       => Win32.AccCtrl.NO_INHERITANCE,
            vTrustee             =>
              (pMultipleTrustee         => null,
               MultipleTrusteeOperation => Win32.AccCtrl.NO_MULTIPLE_TRUSTEE,
               TrusteeForm              => Win32.AccCtrl.TRUSTEE_IS_SID,
               TrusteeType              => Win32.AccCtrl.TRUSTEE_IS_GROUP,
               ptstrName                => Win32.AccCtrl.To_LPSTR (GSID))),
         PWP.O =>
           (grfAccessPermissions => 0,
            grfAccessMode        => Win32.AccCtrl.SET_ACCESS,
            grfInheritance       => Win32.AccCtrl.NO_INHERITANCE,
            vTrustee             =>
              (pMultipleTrustee         => null,
               MultipleTrusteeOperation => Win32.AccCtrl.NO_MULTIPLE_TRUSTEE,
               TrusteeForm              => Win32.AccCtrl.TRUSTEE_IS_SID,
               TrusteeType              =>
                 Win32.AccCtrl.TRUSTEE_IS_WELL_KNOWN_GROUP,
               ptstrName                => Win32.AccCtrl.To_LPSTR (ESID))));

      --  Add the permissons in corresponding grfAccessPermissions

      for P in Permission'Range  loop
         if Permission (P) then
            EA (PWP.Masks_P2W (P).Group).grfAccessPermissions :=
              EA (PWP.Masks_P2W (P).Group).grfAccessPermissions
              or PWP.Masks_P2W (P).Mask;
         end if;
      end loop;

      --  Set back the explicit access

      Ret := Win32.Aclapi.SetEntriesInAcl
        (cCountOfExplicitEntries => EA'Length,
         pListOfExplicitEntries  => EA (EA'First)'Unchecked_Access,
         OldAcl                  => null,
         NewAcl                  => DACL'Access);

      POSIX_Win32.Check_Retcode (Ret, "Change_Permissions.SetEntriesInAcl");

      Ret := Win32.Aclapi.SetNamedSecurityInfo
        (Win32.Addr (L_Pathname),
         Win32.AccCtrl.SE_FILE_OBJECT,
         Win32.Winnt.DACL_SECURITY_INFORMATION,
         System.Null_Address,
         System.Null_Address,
         DACL,
         null);

      POSIX_Win32.Check_Retcode
        (Ret, "Change_Permissions.SetNamedSecurityInfo");
   end Change_Permissions;

   ----------------------
   -- Create_Directory --
   ----------------------

   procedure Create_Directory
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set)
   is
      pragma Warnings (Off, Permission);

      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.NUL;
      Result     : Win32.BOOL;
   begin
      Result := Win32.Winbase.CreateDirectory
        (Win32.Addr (L_Pathname),
         null); --  Security Attributes
      POSIX_Win32.Check_Result (Result, "Create_Directory");
   end Create_Directory;

   -----------------
   -- Create_FIFO --
   -----------------

   procedure Create_FIFO
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set)
   is
      pragma Warnings (Off, Pathname);
      pragma Warnings (Off, Permission);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Create_FIFO");
   end Create_FIFO;

   ---------------
   -- Existence --
   ---------------

   function Existence (Pathname : POSIX.Pathname) return POSIX.Error_Code is
   begin
      if Is_File_Present (Pathname) then
         return POSIX.No_Error;
      else
         POSIX.Set_Error_Code (POSIX.Error_Code (Win32.Winbase.GetLastError));
         return POSIX.Get_Error_Code;
      end if;
   end Existence;

   -----------------
   -- Filename_Of --
   -----------------

   function Filename_Of
     (D_Entry : Directory_Entry) return POSIX.Filename is
   begin
      return POSIX.To_POSIX_String
        (Interfaces.C.To_Ada (Win32.To_C (D_Entry.cFileName)));
   end Filename_Of;

   -------------------------------
   -- For_Every_Directory_Entry --
   -------------------------------

   procedure For_Every_Directory_Entry
     (Pathname : POSIX.Pathname)
   is
      use type Win32.INT;
      use type Win32.DWORD;
      use type Win32.BOOL;
      use type Win32.Winnt.HANDLE;

      Result     : Win32.BOOL;
      Data       : aliased Win32.Winbase.WIN32_FIND_DATA;
      L_Pathname : String :=
        POSIX.To_String (Pathname) & "\*" & ASCII.NUL;
      Handle     : Win32.Winnt.HANDLE;
      Quit       : Boolean := False;

   begin
      --  if pathname included a terminal '/' or '\', L_pathname will end
      --  in "/\*" or "\\*", which is not accepted. Fix that.

      if L_Pathname (L_Pathname'Last - 3) = '/'
        or else L_Pathname (L_Pathname'Last - 3) = '\'
      then
         L_Pathname (L_Pathname'Last - 2 .. L_Pathname'Last - 1)
           := '*' & ASCII.NUL;
      end if;

      if not Is_File_Present (Pathname) then
         POSIX_Win32.Raise_Error
           ("For_Every_Directory_Entry", POSIX.No_Such_File_Or_Directory);
      end if;

      Handle := Win32.Winbase.FindFirstFile
        (Win32.Addr (L_Pathname), Data'Unchecked_Access);

      if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
         if Win32.Winbase.GetLastError =
           Win32.Winerror.ERROR_FILE_NOT_FOUND then
            --  No file to be scanned
            return;
         else
            POSIX_Win32.Raise_Error
              ("For_Every_Directory_Entry", POSIX.Not_A_Directory);
         end if;
      end if;

      loop
         Action  (Directory_Entry (Data), Quit);
         exit when Quit;

         Result := Win32.Winbase.FindNextFile (Handle,
                                               Data'Unchecked_Access);
         exit when Result = Win32.FALSE;
      end loop;

      Result := Win32.Winbase.FindClose (Handle);
      POSIX_Win32.Check_Result (Result, "For_Every_Directory_Entry");
   end For_Every_Directory_Entry;

   -------------------
   -- Is_Accessible --
   -------------------

   function Is_Accessible
     (Pathname    : POSIX.Pathname;
      Access_Mode : Access_Mode_Set) return Boolean
   is
      use type POSIX.Error_Code;
   begin
      return Accessibility (Pathname, Access_Mode) = POSIX.No_Error;
   end Is_Accessible;

   ---------------------------
   -- Is_Block_Special_File --
   ---------------------------

   function Is_Block_Special_File
     (Pathname : POSIX.Pathname) return Boolean
   is
      use POSIX.File_Status;
   begin
      return Is_Block_Special_File (Get_File_Status (Pathname));
   exception
      when POSIX_Error =>
         --  No such file
         return False;
   end Is_Block_Special_File;

   -------------------------------
   -- Is_Character_Special_File --
   -------------------------------

   function Is_Character_Special_File
     (Pathname : POSIX.Pathname) return Boolean
   is
      use POSIX.File_Status;
   begin
      return Is_Character_Special_File (Get_File_Status (Pathname));
   exception
      when POSIX_Error =>
         --  No such file
         return False;
   end Is_Character_Special_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Pathname : POSIX.Pathname) return Boolean is
      use POSIX.File_Status;
   begin
      return Is_Directory (Get_File_Status (Pathname));
   exception
      when POSIX_Error =>
         --  No such file
         return False;
   end Is_Directory;

   -------------
   -- Is_FIFO --
   -------------

   function Is_FIFO (Pathname : POSIX.Pathname) return Boolean is
      use POSIX.File_Status;
   begin
      return Is_FIFO (Get_File_Status (Pathname));
   exception
      when POSIX_Error =>
         --  No such file
         return False;
   end Is_FIFO;

   -------------
   -- Is_File --
   -------------

   function Is_File (Pathname : POSIX.Pathname) return Boolean is
      use POSIX.File_Status;
   begin
      return Is_Regular_File (Get_File_Status (Pathname));
   exception
      when POSIX_Error =>
         --  No such file
         return False;
   end Is_File;

   ---------------------
   -- Is_File_Present --
   ---------------------

   function Is_File_Present (Pathname : POSIX.Pathname) return Boolean is
      use type Win32.DWORD;
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.NUL;
      Retcode    : Win32.DWORD;
   begin
      Retcode := Win32.Winbase.GetFileAttributes (Win32.Addr (L_Pathname));

      return Retcode /= 16#FFFF_FFFF#;
   end Is_File_Present;

   ---------------
   -- Is_Socket --
   ---------------

   function Is_Socket (Pathname : POSIX.Pathname) return Boolean is
      pragma Warnings (Off, Pathname);
   begin
      --  No such thing on Windows
      return False;
   end Is_Socket;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (Pathname : POSIX.Pathname) return Boolean is
      pragma Warnings (Off, Pathname);
   begin
      --  No such thing on Windows
      return False;
   end Is_Symbolic_Link;

   ----------
   -- Link --
   ----------

   procedure Link
     (Old_Pathname : POSIX.Pathname;
      New_Pathname : POSIX.Pathname)
   is
      Result         : Win32.BOOL;
      L_Old_Pathname : constant String :=
                         POSIX.To_String (Old_Pathname) & ASCII.NUL;
      L_New_Pathname : constant String :=
                         POSIX.To_String (New_Pathname) & ASCII.NUL;
   begin
      Result := Win32.Winbase.CopyFile
        (Win32.Addr (L_Old_Pathname),
         Win32.Addr (L_New_Pathname), Win32.FALSE);
      POSIX_Win32.Check_Result (Result, "Link");
   end Link;

   ----------------------
   -- Remove_Directory --
   ----------------------

   procedure Remove_Directory (Pathname : POSIX.Pathname) is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.NUL;
      Result     : Win32.BOOL;
   begin
      Result := Win32.Winbase.RemoveDirectory (Win32.Addr (L_Pathname));
      POSIX_Win32.Check_Result (Result, "Remove_Directory");
   end Remove_Directory;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (Old_Pathname : POSIX.Pathname;
      New_Pathname : POSIX.Pathname)
   is
      Result         : Win32.BOOL;
      L_Old_Pathname : constant String :=
                         POSIX.To_String (Old_Pathname) & ASCII.NUL;
      L_New_Pathname : constant String :=
                         POSIX.To_String (New_Pathname) & ASCII.NUL;
   begin
      Result := Win32.Winbase.MoveFile
        (Win32.Addr (L_Old_Pathname),
         Win32.Addr (L_New_Pathname));
      POSIX_Win32.Check_Result (Result, "Rename");
   end Rename;

   --------------------
   -- Set_File_Times --
   --------------------

   procedure Set_File_Times
     (Pathname          : POSIX.Pathname;
      Access_Time       : POSIX.Calendar.POSIX_Time;
      Modification_Time : POSIX.Calendar.POSIX_Time)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.NUL;
      Handle : Win32.Winnt.HANDLE;

      Result                        : Win32.BOOL;
      Access_System_Time            : aliased Win32.Winbase.SYSTEMTIME;
      Modification_System_Time      : aliased Win32.Winbase.SYSTEMTIME;
      Current_Access_FileTime       : aliased Win32.Winbase.FILETIME;
      Current_Modification_FileTime : aliased Win32.Winbase.FILETIME;
      Current_Creation_FileTime     : aliased Win32.Winbase.FILETIME;
      UTC_Access_FileTime           : aliased Win32.Winbase.FILETIME;
      UTC_Modification_FileTime     : aliased Win32.Winbase.FILETIME;

   begin
      Access_System_Time       := To_SYSTEMTIME (Access_Time);
      Modification_System_Time := To_SYSTEMTIME (Modification_Time);

      Handle := Win32.Winbase.CreateFile
        (Win32.Addr (L_Pathname),
         Win32.Winnt.GENERIC_WRITE,
         0,
         null, --  Security Attributes
         Win32.Winbase.OPEN_EXISTING,
         Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
         POSIX_Win32.Null_Handle);

      Result := Win32.Winbase.GetFileTime
        (Handle,
         Current_Creation_FileTime'Unchecked_Access,
         Current_Access_FileTime'Unchecked_Access,
         Current_Modification_FileTime'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");

      Result := Win32.Winbase.SystemTimeToFileTime
        (Access_System_Time'Unchecked_Access,
         Current_Access_FileTime'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");
      Result := Win32.Winbase.LocalFileTimeToFileTime
        (Current_Access_FileTime'Unchecked_Access,
         UTC_Access_FileTime'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");

      Result := Win32.Winbase.SystemTimeToFileTime
        (Modification_System_Time'Unchecked_Access,
         Current_Modification_FileTime'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");
      Result := Win32.Winbase.LocalFileTimeToFileTime
        (Current_Modification_FileTime'Unchecked_Access,
         UTC_Modification_FileTime'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");

      Result := Win32.Winbase.SetFileTime
        (Handle,
         Current_Creation_FileTime'Unchecked_Access,
         UTC_Access_FileTime'Unchecked_Access,
         UTC_Modification_FileTime'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");

      Result := Win32.Winbase.CloseHandle (Handle);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");
   end Set_File_Times;

   --------------------
   -- Set_File_Times --
   --------------------

   procedure Set_File_Times (Pathname : POSIX.Pathname) is
      Current_Time : POSIX.Calendar.POSIX_Time;
   begin
      Current_Time := POSIX.Calendar.Clock;
      Set_File_Times (Pathname, Current_Time, Current_Time);
   end Set_File_Times;

   ------------
   -- Unlink --
   ------------

   procedure Unlink (Pathname : POSIX.Pathname) is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.NUL;
      Result     : Win32.BOOL;
   begin
      Result := Win32.Winbase.DeleteFile (Win32.Addr (L_Pathname));
      POSIX_Win32.Check_Result (Result, "Unlink");
   end Unlink;

end POSIX.Files;
