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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Pointers;
with System;

with Win32.AccCtrl;
with Win32.Aclapi;
with Win32.Windef;

with POSIX_Win32;
with POSIX_Win32.File_Handle;
with POSIX_Win32.Permissions;

package body POSIX.File_Status is

   Epoch : aliased Win32.Winbase.FILETIME;
   --  Oldest date for DOS, used for times of /
   --  Must be a variable, because initialized in statements parts of package
   --  elaboration.

   function To_POSIX_Time is new Ada.Unchecked_Conversion
     (Win32.Winbase.SYSTEMTIME, POSIX.Calendar.POSIX_Time);

   Epoch_System_Time : aliased Win32.Winbase.SYSTEMTIME :=
                         (wYear      => 1980,
                          wMonth     => 01,
                          wDayOfWeek => 03, -- Tuesday
                          wDay       => 01,
                          others     => 0); -- Time

   procedure Get_Shared_Data (File_Status : Status);
   --  Retreive the owner/group for the give file

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (File_Status : in out Status) is
   begin
      File_Status.Data.Ref_Count := File_Status.Data.Ref_Count + 1;
   end Adjust;

   ------------------
   -- Device_ID_Of --
   ------------------

   function Device_ID_Of (File_Status : Status) return Device_ID is
      pragma Unreferenced (File_Status);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("File_ID_Of");
      return 0; -- to please compiler
   end Device_ID_Of;

   ----------------
   -- File_ID_Of --
   ----------------

   function File_ID_Of (File_Status : Status) return File_ID is
      pragma Unreferenced (File_Status);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("File_ID_Of");
      return (0, 0); -- to please compiler
   end File_ID_Of;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (File_Status : in out Status) is
      use type Win32.Winnt.PSID;

      procedure Unchecked_Free is
        new Unchecked_Deallocation (Shared_Data, Shared_Data_Access);

      V : Win32.PVOID;
      pragma Unreferenced (V);

      D : Shared_Data_Access := File_Status.Data;

   begin
      File_Status.Data := null;
      if D /= null then
         D.Ref_Count := D.Ref_Count - 1;

         if D.Ref_Count = 0 then
            if D.Owner /= System.Null_Address then
               V := Win32.Winbase.FreeSid (D.Owner);
            end if;

            if D.Group /= System.Null_Address then
               V := Win32.Winbase.FreeSid (D.Group);
            end if;

            Unchecked_Free (D);
         end if;
      end if;
   end Finalize;

   ---------------------
   -- Get_File_Status --
   ---------------------

   function Get_File_Status (Pathname : POSIX.Pathname) return Status is
      use type Win32.DWORD, Win32.Winnt.HANDLE;

      Result          : Win32.BOOL with Unreferenced;
      L_Pathname      : String := POSIX.To_String (Pathname) & ASCII.NUL;
      File_Attributes : Win32.DWORD;
      Handle          : Win32.Winnt.HANDLE;
      File_Status     : Status;
      Find_Data       : aliased Win32.Winbase.WIN32_FIND_DATA;
   begin
      --  Eliminate file names containing wildcard characters, or subsequent
      --  call to FindFirstFile will expand them, matching some other file.

      for I in Pathname'Range loop
         case Pathname (I) is
            when '*' | '?' =>
               POSIX_Win32.Raise_Error
                 ("Get_File_Status (Pathname)", No_Such_File_Or_Directory);
            when others =>
               null;
         end case;
      end loop;

      --  Eliminate trailing / (or \), or FindFirstFile will not work
      --  But keep it if it refers to a root directory.

      if Pathname (Pathname'Last) = '/'
        or else Pathname (Pathname'Last) = '\'
      then
         if Pathname'Length > 1
           and then Pathname (Pathname'Last - 1) /= ':'
         then
            L_Pathname (L_Pathname'Last - 1) := ASCII.NUL;
         end if;
      end if;

      Handle := Win32.Winbase.FindFirstFile
        (Win32.Addr (L_Pathname), Find_Data'Unchecked_Access);

      if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
         --  FindFirstFile doesn't work on root directories, so call
         --  GetFileAttributes to see if the specified file exists.

         File_Attributes :=
           Win32.Winbase.GetFileAttributes (Win32.Addr (L_Pathname));

         if File_Attributes = 16#FFFF_FFFF# then
            POSIX_Win32.Raise_Error
              ("Get_File_Status (Pathname)", No_Such_File_Or_Directory);

         else
            --  Make up some fake information for this file.  It has the
            --  correct file attributes but times are unknown.

            File_Status :=
              (Finalization.Controlled with
               File             => 0,
               File_Name        => To_Unbounded_String (To_String (Pathname)),
               Is_Executable    => False,
               File_Attributes  => File_Attributes,
               Creation_Time    => Epoch,
               Last_Access_Time => Epoch,
               Last_Write_Time  => Epoch,
               File_Size_Low    => 0,
               File_Size_High   => 0,
               File_Links       => 1,
               File_Type        => Win32.Winbase.FILE_TYPE_DISK,
               Data             => new Shared_Data'
                 (System.Null_Address, System.Null_Address, null, 2));
         end if;

      else
         File_Status :=
           (Finalization.Controlled with
            File             => 0,
            File_Name        => To_Unbounded_String (To_String (Pathname)),
            Is_Executable    => POSIX_Win32.Is_Executable (Pathname),
            File_Attributes  => Find_Data.dwFileAttributes,
            Creation_Time    => Find_Data.ftCreationTime,
            Last_Access_Time => Find_Data.ftLastAccessTime,
            Last_Write_Time  => Find_Data.ftLastWriteTime,
            File_Size_Low    => Find_Data.nFileSizeLow,
            File_Size_High   => Find_Data.nFileSizeHigh,
            File_Links       => 1,
            File_Type        => Win32.Winbase.FILE_TYPE_DISK,
            Data             => new Shared_Data'
              (System.Null_Address, System.Null_Address, null, 2));

         Result := Win32.Winbase.FindClose (Handle);
      end if;

      return File_Status;
   end Get_File_Status;

   ---------------------
   -- Get_File_Status --
   ---------------------

   function Get_File_Status
     (File : POSIX.IO.File_Descriptor) return Status
   is
      Handle           : Win32.Winnt.HANDLE;
      Result           : Win32.BOOL;
      File_Information : aliased Win32.Winbase.BY_HANDLE_FILE_INFORMATION;
   begin
      Handle := POSIX_Win32.File_Handle.Get (File);
      Result := Win32.Winbase.GetFileInformationByHandle
        (Handle, File_Information'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Get_File_Status (File_Descriptor)");

      return
        (Finalization.Controlled with
         File             => File,
         File_Name        => Null_Unbounded_String,
         File_Attributes  => File_Information.dwFileAttributes,
         Is_Executable    => False,   -- No way to know at that point...
         Creation_Time    => File_Information.ftCreationTime,
         Last_Access_Time => File_Information.ftLastAccessTime,
         Last_Write_Time  => File_Information.ftLastWriteTime,
         File_Size_Low    => File_Information.nFileSizeLow,
         File_Size_High   => File_Information.nFileSizeHigh,
         File_Links       => File_Information.nNumberOfLinks,
         File_Type        => Win32.Winbase.GetFileType (Handle),
         Data             => new Shared_Data'
            (System.Null_Address, System.Null_Address, null, 2));
   end Get_File_Status;

   ---------------------
   -- Get_Shared_Data --
   ---------------------

   procedure Get_Shared_Data (File_Status : Status) is
      use type Win32.DWORD;
      use type Win32.Winnt.HANDLE;
      use type Win32.Winnt.PACL;
      use type Win32.Winnt.SECURITY_INFORMATION;

      Handle : Win32.Winnt.HANDLE;
      H      : Win32.Windef.HLOCAL with Unreferenced;
      Close  : Boolean := False;
      Res    : Win32.BOOL with Unreferenced;
      Ret    : Win32.DWORD;
      SD     : aliased Win32.Winnt.PSECURITY_DESCRIPTOR;
   begin
      if File_Status.Data = null then
         POSIX_Win32.Raise_Error
           ("Get_Shared_Data, status invalid", POSIX.Invalid_Argument);

      elsif File_Status.Data.Owner = System.Null_Address
        or else File_Status.Data.Group = System.Null_Address
      then
         if File_Status.File_Name = Null_Unbounded_String then
            Handle := POSIX_Win32.File_Handle.Get (File_Status.File);

         else
            declare
               L_Name : constant String :=
                          To_String (File_Status.File_Name) & ASCII.NUL;
            begin
               Handle := Win32.Winbase.CreateFile
                 (lpFileName            => Win32.Addr (L_Name),
                  dwDesiredAccess       => Win32.Winnt.READ_CONTROL,
                  dwShareMode           => Win32.Winnt.FILE_SHARE_READ,
                  lpSecurityAttributes  => null,
                  dwCreationDisposition => Win32.Winbase.OPEN_EXISTING,
                  dwFlagsAndAttributes  => Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
                  hTemplateFile         => POSIX_Win32.Null_Handle);
               Close := True;
            end;
         end if;

         if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
            --  No enough rights to open the file
            return;
         end if;

         Ret := Win32.Aclapi.GetSecurityInfo
           (Handle,
            Win32.AccCtrl.SE_FILE_OBJECT,
            Win32.Winnt.OWNER_SECURITY_INFORMATION
              + Win32.Winnt.GROUP_SECURITY_INFORMATION
              + Win32.Winnt.DACL_SECURITY_INFORMATION,
            File_Status.Data.Owner'Access,
            File_Status.Data.Group'Access,
            File_Status.Data.DACL'Access,
            null,
            SD'Access);

         H := Win32.Winbase.LocalFree (SD);

         POSIX_Win32.Check_Retcode (Ret, "Get_Owner_Group_Of.GetSecurityInfo");

         if Close then
            Res := Win32.Winbase.CloseHandle (Handle);
         end if;
      end if;
   end Get_Shared_Data;

   --------------
   -- Group_Of --
   --------------

   function Group_Of
     (File_Status : Status) return POSIX.Process_Identification.Group_ID is
   begin
      Get_Shared_Data (File_Status);

      return Process_Identification.Value
        (POSIX_Win32.To_String (File_Status.Data.Group));
   end Group_Of;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (File_Status : in out Status) is
   begin
      File_Status.Data :=
        new Shared_Data'(System.Null_Address, System.Null_Address, null, 1);
   end Initialize;

   ---------------------------
   -- Is_Block_Special_File --
   ---------------------------

   function Is_Block_Special_File (File_Status : Status) return Boolean is
      pragma Unreferenced (File_Status);
   begin
      return False;
   end Is_Block_Special_File;

   -------------------------------
   -- Is_Character_Special_File --
   -------------------------------

   function Is_Character_Special_File
     (File_Status : Status) return Boolean
   is
      use type Win32.DWORD;
   begin
      return File_Status.File_Type = Win32.Winbase.FILE_TYPE_CHAR;
   end Is_Character_Special_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (File_Status : Status) return Boolean is
      use type Win32.DWORD;
   begin
      return (File_Status.File_Attributes
              and Win32.Winnt.FILE_ATTRIBUTE_DIRECTORY) /= 0;
   end Is_Directory;

   -------------
   -- Is_FIFO --
   -------------

   function Is_FIFO (File_Status : Status) return Boolean is
      pragma Unreferenced (File_Status);
   begin
      return False;
   end Is_FIFO;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File_Status : Status) return Boolean is
      use type Win32.DWORD;
      --  There seem to be incompatibilities in the various FILE_ATTRIBUTES
      --  between versions of Windows. It seems simpler to assume that a
      --  regular file is a disk file that is not a directory.
   begin
      return File_Status.File_Type = Win32.Winbase.FILE_TYPE_DISK
        and not Is_Directory (File_Status);
   end Is_Regular_File;

   -------------------------
   -- Last_Access_Time_Of --
   -------------------------

   function Last_Access_Time_Of
     (File_Status : Status) return POSIX.Calendar.POSIX_Time
   is
      Result           : Win32.BOOL with Unreferenced;
      System_Time      : aliased Win32.Winbase.SYSTEMTIME;
      Last_Access_Time : aliased Win32.Winbase.FILETIME;
      Local_Time       : aliased Win32.Winbase.FILETIME;
   begin
      Last_Access_Time := File_Status.Last_Access_Time;
      Result := Win32.Winbase.FileTimeToLocalFileTime
        (Last_Access_Time'Unchecked_Access, Local_Time'Unchecked_Access);
      Result := Win32.Winbase.FileTimeToSystemTime
        (Local_Time'Unchecked_Access, System_Time'Unchecked_Access);
      return To_POSIX_Time (System_Time);
   end Last_Access_Time_Of;

   -------------------------------
   -- Last_Modification_Time_Of --
   -------------------------------

   function Last_Modification_Time_Of
     (File_Status : Status) return POSIX.Calendar.POSIX_Time
   is
      use type Win32.BOOL;
      Result          : Win32.BOOL;
      System_Time     : aliased Win32.Winbase.SYSTEMTIME;
      Last_Write_Time : aliased Win32.Winbase.FILETIME;
      Local_Time      : aliased Win32.Winbase.FILETIME;
   begin
      Last_Write_Time := File_Status.Last_Write_Time;
      Result := Win32.Winbase.FileTimeToLocalFileTime
        (Last_Write_Time'Unchecked_Access, Local_Time'Unchecked_Access);

      if Result = Win32.TRUE then
         Result := Win32.Winbase.FileTimeToSystemTime
           (Local_Time'Unchecked_Access, System_Time'Unchecked_Access);
      end if;

      POSIX_Win32.Check_Result (Result, "Last_Modification_Time_Of");

      return To_POSIX_Time (System_Time);
   end Last_Modification_Time_Of;

   --------------------------------
   -- Last_Status_Change_Time_Of --
   --------------------------------

   function Last_Status_Change_Time_Of
     (File_Status : Status) return POSIX.Calendar.POSIX_Time is
   begin
      return Last_Modification_Time_Of (File_Status);
   end Last_Status_Change_Time_Of;

   -------------------
   -- Link_Count_Of --
   -------------------

   function Link_Count_Of (File_Status : Status) return Links is
   begin
      return Links (File_Status.File_Links);
   end Link_Count_Of;

   --------------
   -- Owner_Of --
   --------------

   function Owner_Of
     (File_Status : Status) return POSIX.Process_Identification.User_ID is
   begin
      Get_Shared_Data (File_Status);

      return Process_Identification.Value
        (POSIX_Win32.To_String (File_Status.Data.Owner));
   end Owner_Of;

   -----------------------
   -- Permission_Set_Of --
   -----------------------

   function Permission_Set_Of
     (File_Status : Status) return POSIX.Permissions.Permission_Set
   is
      use type Win32.AccCtrl.TRUSTEE_FORM;
      use type Win32.DWORD;
      use type Win32.Winnt.PACL;
      use type Win32.Winnt.PSID;

      use POSIX.Permissions;

      PS  : POSIX.Permissions.Permission_Set := (others => False);
      PEA : aliased Win32.AccCtrl.PEXPLICIT_ACCESS;
      Len : aliased Win32.ULONG;
      Ret : Win32.DWORD;
   begin
      Get_Shared_Data (File_Status);

      --  Note that PEA is a pointer to the first element of an array of
      --  EXPLICIT_ACCESS.

      Ret := Win32.Aclapi.GetExplicitEntriesFromAcl
        (File_Status.Data.DACL, Len'Unchecked_Access, PEA'Access);

      POSIX_Win32.Check_Retcode
        (Ret, "Permission_Set_Of.GetExplicitEntriesFromAcl");

      if Len = 0 then
         --  No explicit DACL, just use the standard attributes in this case

         PS (Owner_Read)  := True;
         PS (Group_Read)  := True;
         PS (Others_Read) := True;

         if (File_Status.File_Attributes
             and Win32.Winnt.FILE_ATTRIBUTE_READONLY) = 0
         then
            PS (Owner_Write)  := True;
            PS (Group_Write)  := True;
            PS (Others_Write) := True;
         end if;

         if File_Status.Is_Executable then
            PS (Owner_Execute)  := True;
            PS (Group_Execute)  := True;
            PS (Others_Execute) := True;
         end if;

      else
         declare
            use type Win32.BOOL;

            procedure Set
              (Permission        : Permissions.Permission;
               AccessPermissions : Win32.DWORD;
               Mask              : Win32.DWORD;
               Access_Mode       : Win32.AccCtrl.ACCESS_MODE) with Inline;
            --  Set the given permission if Mask is set in AccessPermissions

            ---------
            -- Set --
            ---------

            procedure Set
              (Permission        : Permissions.Permission;
               AccessPermissions : Win32.DWORD;
               Mask              : Win32.DWORD;
               Access_Mode       : Win32.AccCtrl.ACCESS_MODE)
            is
               use type Win32.AccCtrl.ACCESS_MODE;
            begin
               if (AccessPermissions and Mask) /= 0 then
                  if Access_Mode = Win32.AccCtrl.GRANT_ACCESS then
                     PS (Permission) := True;
                  elsif Access_Mode = Win32.AccCtrl.DENY_ACCESS then
                     PS (Permission) := False;
                  end if;
               end if;
            end Set;

            Empty : constant Win32.AccCtrl.EXPLICIT_ACCESS :=
                      (0, Win32.AccCtrl.NOT_USED_ACCESS, 0, vTrustee => <>);

            type EXPLICIT_ACCESS_Array is array (Positive range <>)
              of aliased Win32.AccCtrl.EXPLICIT_ACCESS;

            package EAA is new Interfaces.C.Pointers
              (Positive, Win32.AccCtrl.EXPLICIT_ACCESS,
               EXPLICIT_ACCESS_Array, Empty);

            P     : EAA.Pointer := EAA.Pointer (PEA);
            --  P is a pointer to the first EXPLICIT_ACCESS in the array
            --  containing Len of them. This pointer will be used to go through
            --  the underlying C array.

         begin
            --  Loop on all EXPLICIT_ACCESS structure and set the corresponding
            --  permissions in the given order.

            for K in 1 .. Integer (Len) loop
               --  Only handle SID style trustee, other forms are not
               --  interresting for this implementation.

               if P.vTrustee.TrusteeForm = Win32.AccCtrl.TRUSTEE_IS_SID then
                  declare
                     SID : constant Win32.Winnt.PSID :=
                             Win32.AccCtrl.To_PSID (P.vTrustee.ptstrName);
                     OI  : POSIX_Win32.Permissions.UGO :=
                             POSIX_Win32.Permissions.U;
                  begin
                     if Win32.Winbase.IsValidSid (SID) = Win32.TRUE then
                        if Win32.Winbase.EqualSid (SID, File_Status.Data.Owner)
                          = Win32.TRUE
                        then
                           --  This is the owner
                           OI := POSIX_Win32.Permissions.U;

                        elsif Win32.Winbase.EqualSid
                          (SID, File_Status.Data.Group) = Win32.TRUE
                        then
                           --  This is the group
                           OI := POSIX_Win32.Permissions.G;

                        elsif POSIX_Win32.To_String (SID)
                          = POSIX_Win32.Everyone_SID
                        then
                           --  This is everyone
                           OI := POSIX_Win32.Permissions.O;
                        end if;

                        for Mode in POSIX_Win32.Permissions.RWX'Range loop
                           Set (POSIX_Win32.Permissions.Masks_W2P
                                (OI, Mode).Perm,
                                P.grfAccessPermissions,
                                POSIX_Win32.Permissions.Masks_W2P
                                  (OI, Mode).Mask,
                                P.grfAccessMode);
                        end loop;
                     end if;
                  end;
               end if;
               EAA.Increment (P);
            end loop;
         end;
      end if;

      return PS;
   end Permission_Set_Of;

   -------------
   -- Size_Of --
   -------------

   function Size_Of (File_Status : Status) return POSIX.IO_Count is
   begin
      return POSIX.IO_Count (File_Status.File_Size_Low);
   end Size_Of;

begin
   declare
      Ignored : Win32.BOOL with Unreferenced;
   begin
      Ignored := Win32.Winbase.SystemTimeToFileTime
        (Epoch_System_Time'Access, Epoch'Access);
   end;
end POSIX.File_Status;
