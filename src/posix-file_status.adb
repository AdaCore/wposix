------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
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

with Ada.Unchecked_Conversion;

with Win32.Winnt;

with POSIX_Win32;
with POSIX_Win32.File_Handle;

package body POSIX.File_Status is

   --  Operations to Obtain File Status

   Epoch : aliased Win32.Winbase.FILETIME;
   --  Oldest date for DOS, used for times of /
   --  Must be a variable, because initialized in statements parts of package
   --  elaboration.

   function To_POSIX_Time is new Ada.Unchecked_Conversion
     (Win32.Winbase.SYSTEMTIME, POSIX.Calendar.POSIX_Time);

   Epoch_System_Time : aliased Win32.Winbase.SYSTEMTIME :=
                         (WYear      => 1980,
                          WMonth     => 01,
                          WDayOfWeek => 03, -- Tuesday
                          WDay       => 01,
                          others     => 0); -- Time

   ------------------
   -- Device_ID_Of --
   ------------------

   function Device_ID_Of (File_Status : in Status) return Device_ID is
      pragma Warnings (Off, File_Status);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("File_ID_Of");
      return 0; -- to please compiler
   end Device_ID_Of;

   ----------------
   -- File_ID_Of --
   ----------------

   function File_ID_Of (File_Status : in Status) return File_ID is
      pragma Warnings (Off, File_Status);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("File_ID_Of");
      return (0, 0); -- to please compiler
   end File_ID_Of;

   ---------------------
   -- Get_File_Status --
   ---------------------

   function Get_File_Status (Pathname : in POSIX.Pathname) return Status is
      use type Win32.DWORD, Win32.Winnt.HANDLE;

      Result          : Win32.BOOL;
      pragma Unreferenced (Result);
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

            File_Status := (Is_Executable    => False,
                            File_Attributes  => File_Attributes,
                            Creation_Time    => Epoch,
                            Last_Access_Time => Epoch,
                            Last_Write_Time  => Epoch,
                            File_Size_Low    => 0,
                            File_Size_High   => 0,
                            File_Links       => 1,
                            File_Type        => Win32.Winbase.FILE_TYPE_DISK);
         end if;

      else
         File_Status :=
           (Is_Executable    => POSIX_Win32.Is_Executable (Pathname),
            File_Attributes  => Find_Data.dwFileAttributes,
            Creation_Time    => Find_Data.ftCreationTime,
            Last_Access_Time => Find_Data.ftLastAccessTime,
            Last_Write_Time  => Find_Data.ftLastWriteTime,
            File_Size_Low    => Find_Data.nFileSizeLow,
            File_Size_High   => Find_Data.nFileSizeHigh,
            File_Links       => 1,
            File_Type        => Win32.Winbase.FILE_TYPE_DISK);

         Result := Win32.Winbase.FindClose (Handle);
      end if;

      return File_Status;
   end Get_File_Status;

   ---------------------
   -- Get_File_Status --
   ---------------------

   function Get_File_Status
     (File : in POSIX.IO.File_Descriptor) return Status
   is
      Handle           : Win32.Winnt.HANDLE;
      Result           : Win32.BOOL;
      File_Information : aliased Win32.Winbase.BY_HANDLE_FILE_INFORMATION;
   begin
      Handle := POSIX_Win32.File_Handle.Get (File);
      Result := Win32.Winbase.GetFileInformationByHandle
        (Handle, File_Information'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Get_File_Status (File_Descriptor)");

      return (File_Attributes  => File_Information.dwFileAttributes,
              Is_Executable    => False,   -- No way to know at that point...
              Creation_Time    => File_Information.ftCreationTime,
              Last_Access_Time => File_Information.ftLastAccessTime,
              Last_Write_Time  => File_Information.ftLastWriteTime,
              File_Size_Low    => File_Information.nFileSizeLow,
              File_Size_High   => File_Information.nFileSizeHigh,
              File_Links       => File_Information.nNumberOfLinks,
              File_Type        => Win32.Winbase.GetFileType (Handle));
   end Get_File_Status;

   --------------
   -- Group_Of --
   --------------

   function Group_Of
     (File_Status : in Status) return POSIX.Process_Identification.Group_ID
   is
      pragma Warnings (Off, File_Status);
   begin
      return POSIX.Process_Identification.Get_Real_Group_ID;
   end Group_Of;

   ---------------------------
   -- Is_Block_Special_File --
   ---------------------------

   function Is_Block_Special_File (File_Status : in Status) return Boolean is
      pragma Warnings (Off, File_Status);
   begin
      return False;
   end Is_Block_Special_File;

   -------------------------------
   -- Is_Character_Special_File --
   -------------------------------

   function Is_Character_Special_File
     (File_Status : in Status) return Boolean
   is
      use type Win32.DWORD;
   begin
      return File_Status.File_Type = Win32.Winbase.FILE_TYPE_CHAR;
   end Is_Character_Special_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (File_Status : in Status) return Boolean is
      use type Win32.DWORD;
   begin
      return (File_Status.File_Attributes and
              Win32.Winnt.FILE_ATTRIBUTE_DIRECTORY) /= 0;
   end Is_Directory;

   -------------
   -- Is_FIFO --
   -------------

   function Is_FIFO (File_Status : in Status) return Boolean is
      pragma Warnings (Off, File_Status);
   begin
      return False;
   end Is_FIFO;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File_Status : in Status) return Boolean is
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
     (File_Status : in Status) return POSIX.Calendar.POSIX_Time
   is
      Result           : Win32.BOOL;
      pragma Unreferenced (Result);
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
     (File_Status : in Status) return POSIX.Calendar.POSIX_Time
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
     (File_Status : in Status) return POSIX.Calendar.POSIX_Time is
   begin
      return Last_Modification_Time_Of (File_Status);
   end Last_Status_Change_Time_Of;

   -------------------
   -- Link_Count_Of --
   -------------------

   function Link_Count_Of (File_Status : in Status) return Links is
   begin
      return Links (File_Status.File_Links);
   end Link_Count_Of;

   --------------
   -- Owner_Of --
   --------------

   function Owner_Of
     (File_Status : in Status) return POSIX.Process_Identification.User_ID
   is
      pragma Warnings (Off, File_Status);
   begin
      return POSIX.Process_Identification.Get_Real_User_ID;
   end Owner_Of;

   -----------------------
   -- Permission_Set_Of --
   -----------------------

   function Permission_Set_Of
     (File_Status : in Status) return POSIX.Permissions.Permission_Set
   is
      use POSIX.Permissions;
      use type Win32.DWORD;
      PS : POSIX.Permissions.Permission_Set := (others => False);
   begin
      PS (Owner_Read)  := True;
      PS (Group_Read)  := True;
      PS (Others_Read) := True;

      if (File_Status.File_Attributes and
          Win32.Winnt.FILE_ATTRIBUTE_READONLY) = 0 then
         PS (Owner_Write)  := True;
         PS (Group_Write)  := True;
         PS (Others_Write) := True;
      end if;

      if File_Status.Is_Executable then
         PS (Owner_Execute)  := True;
         PS (Group_Execute)  := True;
         PS (Others_Execute) := True;
      end if;

      return PS;
   end Permission_Set_Of;

   -------------
   -- Size_Of --
   -------------

   function Size_Of (File_Status : in Status) return POSIX.IO_Count is
   begin
      return POSIX.IO_Count (File_Status.File_Size_Low);
   end Size_Of;

begin
   declare
      Ignored : Win32.BOOL;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Win32.Winbase.SystemTimeToFileTime
        (Epoch_System_Time'Access, Epoch'Access);
   end;
end POSIX.File_Status;
