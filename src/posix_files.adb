
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with System;
with Interfaces.C;
with Ada.Unchecked_Conversion;
with Win32.Winnt;
with Win32.Winbase;
with Win32.Winerror;

with POSIX_Win32;
with POSIX_Calendar;
with POSIX_File_Status;

package body POSIX_Files is

   --  Operations to create files in the File System

   ----------------------
   -- Create_Directory --
   ----------------------

   procedure Create_Directory
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX_Permissions.Permission_Set)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      Result     : Win32.BOOL;
   begin
      Result := Win32.Winbase.CreateDirectory
        (Win32.Addr (L_Pathname),
         null --  Security Attributes
         );
      POSIX_Win32.Check_Result (Result, "Create_Directory");
   end Create_Directory;


   -----------------
   -- Create_FIFO --
   -----------------

   procedure Create_FIFO
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX_Permissions.Permission_Set) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Create_FIFO");
   end Create_FIFO;





   --  Operations to remove files from the File System

   ------------
   -- Unlink --
   ------------

   procedure Unlink (Pathname : in POSIX.Pathname) is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      Result     : Win32.BOOL;
   begin
      Result := Win32.Winbase.DeleteFile (Win32.Addr (L_Pathname));
      POSIX_Win32.Check_Result (Result, "Unlink");
   end Unlink;


   ----------------------
   -- Remove_Directory --
   ----------------------

   procedure Remove_Directory (Pathname : in POSIX.Pathname) is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      Result     : Win32.BOOL;
   begin
      Result := Win32.Winbase.RemoveDirectory (Win32.Addr (L_Pathname));
      POSIX_Win32.Check_Result (Result, "Remove_Directory");
   end Remove_Directory;





   --  Predicates on files in the File System

   -------------
   -- Is_File --
   -------------

   function Is_File (Pathname : in POSIX.Pathname)
                     return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Regular_File (Get_File_Status (Pathname));
   exception
      when others =>
         return False;
   end Is_File;


   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Pathname : in POSIX.Pathname)
                          return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Directory (Get_File_Status (Pathname));
   exception
      when others =>
         return False;
   end Is_Directory;


   -------------
   -- Is_FIFO --
   -------------

   function Is_FIFO (Pathname : in POSIX.Pathname)
                     return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_FIFO (Get_File_Status (Pathname));
   exception
      when others =>
         return False;
   end Is_FIFO;


   -------------------------------
   -- Is_Character_Special_File --
   -------------------------------

   function Is_Character_Special_File (Pathname : in POSIX.Pathname)
                                       return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Character_Special_File (Get_File_Status (Pathname));
   exception
      when others =>
         return False;
   end Is_Character_Special_File;


   ---------------------------
   -- Is_Block_Special_File --
   ---------------------------

   function Is_Block_Special_File (Pathname : in POSIX.Pathname)
                                   return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Block_Special_File (Get_File_Status (Pathname));
   exception
      when others =>
         return False;
   end Is_Block_Special_File;





   --  Operations to modify File Pathnames

   ----------
   -- Link --
   ----------

   procedure Link
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname)
   is
      Result         : Win32.BOOL;
      L_Old_Pathname : constant String :=
        POSIX.To_String (Old_Pathname) & ASCII.Nul;
      L_New_Pathname : constant String :=
        POSIX.To_String (New_Pathname) & ASCII.Nul;
   begin
      Result := Win32.Winbase.CopyFile
        (Win32.Addr (L_Old_Pathname),
         Win32.Addr (L_New_Pathname), Win32.FALSE);
      POSIX_Win32.Check_Result (Result, "Link");
   end Link;


   ------------
   -- Rename --
   ------------

   procedure Rename
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname)
   is
      Result         : Win32.BOOL;
      L_Old_Pathname : constant String :=
        POSIX.To_String (Old_Pathname) & ASCII.Nul;
      L_New_Pathname : constant String :=
        POSIX.To_String (New_Pathname) & ASCII.Nul;
   begin
      Result := Win32.Winbase.MoveFile
        (Win32.Addr (L_Old_Pathname),
         Win32.Addr (L_New_Pathname));
      POSIX_Win32.Check_Result (Result, "Rename");
   end Rename;





   --  Iterating over files within a directory

   -----------------
   -- Filename_Of --
   -----------------

   function Filename_Of (D_Entry : Directory_Entry)
                         return POSIX.Filename
   is
   begin
      return POSIX.To_POSIX_String
        (Interfaces.C.To_Ada (Win32.To_C (D_Entry.cFileName)));
   end Filename_Of;


   -------------------------------
   -- For_Every_Directory_Entry --
   -------------------------------

   procedure For_Every_Directory_Entry
     (Pathname : in POSIX.Pathname)
   is
      use type Win32.INT;
      use type Win32.DWORD;
      use type Win32.BOOL;
      use type Win32.Winnt.HANDLE;

      Result     : Win32.BOOL;
      Data       : aliased Win32.Winbase.WIN32_FIND_DATA;
      L_Pathname : constant String :=
        POSIX.To_String (Pathname) & "/*" & ASCII.Nul;
      Handle     : Win32.Winnt.HANDLE;
      Quit       : Boolean := False;

   begin

      if not Is_File_Present (Pathname) then
         POSIX_Win32.Raise_Error ("For_Every_Directory_Entry",
                                  POSIX.No_Such_File_Or_Directory);
      end if;

      Handle := Win32.Winbase.FindFirstFile (Win32.Addr (L_Pathname),
                                             Data'Unchecked_Access);

      if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
         if Win32.Winbase.GetLastError =
           Win32.Winerror.ERROR_FILE_NOT_FOUND then
            --  no file to be scanned
            return;
         else
            POSIX_Win32.Raise_Error ("For_Every_Directory_Entry",
                                     POSIX.Not_A_Directory);
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




   --  Operations to Update File Status Information

   ----------------------------
   -- Change_Owner_And_Group --
   ----------------------------

   procedure Change_Owner_And_Group
     (Pathname : in POSIX.Pathname;
      Owner    : in POSIX_Process_Identification.User_ID;
      Group    : in POSIX_Process_Identification.Group_ID) is
   begin
      null;
   end Change_Owner_And_Group;


   ------------------------
   -- Change_Permissions --
   ------------------------

   procedure Change_Permissions
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX_Permissions.Permission_Set)
   is
      use type Win32.DWORD;
      use POSIX_Permissions;
      Result     : Win32.BOOL;
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      Attributes : Win32.DWORD;
   begin
      Attributes := Win32.Winbase.GetFileAttributes (Win32.Addr (L_Pathname));
      if Attributes = 16#FFFF_FFFF# then
         POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error,
                                    "Change_Permissions");
      end if;

      if Permission (Owner_Read) and then Permission (Owner_Write) then
         Attributes := Attributes and
           (not Win32.Winnt.FILE_ATTRIBUTE_READONLY);
      else
         Attributes := Attributes or Win32.Winnt.FILE_ATTRIBUTE_READONLY;
      end if;

      Result := Win32.Winbase.SetFileAttributes (Win32.Addr (L_Pathname),
                                                 Attributes);
      POSIX_Win32.Check_Result (Result, "Change_Permissions");
   end Change_Permissions;


   -------------------
   -- To_SYSTEMTIME --
   -------------------

   function To_SYSTEMTIME is new Ada.Unchecked_Conversion
     (POSIX_Calendar.POSIX_Time,
      Win32.Winbase.SYSTEMTIME);

   --------------------
   -- Set_File_Times --
   --------------------

   procedure Set_File_Times
     (Pathname          : in POSIX.Pathname;
      Access_Time       : in POSIX_Calendar.POSIX_Time;
      Modification_Time : in POSIX_Calendar.POSIX_Time)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
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
         System.Null_Address);

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

   procedure Set_File_Times (Pathname : in POSIX.Pathname) is
      Current_Time : POSIX_Calendar.POSIX_Time;
   begin
      Current_Time := POSIX_Calendar.Clock;
      Set_File_Times (Pathname, Current_Time, Current_Time);
   end Set_File_Times;




   --  Operations to Determine File Accessibility

   -------------------
   -- Is_Accessible --
   -------------------

   function Is_Accessible
     (Pathname    : in POSIX.Pathname;
      Access_Mode : in Access_Mode_Set)
      return Boolean
   is
      use type POSIX.Error_Code;
   begin
      return Accessibility (Pathname, Access_Mode) = POSIX.No_Error;
   end Is_Accessible;


   -------------------
   -- Accessibility --
   -------------------

   function Accessibility
     (Pathname    : in POSIX.Pathname;
      Access_Mode : in Access_Mode_Set)
      return POSIX.Error_Code
   is
      use POSIX_Permissions;
      F_Perms : POSIX_Permissions.Permission_Set;
      Retcode : POSIX.Error_Code := POSIX.No_Error;
   begin
      if Is_File_Present (Pathname) then
         F_Perms := POSIX_File_Status.Permission_Set_Of
           (POSIX_File_Status.Get_File_Status (Pathname));
         if (Access_Mode (Read_Ok) and then not F_Perms (Owner_Read))
           or else
           (Access_Mode (Write_Ok) and then not F_Perms (Owner_Write))
           or else
           (Access_Mode (Execute_Ok) and then not F_Perms (Owner_Execute)) then
            Retcode := POSIX.Permission_Denied;
         end if;
      else
         Retcode := POSIX.No_Such_File_Or_Directory;
      end if;
      return Retcode;
   end Accessibility;


   ---------------------
   -- Is_File_Present --
   ---------------------

   function Is_File_Present (Pathname : in POSIX.Pathname)
                             return Boolean
   is
      use type Win32.DWORD;
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      Retcode    : Win32.DWORD;
   begin
      Retcode := Win32.Winbase.GetFileAttributes (Win32.Addr (L_Pathname));

      return Retcode /= 16#FFFF_FFFF#;
   end Is_File_Present;


   ---------------
   -- Existence --
   ---------------

   function Existence (Pathname : in POSIX.Pathname)
                       return POSIX.Error_Code is
   begin
      if Is_File_Present (Pathname) then
         return POSIX.No_Error;
      else
         POSIX.Set_Error_Code (POSIX.Error_Code (Win32.Winbase.GetLastError));
         return POSIX.Get_Error_Code;
      end if;
   end Existence;

end POSIX_Files;
