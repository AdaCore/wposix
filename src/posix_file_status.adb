
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Ada.Unchecked_Conversion;
with Win32.Winnt;

with POSIX_Win32;
with POSIX_Win32.File_Handle;
with POSIX_Permissions;
with POSIX_IO;

package body POSIX_File_Status is

   Result  : Win32.BOOL;


   --  Operations to Obtain File Status

   Find_Data : aliased Win32.Winbase.WIN32_FIND_DATA;

   ---------------------
   -- Get_File_Status --
   ---------------------

   function Get_File_Status (Pathname : POSIX.Pathname)
                             return Status
   is
      use type Win32.DWORD;
      L_Pathname      : constant String
        := POSIX.To_String (Pathname) & ASCII.Nul;
      File_Attributes : Win32.DWORD;
      File            : POSIX_IO.File_Descriptor;
      Handle          : Win32.Winnt.Handle;
      File_Status     : Status;
   begin
      File_Attributes := Win32.Winbase.GetFileAttributes
        (Win32.Addr (L_Pathname));

      if (File_Attributes /= 16#FFFF_FFFF#) then

         if (File_Attributes and
             Win32.Winnt.FILE_ATTRIBUTE_DIRECTORY) /= 0 then
            Handle := Win32.Winbase.FindFirstFile
              (Win32.Addr (L_Pathname),
               Find_Data'Access);
            File_Status.Is_Executable  := POSIX_Win32.Is_Executable (Pathname);
            File_Status.File_Attributes  := Find_Data.dwFileAttributes;
            File_Status.Creation_Time    := Find_Data.ftCreationTime;
            File_Status.Last_Access_Time := Find_Data.ftLastAccessTime;
            File_Status.Last_Write_Time  := Find_Data.ftLastWriteTime;
            File_Status.File_Size_Low    := Find_Data.nFileSizeLow;
            File_Status.File_Size_High   := Find_Data.nFileSizeHigh;
         else
            File := POSIX_IO.Open (Pathname, POSIX_IO.Read_Only);
            File_Status := Get_File_Status (File);
            POSIX_IO.Close (File);
            File_Status.Is_Executable := POSIX_Win32.Is_Executable (Pathname);
         end if;
      else
         POSIX_Win32.Check_Result (Win32.FALSE, "Get_File_Status (Pathname)");
      end if;
      return File_Status;
   end Get_File_Status;


   File_Information : aliased Win32.Winbase.BY_HANDLE_FILE_INFORMATION;

   ---------------------
   -- Get_File_Status --
   ---------------------

   function Get_File_Status (File     : POSIX_IO.File_Descriptor)
                             return Status
   is
      File_Status : Status;
      Handle      : Win32.Winnt.HANDLE := POSIX_Win32.File_Handle.Get (File);
   begin
      Result := Win32.Winbase.GetFileInformationByHandle
        (Handle, File_Information'Access);
      POSIX_Win32.Check_Result (Result, "Get_File_Status (File)");

      File_Status.File_Attributes  := File_Information.dwFileAttributes;
      File_Status.Creation_Time    := File_Information.ftCreationTime;
      File_Status.Last_Access_Time := File_Information.ftLastAccessTime;
      File_Status.Last_Write_Time  := File_Information.ftLastWriteTime;
      File_Status.File_Size_Low    := File_Information.nFileSizeLow;
      File_Status.File_Size_High   := File_Information.nFileSizeHigh;
      File_Status.File_Index_Low   := File_Information.nFileIndexLow;
      File_Status.File_Index_High  := File_Information.nFileIndexHigh;
      File_Status.File_Links       := File_Information.nNumberOfLinks;

      File_Status.File_Type        := Win32.Winbase.GetFileType (Handle);

      return File_Status;
   end Get_File_Status;




   --  Operations to get information from Status

   -----------------------
   -- Permission_Set_Of --
   -----------------------

   function Permission_Set_Of (File_Status : Status)
                               return POSIX_Permissions.Permission_Set
   is
      use POSIX_Permissions;
      use type Win32.DWORD;
      PS : POSIX_Permissions.Permission_Set := (others => False);
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


   ----------------
   -- File_ID_Of --
   ----------------

   function File_ID_Of (File_Status : Status)
                        return File_ID
   is
   begin
      return File_ID'(Low  => File_Status.File_Index_Low,
                      High => File_Status.File_Index_High);
   end File_ID_Of;


   ------------------
   -- Device_ID_Of --
   ------------------

   function Device_ID_Of (File_Status : Status)
                          return Device_ID
   is
      DID : Device_ID := 0;
   begin
      return DID;
   end Device_ID_Of;


   -------------------
   -- Link_Count_Of --
   -------------------

   function Link_Count_Of (File_Status : Status)
                           return Links is
   begin
      return Links (File_Status.File_Links);
   end Link_Count_Of;


   --------------
   -- Owner_Of --
   --------------

   function Owner_Of (File_Status : Status)
                      return POSIX_Process_Identification.User_ID
   is
      Owner : POSIX_Process_Identification.User_ID;
   begin
      return Owner;
   end Owner_Of;


   --------------
   -- Group_Of --
   --------------

   function Group_Of (File_Status : Status)
                      return POSIX_Process_Identification.Group_ID
   is
      Group : POSIX_Process_Identification.Group_ID;
   begin
      return Group;
   end Group_Of;


   -------------
   -- Size_Of --
   -------------

   function Size_Of (File_Status : Status)
                     return POSIX.IO_Count is
   begin
      return POSIX.IO_Count
        (File_Status.File_Size_Low);
   end Size_Of;


   -------------------
   -- To_POSIX_Time --
   -------------------

   function To_POSIX_Time is new Ada.Unchecked_Conversion
     (Win32.Winbase.SYSTEMTIME, POSIX_Calendar.POSIX_Time);

   System_Time      : aliased Win32.Winbase.SYSTEMTIME;
   Last_Access_Time : aliased Win32.Winbase.FILETIME;
   Last_Write_Time  : aliased Win32.Winbase.FILETIME;
   Local_Time       : aliased Win32.Winbase.FILETIME;

   -------------------------
   -- Last_Access_Time_Of --
   -------------------------

   function Last_Access_Time_Of (File_Status : Status)
                                 return POSIX_Calendar.POSIX_Time
   is
   begin
      Last_Access_Time := File_Status.Last_Access_Time;
      Result := Win32.Winbase.FileTimeToLocalFileTime
        (Last_Access_Time'Access, Local_Time'Access);
      Result := Win32.Winbase.FileTimeToSystemTime
        (Local_Time'Access, System_Time'Access);
      return To_POSIX_Time (System_Time);
   end Last_Access_Time_Of;


   -------------------------------
   -- Last_Modification_Time_Of --
   -------------------------------

   function Last_Modification_Time_Of (File_Status : Status)
                                       return POSIX_Calendar.POSIX_Time
   is
   begin
      Last_Write_Time := File_Status.Last_Write_Time;
      Result := Win32.Winbase.FileTimeToLocalFileTime
        (Last_Write_Time'Access, Local_Time'Access);
      Result := Win32.Winbase.FileTimeToSystemTime
        (Local_Time'Access, System_Time'Access);
      return To_POSIX_Time (System_Time);
   end Last_Modification_Time_Of;


   --------------------------------
   -- Last_Status_Change_Time_Of --
   --------------------------------

   function Last_Status_Change_Time_Of (File_Status : Status)
                                        return POSIX_Calendar.POSIX_Time is
   begin
      return Last_Modification_Time_Of (File_Status);
   end Last_Status_Change_Time_Of;


   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (File_Status : Status)
                          return Boolean
   is
      use type Win32.DWORD;
   begin
      return (File_Status.File_Attributes and
              Win32.Winnt.FILE_ATTRIBUTE_DIRECTORY) /= 0;
   end Is_Directory;


   -------------------------------
   -- Is_Character_Special_File --
   -------------------------------

   function Is_Character_Special_File (File_Status : Status)
                                       return Boolean
   is
      use type Win32.DWORD;
   begin
      return File_Status.File_Type = Win32.Winbase.FILE_TYPE_CHAR;
   end Is_Character_Special_File;


   ---------------------------
   -- Is_Block_Special_File --
   ---------------------------

   function Is_Block_Special_File (File_Status : Status)
                                   return Boolean is
   begin
      return False;
   end Is_Block_Special_File;


   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File_Status : Status)
                             return Boolean
   is
      use type Win32.DWORD;
      Regular_File_Mask : constant Win32.DWORD :=
        Win32.Winnt.FILE_ATTRIBUTE_NORMAL or
        Win32.Winnt.FILE_ATTRIBUTE_ARCHIVE;
   begin
      return (File_Status.File_Attributes and Regular_File_Mask) /= 0;
   end Is_Regular_File;


   -------------
   -- Is_FIFO --
   -------------

   function Is_FIFO (File_Status : Status)
                     return Boolean is
   begin
      return False;
   end Is_FIFO;

end POSIX_File_Status;
