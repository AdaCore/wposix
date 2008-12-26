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

with Win32.Winbase;

with POSIX.Permissions;
with POSIX.Process_Identification;
with POSIX.IO;
with POSIX.Calendar;

package POSIX.File_Status is

   type Status is private;

   --  Operations to Obtain File Status

   function Get_File_Status (Pathname : in POSIX.Pathname) return Status;

   function Get_File_Status (File : in POSIX.IO.File_Descriptor) return Status;

   --  Operations to get information from Status

   type File_ID is private;

   type Device_ID is private;

   subtype Links is Natural range 0 .. POSIX.Link_Limit_Maxima'Last;

   function Permission_Set_Of
     (File_Status : in Status) return POSIX.Permissions.Permission_Set;

   function File_ID_Of
     (File_Status : in Status) return File_ID;

   function Device_ID_Of
     (File_Status : in Status) return Device_ID;

   function Link_Count_Of
     (File_Status : in Status) return Links;

   function Owner_Of
     (File_Status : in Status) return POSIX.Process_Identification.User_ID;

   function Group_Of
     (File_Status : in Status) return POSIX.Process_Identification.Group_ID;

   function Size_Of
     (File_Status : in Status) return POSIX.IO_Count;

   function Last_Access_Time_Of
     (File_Status : in Status) return POSIX.Calendar.POSIX_Time;

   function Last_Modification_Time_Of
     (File_Status : in Status) return POSIX.Calendar.POSIX_Time;

   function Last_Status_Change_Time_Of
     (File_Status : in Status) return POSIX.Calendar.POSIX_Time;

   function Is_Directory (File_Status : in Status) return Boolean;

   function Is_Character_Special_File
     (File_Status : in Status) return Boolean;

   function Is_Block_Special_File
     (File_Status : in Status) return Boolean;

   function Is_Regular_File (File_Status : in Status) return Boolean;

   function Is_FIFO (File_Status : in Status) return Boolean;

private

   type Status is record
      Is_Executable    : Boolean     := False;
      File_Attributes  : Win32.DWORD := 0;
      Creation_Time    : Win32.Winbase.FILETIME;
      Last_Access_Time : Win32.Winbase.FILETIME;
      Last_Write_Time  : Win32.Winbase.FILETIME;
      File_Size_Low    : Win32.DWORD := 0;
      File_Size_High   : Win32.DWORD := 0;
      File_Links       : Win32.DWORD := 0;
      File_Type        : Win32.DWORD := Win32.Winbase.FILE_TYPE_UNKNOWN;
   end record;

   type File_ID is record
      Low, High : Win32.DWORD;
   end record;

   type Device_ID is new Integer;

end POSIX.File_Status;
