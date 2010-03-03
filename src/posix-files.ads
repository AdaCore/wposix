------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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

with POSIX.Permissions;
with POSIX.Process_Identification;
with POSIX.Calendar;

private with Win32.Winbase;

package POSIX.Files is

   --  Operations to create files in the File System

   procedure Create_Directory
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX.Permissions.Permission_Set);

   procedure Create_FIFO
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX.Permissions.Permission_Set);

   --  Operations to remove files from the File System

   procedure Unlink (Pathname : in POSIX.Pathname);

   procedure Remove_Directory (Pathname : in POSIX.Pathname);

   --  Predicates on files in the File System

   function Is_File (Pathname : in POSIX.Pathname) return Boolean;

   function Is_Directory (Pathname : in POSIX.Pathname) return Boolean;

   function Is_FIFO (Pathname : in POSIX.Pathname) return Boolean;

   function Is_Character_Special_File
     (Pathname : in POSIX.Pathname) return Boolean;

   function Is_Block_Special_File
     (Pathname : in POSIX.Pathname) return Boolean;

   function Is_Symbolic_Link (Pathname : in POSIX.Pathname) return Boolean;

   function Is_Socket (Pathname : in POSIX.Pathname) return Boolean;

   --  Operations to modify File Pathnames

   procedure Link
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname);

   procedure Rename
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname);

   --  Iterating over files within a directory

   type Directory_Entry is limited private;

   function Filename_Of (D_Entry : Directory_Entry) return POSIX.Filename;

   generic
      with procedure Action
        (D_Entry : in     Directory_Entry;
         Quit    : in out Boolean);
   procedure For_Every_Directory_Entry (Pathname : in POSIX.Pathname);

   --  Operations to Update File Status Information

   procedure Change_Owner_And_Group
     (Pathname : in POSIX.Pathname;
      Owner    : in POSIX.Process_Identification.User_ID;
      Group    : in POSIX.Process_Identification.Group_ID);

   procedure Change_Permissions
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX.Permissions.Permission_Set);

   procedure Set_File_Times
     (Pathname          : in POSIX.Pathname;
      Access_Time       : in POSIX.Calendar.POSIX_Time;
      Modification_Time : in POSIX.Calendar.POSIX_Time);

   procedure Set_File_Times (Pathname : in POSIX.Pathname);

   --  Operations to Determine File Accessibility

   type Access_Mode is (Read_Ok, Write_Ok, Execute_Ok);
   type Access_Mode_Set is array (Access_Mode) of Boolean;

   function Is_Accessible
     (Pathname    : in POSIX.Pathname;
      Access_Mode : in Access_Mode_Set) return Boolean;

   function Accessibility
     (Pathname    : in POSIX.Pathname;
      Access_Mode : in Access_Mode_Set) return POSIX.Error_Code;

   function Is_File_Present (Pathname : in POSIX.Pathname) return Boolean;

   function Existence (Pathname : in POSIX.Pathname) return POSIX.Error_Code;

private

   type Directory_Entry is new Win32.Winbase.WIN32_FIND_DATA;

end POSIX.Files;
