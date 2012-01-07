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

with POSIX.Permissions;
with POSIX.Process_Identification;
with POSIX.Calendar;

private with Win32.Winbase;

package POSIX.Files is

   --  Operations to create files in the File System

   procedure Create_Directory
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set);

   procedure Create_FIFO
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set);

   --  Operations to remove files from the File System

   procedure Unlink (Pathname : POSIX.Pathname);

   procedure Remove_Directory (Pathname : POSIX.Pathname);

   --  Predicates on files in the File System

   function Is_File (Pathname : POSIX.Pathname) return Boolean;

   function Is_Directory (Pathname : POSIX.Pathname) return Boolean;

   function Is_FIFO (Pathname : POSIX.Pathname) return Boolean;

   function Is_Character_Special_File
     (Pathname : POSIX.Pathname) return Boolean;

   function Is_Block_Special_File
     (Pathname : POSIX.Pathname) return Boolean;

   function Is_Symbolic_Link (Pathname : POSIX.Pathname) return Boolean;

   function Is_Socket (Pathname : POSIX.Pathname) return Boolean;

   --  Operations to modify File Pathnames

   procedure Link
     (Old_Pathname : POSIX.Pathname;
      New_Pathname : POSIX.Pathname);

   procedure Rename
     (Old_Pathname : POSIX.Pathname;
      New_Pathname : POSIX.Pathname);

   --  Iterating over files within a directory

   type Directory_Entry is limited private;

   function Filename_Of (D_Entry : Directory_Entry) return POSIX.Filename;

   generic
      with procedure Action
        (D_Entry :        Directory_Entry;
         Quit    : in out Boolean);
   procedure For_Every_Directory_Entry (Pathname : POSIX.Pathname);

   --  Operations to Update File Status Information

   procedure Change_Owner_And_Group
     (Pathname : POSIX.Pathname;
      Owner    : POSIX.Process_Identification.User_ID;
      Group    : POSIX.Process_Identification.Group_ID);

   procedure Change_Permissions
     (Pathname   : POSIX.Pathname;
      Permission : POSIX.Permissions.Permission_Set);

   procedure Set_File_Times
     (Pathname          : POSIX.Pathname;
      Access_Time       : POSIX.Calendar.POSIX_Time;
      Modification_Time : POSIX.Calendar.POSIX_Time);

   procedure Set_File_Times (Pathname : POSIX.Pathname);

   --  Operations to Determine File Accessibility

   type Access_Mode is (Read_Ok, Write_Ok, Execute_Ok);
   type Access_Mode_Set is array (Access_Mode) of Boolean;

   function Is_Accessible
     (Pathname    : POSIX.Pathname;
      Access_Mode : Access_Mode_Set) return Boolean;

   function Accessibility
     (Pathname    : POSIX.Pathname;
      Access_Mode : Access_Mode_Set) return POSIX.Error_Code;

   function Is_File_Present (Pathname : POSIX.Pathname) return Boolean;

   function Existence (Pathname : POSIX.Pathname) return POSIX.Error_Code;

private

   type Directory_Entry is new Win32.Winbase.WIN32_FIND_DATA;

end POSIX.Files;
