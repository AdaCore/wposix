
--  $Id$
--  Author : Pascal Obry
--  p.obry@wanadoo.fr

with POSIX.Permissions;
with POSIX.Process_Identification;
with POSIX.Calendar;
with Win32.Winbase;

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

   function Is_File (Pathname : in POSIX.Pathname)
                     return Boolean;

   function Is_Directory (Pathname : in POSIX.Pathname)
                          return Boolean;

   function Is_FIFO (Pathname : in POSIX.Pathname)
                     return Boolean;

   function Is_Character_Special_File (Pathname : in POSIX.Pathname)
                                       return Boolean;

   function Is_Block_Special_File (Pathname : in POSIX.Pathname)
                                   return Boolean;



   --  Operations to modify File Pathnames

   procedure Link
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname);

   procedure Rename
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname);



   --  Iterating over files within a directory

   type Directory_Entry is limited private;

   function Filename_Of (D_Entry : Directory_Entry)
                         return POSIX.Filename;

   generic
      with procedure Action
        (D_Entry : in     Directory_Entry;
         Quit    : in out Boolean);
   procedure For_Every_Directory_Entry
     (Pathname : in POSIX.Pathname);



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
      Access_Mode : in Access_Mode_Set)
      return Boolean;

   function Accessibility
     (Pathname    : in POSIX.Pathname;
      Access_Mode : in Access_Mode_Set)
      return POSIX.Error_Code;

   function Is_File_Present (Pathname : in POSIX.Pathname)
                             return Boolean;

   function Existence (Pathname : in POSIX.Pathname)
                       return POSIX.Error_Code;

private

   type Directory_Entry is new Win32.Winbase.WIN32_FIND_DATA;

end POSIX.Files;
