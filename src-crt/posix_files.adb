
--  $Id$

with System;
with Interfaces.C;
with Ada.Unchecked_Conversion;
with Win32.Crt.Direct;
with Win32.Crt.Stat;
with Win32.Crt.Stdio;
with Win32.Crt.IO;
with Win32.Winnt;
with Win32.Winbase;
with Win32.Crt.Types;
with Win32.Crt.Utime;

with POSIX_Win32;
with POSIX_Calendar;
with POSIX_File_Status;

package body POSIX_Files is

   use POSIX_Win32;
   use type Win32.INT;
   use type Win32.LONG;
   use type Win32.ULONG;
   use type Win32.BOOL;

   --  Operations to create files in the File System
   Retcode : Win32.INT;
   Result  : Win32.BOOL;

   procedure Create_Directory
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX_Permissions.Permission_Set)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
   begin
      Result := Win32.Winbase.CreateDirectory
        (Win32.Addr (L_Pathname),
         POSIX_Win32.Null_Security_Attributes'Access);
      POSIX_Win32.Check_Result (Result, "Create_Directory");
   end Create_Directory;

                    -----------------------------------

   procedure Create_FIFO
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX_Permissions.Permission_Set)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Create_FIFO");
      POSIX_Win32.Check_Retcode (Retcode, "Create_Fifo");
   end Create_FIFO;






   --  Operations to remove files from the File System

   procedure Unlink (Pathname : in POSIX.Pathname) is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
   begin
      Result := Win32.Winbase.DeleteFile (Win32.Addr (L_Pathname));
      POSIX_Win32.Check_Result (Result, "Unlink");
   end Unlink;

                    -----------------------------------

   procedure Remove_Directory (Pathname : in POSIX.Pathname) is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
   begin
      Result := Win32.Winbase.RemoveDirectory (Win32.Addr (L_Pathname));
      POSIX_Win32.Check_Result (Result, "Remove_Directory");
   end Remove_Directory;





   --  Predicates on files in the File System

   function Is_File (Pathname : in POSIX.Pathname)
                     return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Regular_File (Get_File_Status (Pathname));
   exception
      when others =>
         POSIX_Win32.Check_Result (Win32.False, "Is_File");
         return False;
   end Is_File;

                    -----------------------------------

   function Is_Directory (Pathname : in POSIX.Pathname)
                          return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Directory (Get_File_Status (Pathname));
   exception
      when others =>
         POSIX_Win32.Check_Result (Win32.False, "Is_Directory");
         return False;
   end Is_Directory;


                    -----------------------------------

   function Is_FIFO (Pathname : in POSIX.Pathname)
                     return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_FIFO (Get_File_Status (Pathname));
   exception
      when others =>
         POSIX_Win32.Check_Result (Win32.False, "Is_FIFO");
         return False;
   end Is_FIFO;


                    -----------------------------------

   function Is_Character_Special_File (Pathname : in POSIX.Pathname)
                                       return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Character_Special_File (Get_File_Status (Pathname));
   exception
      when others =>
         POSIX_Win32.Check_Result (Win32.False, "Is_Character_Special_File");
         return False;
   end Is_Character_Special_File;


                    -----------------------------------

   function Is_Block_Special_File (Pathname : in POSIX.Pathname)
                                   return Boolean
   is
      use POSIX_File_Status;
   begin
      return Is_Block_Special_File (Get_File_Status (Pathname));
   exception
      when others =>
         POSIX_Win32.Check_Result (Win32.False, "Is_Block_Special_File");
         return False;
   end Is_Block_Special_File;





   --  Operations to modify File Pathnames

   procedure Link
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname)
   is
      L_Old_Pathname : constant String :=
        POSIX.To_String (Old_Pathname) & ASCII.Nul;
      L_New_Pathname : constant String :=
        POSIX.To_String (New_Pathname) & ASCII.Nul;
   begin
      Result := Win32.Winbase.CopyFile
        (Win32.Addr (L_Old_Pathname),
         Win32.Addr (L_New_Pathname), 1);
      POSIX_Win32.Check_Result (Result, "Link");
   end Link;

                    -----------------------------------

   procedure Rename
     (Old_Pathname : in POSIX.Pathname;
      New_Pathname : in POSIX.Pathname)
   is
      L_Old_Pathname : constant String :=
        POSIX.To_String (Old_Pathname) & ASCII.Nul;
      L_New_Pathname : constant String :=
        POSIX.To_String (New_Pathname) & ASCII.Nul;
   begin
      Retcode := Win32.Crt.Stdio.Rename
        (Win32.Addr (L_Old_Pathname), Win32.Addr (L_New_Pathname));
      POSIX_Win32.Check_Retcode (Retcode, "Rename");
   end Rename;





   --  Iterating over files within a directory

   function Filename_Of (D_Entry : Directory_Entry)
                         return POSIX.Filename
   is
      Name : String (1 .. 200);
      I    : Natural := 0;
      C    : Character;
   begin
      loop
         C := Interfaces.C.To_Ada (D_Entry.Name (I));
         exit when C = ASCII.Nul;
         I := I + 1;
         Name (I) := C;
         exit when I = 200;
      end loop;
      return POSIX.To_POSIX_String (Name (1 .. I));
   end Filename_Of;

                    -----------------------------------

   procedure For_Every_Directory_Entry
     (Pathname : in POSIX.Pathname)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      Data       : aliased Win32.Crt.IO.Finddata_T;
      Handle     : Win32.LONG;
      Quit       : Boolean;
   begin
      Handle := Win32.Crt.IO.Findfirst (Win32.Addr (L_Pathname), Data'Access);
      if Handle = -1 then
         Retcode := -1;
      else
         Retcode := 0;
      end if;
      POSIX_Win32.Check_Retcode (Retcode, "For_Every_Directory_Entry");
      Action  (Directory_Entry (Data), Quit);
      if not Quit then
         loop
            Retcode := Win32.Crt.IO.Findnext (Handle, Data'Access);
            exit when Retcode = -1;
            Action  (Directory_Entry (Data), Quit);
            exit when quit;
         end loop;
      end if;
      if Retcode = -1 then
         Retcode := Win32.Crt.IO.Findclose (Handle);
         POSIX_Win32.Check_Retcode (-1, "For_Every_Directory_Entry");
      else
         Retcode := Win32.Crt.IO.Findclose (Handle);
         POSIX_Win32.Check_Retcode (Retcode, "For_Every_Directory_Entry");
      end if;
   end For_Every_Directory_Entry;




   --  Operations to Update File Status Information

   procedure Change_Owner_And_Group
     (Pathname : in POSIX.Pathname;
      Owner    : in POSIX_Process_Identification.User_ID;
      Group    : in POSIX_Process_Identification.Group_ID) is
   begin
      null;
   end Change_Owner_And_Group;


                    -----------------------------------

   procedure Change_Permissions
     (Pathname   : in POSIX.Pathname;
      Permission : in POSIX_Permissions.Permission_Set)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      Mask       : Win32.UINT      := POSIX_Win32.To_Mask (Permission);
   begin
      Retcode := Win32.Crt.IO.Chmod (Win32.Addr (L_Pathname), Win32.INT (Mask));
      POSIX_Win32.Check_Retcode (Retcode, "Change_Permissions");
   end Change_Permissions;

                    -----------------------------------

   function POSIX_Time_To_Time_T is
     new Ada.Unchecked_Conversion (POSIX_Calendar.POSIX_Time,
                                   Win32.Crt.Types.Time_T);

   procedure Set_File_Times
     (Pathname          : in POSIX.Pathname;
      Access_Time       : in POSIX_Calendar.POSIX_Time;
      Modification_Time : in POSIX_Calendar.POSIX_Time)
   is
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
      New_Times  : aliased Win32.Crt.Utime.Utimbuf;
   begin
      New_Times.Actime  := POSIX_Time_To_Time_T (Access_Time);
      New_Times.Modtime := POSIX_Time_To_Time_T (Modification_Time);
      Retcode := Win32.Crt.Utime.Utime (Win32.Addr (L_Pathname),
                                        New_Times'Access);
      POSIX_Win32.Check_Result (Result, "Set_File_Times");
   end Set_File_Times;


                    -----------------------------------

   procedure Set_File_Times (Pathname : in POSIX.Pathname) is
      Current_Time : POSIX_Calendar.POSIX_Time;
   begin
      Current_Time := POSIX_Calendar.Clock;
      Set_File_Times (Pathname, Current_Time, Current_Time);
   end Set_File_Times;




   --  Operations to Determine File Accessibility

   function Is_Accessible
     (Pathname    : in POSIX.Pathname;
      Access_Mode : in Access_Mode_Set)
      return Boolean
   is
      use type POSIX.Error_Code;
   begin
      return Accessibility (Pathname, Access_Mode) = POSIX.No_Error;
   end Is_Accessible;

                    -----------------------------------

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

                    -----------------------------------

   function Is_File_Present (Pathname : in POSIX.Pathname)
                             return Boolean
   is
      use type Interfaces.C.Int;
      L_Pathname : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
   begin
      Retcode := Win32.Crt.IO.C_Access (Win32.Addr (L_Pathname), 0);
      return Retcode = 0;
   end Is_File_Present;

                    -----------------------------------

   function Existence (Pathname : in POSIX.Pathname)
                       return POSIX.Error_Code
   is
      Result : Boolean;
   begin
      Result := Is_File_Present (Pathname);
      return POSIX.Get_Error_Code;
   end Existence;

end POSIX_Files;
