
--  $Id$

with Ada.Unchecked_Conversion;
with Win32.Crt.Types;

with POSIX_Win32;
with POSIX_Permissions;

package body POSIX_File_Status is

   Retcode : Win32.INT;

   File_Status : Status;

   --  Operations to Obtain File Status

   function Get_File_Status (Pathname : POSIX.Pathname)
                             return Status
   is
      L_Pathname  : constant String := POSIX.To_String (Pathname) & ASCII.Nul;
   begin
      Retcode := Win32.Crt.Stat.Stat (Win32.Addr (L_Pathname),
                                  File_Status.Fstat'Access);
      POSIX_Win32.Check_Retcode (Retcode, "Get_File_Status");
      File_Status.Is_Executable := POSIX_Win32.Is_Executable (Pathname);
      return File_Status;
   end Get_File_Status;


                    -----------------------------------

   function Get_File_Status (File     : POSIX_IO.File_Descriptor)
                             return Status is
   begin
      Retcode := Win32.Crt.Stat.Fstat (Win32.INT (File),
                                   File_Status.Fstat'Access);
      POSIX_Win32.Check_Retcode (Retcode, "Get_File_Status");
      return Status (File_Status);
   end Get_File_Status;




   --  Operations to get information from Status

   function Permission_Set_Of (File_Status : Status)
                               return POSIX_Permissions.Permission_Set
   is
      use POSIX_Permissions;
      PS : POSIX_Permissions.Permission_Set := (others => False);
   begin
      PS := POSIX_Win32.To_Set (Win32.UINT (File_Status.Fstat.St_Mode));
      if File_Status.Is_Executable then
         PS (Owner_Execute)  := True;
         PS (Group_Execute)  := True;
         PS (Others_Execute) := True;
      end if;
      return PS;
   end Permission_Set_Of;

                    -----------------------------------

   function File_ID_Of (File_Status : Status)
                        return File_ID is
   begin
      return File_ID (File_Status.Fstat.St_Dev);
   end File_ID_Of;

                    -----------------------------------

   function Device_ID_Of (File_Status : Status)
                          return Device_ID is
   begin
      return Device_ID (File_Status.Fstat.St_Dev);
   end Device_ID_Of;

                    -----------------------------------

   function Link_Count_Of (File_Status : Status)
                           return Links is
   begin
      return Links (File_Status.Fstat.St_Nlink);
   end Link_Count_Of;

                    -----------------------------------

   function Owner_Of (File_Status : Status)
                      return POSIX_Process_Identification.User_ID is
   begin
      return POSIX_Process_Identification.Get_Real_User_ID;
   end Owner_Of;

                    -----------------------------------

   function Group_Of (File_Status : Status)
                      return POSIX_Process_Identification.Group_ID is
   begin
      return POSIX_Process_Identification.Get_Real_Group_ID;
   end Group_Of;

                    -----------------------------------

   function Size_Of (File_Status : Status)
                     return POSIX.IO_Count is
   begin
      return POSIX.IO_Count (File_Status.Fstat.St_Size);
   end Size_Of;
                    -----------------------------------

   function Time_T_To_POSIX_Time is
     new Ada.Unchecked_Conversion (Win32.Crt.Types.Time_T,
                                   POSIX_Calendar.POSIX_Time);

   function Last_Access_Time_Of (File_Status : Status)
                                 return POSIX_Calendar.POSIX_Time is
   begin
      return Time_T_To_POSIX_Time (File_Status.Fstat.St_Atime);
   end Last_Access_Time_Of;

                    -----------------------------------

   function Last_Modification_Time_Of (File_Status : Status)
                                       return POSIX_Calendar.POSIX_Time is
   begin
      return Time_T_To_POSIX_Time (File_Status.Fstat.St_Mtime);
   end Last_Modification_Time_Of;

                    -----------------------------------

   function Last_Status_Change_Time_Of (File_Status : Status)
                                        return POSIX_Calendar.POSIX_Time is
   begin
      return Time_T_To_POSIX_Time (File_Status.Fstat.St_Ctime);
   end Last_Status_Change_Time_Of;

                    -----------------------------------

   use type Win32.USHORT;

   function Is_Directory (File_Status : Status)
                          return Boolean is
   begin
      return (File_Status.Fstat.St_Mode and Win32.Crt.Stat.S_IFDIR) /= 0;
   end Is_Directory;

                    -----------------------------------

   function Is_Character_Special_File (File_Status : Status)
                                       return Boolean is
   begin
      return (File_Status.Fstat.St_Mode and Win32.Crt.Stat.S_IFCHR) /= 0;
   end Is_Character_Special_File;

                    -----------------------------------

   function Is_Block_Special_File (File_Status : Status)
                                   return Boolean is
   begin
      return False;
   end Is_Block_Special_File;

                    -----------------------------------

   function Is_Regular_File (File_Status : Status)
                             return Boolean is
   begin
      return (File_Status.Fstat.St_Mode and Win32.Crt.Stat.S_IFREG) /= 0;
   end Is_Regular_File;

                    -----------------------------------

   function Is_FIFO (File_Status : Status)
                     return Boolean is
   begin
      return (File_Status.Fstat.St_Mode and Win32.Crt.Stat.S_IFIFO) /= 0;
   end Is_FIFO;

end POSIX_File_Status;
