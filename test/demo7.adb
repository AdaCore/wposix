
with Ada.Text_IO;
with TOD_Utilities;
with POSIX_File_Status;
with POSIX_Calendar;

procedure Demo7 is

   use Ada;

   procedure Display_Time (Date_Time : in POSIX_Calendar.POSIX_Time) is
   begin
      Text_IO.Put_Line (TOD_Utilities.Convert
                        (POSIX_Calendar.To_Time (Date_Time)));
   end Display_Time;

   Status : POSIX_File_Status.Status;

begin
   Status := POSIX_File_Status.Get_File_Status ("demo7.adb");
   Display_Time (POSIX_File_Status.Last_Access_Time_Of (Status));
   Display_Time (POSIX_File_Status.Last_Modification_Time_Of (Status));
   Display_Time (POSIX_File_Status.Last_Status_Change_Time_Of (Status));
end Demo7;
