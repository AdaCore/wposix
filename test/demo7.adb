
-- $Id$

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with POSIX_File_Status;
with POSIX_Calendar;

procedure Demo7 is

   use Ada;

   procedure Display_Time (Date_Time : in POSIX_Calendar.POSIX_Time) is
      use POSIX_Calendar;
      Secs : Natural;
      H    : Natural range 0 .. 23;
      M    : Natural range 0 .. 59;
      S    : Natural range 0 .. 59;
   begin
      Text_IO.Put (Day_Number'Image (Day (Date_Time)));
      Text_IO.Put (Month_Number'Image (Month (Date_Time)));
      Text_IO.Put (Year_Number'Image (Year (Date_Time)));

      Secs := Natural (Seconds (Date_Time));

      H := Secs / 3_600;
      Secs := Secs - (H * 3_600);
      M := Secs / 60;
      S := Secs - (M * 60);

      Text_IO.Put ("  -  ");

      Integer_Text_IO.Put (H, 2); Text_IO.Put (':');
      Integer_Text_IO.Put (M, 2); Text_IO.Put (':');
      Integer_Text_IO.Put (S, 2);

      Text_IO.New_Line;
   end Display_Time;

   Status : POSIX_File_Status.Status;

begin
   Text_IO.Put ("now : ");
   Display_Time (POSIX_Calendar.Clock);

   Status := POSIX_File_Status.Get_File_Status ("demo7.adb");
   Text_IO.Put ("demo7 Last Access Time        : ");
   Display_Time (POSIX_File_Status.Last_Access_Time_Of (Status));
   Text_IO.Put ("demo7 Last Modification Time  : ");
   Display_Time (POSIX_File_Status.Last_Modification_Time_Of (Status));
   Text_IO.Put ("demo7 Last Status Change Time : ");
   Display_Time (POSIX_File_Status.Last_Status_Change_Time_Of (Status));
end Demo7;
