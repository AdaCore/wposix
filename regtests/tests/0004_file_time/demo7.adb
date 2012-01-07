------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;

with POSIX_File_Status;
with POSIX_Calendar;

procedure Demo7 is

   use Ada;

   ------------------
   -- Display_Time --
   ------------------

   procedure Display_Time (Date_Time : POSIX_Calendar.POSIX_Time) is
      use POSIX_Calendar;
      Secs : Natural;
      H    : Natural range 0 .. 23;
      M    : Natural range 0 .. 59;
      S    : Natural range 0 .. 59;
   begin
      Text_IO.Put (Day_Number'Image (Day (Date_Time)));
      Text_IO.New_Line;
      Text_IO.Put (Month_Number'Image (Month (Date_Time)));
      Text_IO.New_Line;
      Text_IO.Put (Year_Number'Image (Year (Date_Time)));
      Text_IO.New_Line;

      Secs := Natural (Seconds (Date_Time));

      H := Secs / 3_600;
      Secs := Secs - (H * 3_600);
      M := Secs / 60;
      S := Secs - (M * 60);

      Integer_Text_IO.Put (H, 2);
      Text_IO.New_Line;
      Integer_Text_IO.Put (M, 2);
      Text_IO.New_Line;
      Integer_Text_IO.Put (S, 2);
      Text_IO.New_Line;
   end Display_Time;

   Status : POSIX_File_Status.Status;

begin
   Text_IO.Put_Line ("now");
   Display_Time (POSIX_Calendar.Clock);

   Status := POSIX_File_Status.Get_File_Status ("file.txt");
   Text_IO.Put_Line ("Last Access Time        : ");
   Display_Time (POSIX_File_Status.Last_Access_Time_Of (Status));
   Text_IO.Put_Line ("Last Modification Time  : ");
   Display_Time (POSIX_File_Status.Last_Modification_Time_Of (Status));
   Text_IO.Put_Line ("Last Status Change Time : ");
   Display_Time (POSIX_File_Status.Last_Status_Change_Time_Of (Status));
end Demo7;
