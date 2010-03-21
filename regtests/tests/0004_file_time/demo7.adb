------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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
