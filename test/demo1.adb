
-- $Id$

with Ada.Command_Line;
with Ada.Text_IO;
with POSIX;
with POSIX_Files;
with POSIX_File_Status;

procedure Demo1 is

   use Ada;
   use POSIX;
   use POSIX_Files;
   use POSIX_File_Status;

   Filename : constant String := Command_Line.Argument (1);

begin
   Text_IO.Put_Line ("stat : [" & Filename & "]   -   " &
                     Integer'Image (Filename'Length));

   if Is_File_Present (To_POSIX_String (Filename)) then
      Text_IO.Put_Line (Filename & " is present");
   else
      Text_IO.Put_Line (Filename & " is not present");
   end if;

   if Is_Regular_File (Get_File_Status (To_POSIX_String (Filename))) then
      Text_IO.Put_Line (Filename & " is a regular file");
   else
      Text_IO.Put_Line (Filename & " is not a regular file");
   end if;

   if Is_Directory (To_POSIX_String (Filename)) then
      Text_IO.Put_Line (Filename & " is a directory");
   else
      Text_IO.Put_Line (Filename & " is not a directory");
   end if;
end Demo1;
