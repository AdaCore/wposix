
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with POSIX;
with POSIX_Files;

procedure Demo2 is

   use Ada;
   use Ada.Command_Line;
   use Ada.Text_IO;
   use POSIX;
   use POSIX_Files;

   procedure Display (Dirent : in     Directory_Entry;
                      Quit   : in out Boolean) is
   begin
      Put_Line (To_String (Filename_Of (Dirent)));
      Quit := False;
   end Display;

   procedure Display_Directory is
      new For_Every_Directory_Entry (Action => Display);

begin
   Put_Line ("List directory .");
   Display_Directory (To_POSIX_String ("."));

   New_Line;
   Put_Line ("List directory c:\temp");
   Display_Directory (To_POSIX_String ("c:\temp"));

   New_Line;
   Put_Line ("List directory /tmp");
   Display_Directory (To_POSIX_String ("/tmp"));

   begin
      New_Line;
      Put_Line ("List directory /tmp2");
      Display_Directory (To_POSIX_String ("/tmp2"));
   exception
      when E : others =>
         Put_Line (Exceptions.Exception_Name (E) & " - " &
                   Exceptions.Exception_Message (E) & " : " &
                   POSIX.Image (POSIX.Get_Error_Code));
   end;

   begin
      New_Line;
      Put_Line ("List directory /tmp/toto");
      Display_Directory (To_POSIX_String ("/tmp/toto"));
   exception
      when E : others =>
         Put_Line (Exceptions.Exception_Name (E) & " - " &
                   Exceptions.Exception_Message (E) & " : " &
                   POSIX.Image (POSIX.Get_Error_Code));
   end;
end Demo2;
