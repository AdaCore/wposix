
with Ada.Command_Line;
with Ada.Text_IO;
with POSIX;
with POSIX_Files;

procedure Demo2 is

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
   Display_Directory (To_POSIX_String ("*.c"));
   Display_Directory (To_POSIX_String ("*.adb"));
end Demo2;
