
with Ada.Text_IO;
with POSIX;

procedure Main is
   use Ada;
begin
   Text_IO.Put_Line ("System Name : " & POSIX.To_String (POSIX.System_Name));
end Main;
