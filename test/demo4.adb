
with Ada.Text_IO;

with POSIX;
with POSIX_Files;
with POSIX_Permissions;
with POSIX_Process_Identification;

procedure Demo4 is

   use Ada.Text_IO;
   use POSIX;
   use POSIX_Files;
   use POSIX_Permissions;
   use POSIX_Process_Identification;

begin
   Create_Directory ("un directory", Access_Permission_Set);
   Put_Line ("Name = " & To_String (Get_Login_Name));
   Put_Line ("PID = [" & Image (Get_Process_ID) & ']');
end Demo4;
