
--  $Id$

with Ada.Text_IO;
with POSIX_IO;
with POSIX_Permissions;

procedure Demo3 is

   use Ada.Text_IO;
   use POSIX_IO;
   use POSIX_Permissions;

   File : File_Descriptor;

begin
   File := Open_Or_Create ("fdemo3", Read_Write, Access_Permission_Set);

   Put_Line ("Is_A_Terminal : " & Boolean'Image (Is_A_Terminal (File)));

   Close (File);
end Demo3;
