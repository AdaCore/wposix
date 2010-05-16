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

with POSIX.File_Status;
with POSIX.Files;
with POSIX.Permissions;

procedure CPerms is

   use Ada;
   use POSIX;

   -------------------
   -- Display_Perms --
   -------------------

   procedure Display_Perms (File : String) is
      Status : File_Status.Status;
      Perms  : Permissions.Permission_Set;
   begin
      Text_IO.Put_Line ("File: " & File);
      Status := File_Status.Get_File_Status (POSIX.To_POSIX_String (File));
      Perms := File_Status.Permission_Set_Of (Status);

      for P in Perms'Range loop
         Text_IO.Put_Line (P'Img & " = " & Perms (P)'Img);
      end loop;
      Text_IO.New_Line;
   end Display_Perms;

   Status : File_Status.Status;
   Perms  : Permissions.Permission_Set;

begin
   Status := File_Status.Get_File_Status ("check1");
   Perms := File_Status.Permission_Set_Of (Status);

   Display_Perms ("check1");

   --  Add write to owner/group

   Perms (Permissions.Owner_Write) := True;
   Perms (Permissions.Group_Write) := True;

   Files.Change_Permissions ("check1", Perms);

   Display_Perms ("check1");

   --  Owner permissions

   Files.Change_Permissions ("check1", Permissions.Owner_Permission_Set);

   Display_Perms ("check1");

   --  Group permissions

   Files.Change_Permissions ("check1", Permissions.Group_Permission_Set);

   Display_Perms ("check1");

   --  Others permissions

   Files.Change_Permissions ("check1", Permissions.Others_Permission_Set);

   Display_Perms ("check1");

   --  Access permissions

   Files.Change_Permissions ("check1", Permissions.Access_Permission_Set);

   Display_Perms ("check1");
end CPerms;
