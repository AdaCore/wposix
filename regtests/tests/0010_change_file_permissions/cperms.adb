------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

with POSIX.File_Status;
with POSIX.Files;
with POSIX.Permissions;

procedure CPerms is

   use Ada;
   use POSIX;
   use POSIX.Permissions;

   -------------------
   -- Display_Perms --
   -------------------

   procedure Display_Perms
      (File    : String;
       Actives : Permission_Set)
   is
      Status : File_Status.Status;
      Perms  : Permission_Set;
   begin
      Text_IO.Put_Line ("File: " & File);
      Status := File_Status.Get_File_Status (POSIX.To_POSIX_String (File));
      Perms := File_Status.Permission_Set_Of (Status);

      for P in Perms'Range loop
         if Actives (P) then
            Text_IO.Put_Line (P'Img & " = " & Perms (P)'Img);
         end if;
      end loop;
      Text_IO.New_Line;
   end Display_Perms;

   Status : File_Status.Status;
   Perms  : Permission_Set;

begin
   Status := File_Status.Get_File_Status ("check1");
   Perms := File_Status.Permission_Set_Of (Status);

   Display_Perms ("check1", Access_Permission_Set);

   --  Add write to owner/group

   Perms (Permissions.Owner_Write) := True;
   Perms (Permissions.Group_Write) := True;

   Files.Change_Permissions ("check1", Perms);

   Display_Perms ("check1", Group_Permission_Set or Owner_Permission_Set);

   --  Owner permissions

   Files.Change_Permissions ("check1", Owner_Permission_Set);

   Display_Perms
     ("check1",
      Permission_Set'(Owner_Read | Owner_Write => True,
                      others                   => False));

   --  Group permissions

   --  Files.Change_Permissions ("check1", Permissions.Group_Permission_Set);

   --  Display_Perms ("check1");

   --  Others permissions

   --  Files.Change_Permissions ("check1", Permissions.Others_Permission_Set);

   --  Display_Perms ("check1");

   --  Access permissions

   --  Files.Change_Permissions ("check1", Permissions.Access_Permission_Set);

   --  Display_Perms ("check1");
end CPerms;
