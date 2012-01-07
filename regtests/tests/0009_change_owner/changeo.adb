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
with Ada.Strings.Unbounded;

with POSIX.File_Status;
with POSIX.Files;
with POSIX.Permissions;
with POSIX.Process_Identification;

procedure ChangeO is

   use Ada;
   use Ada.Strings.Unbounded;
   use POSIX;

   Users_SID    : constant String := "S-1-5-32-545";
   Everyone_SID : constant String := "S-1-1-0";

   package PPI renames POSIX.Process_Identification;

   O, G : Unbounded_String;

   ----------------
   -- Display_OG --
   ----------------

   procedure Display_OG (File : String) is
      Status : constant File_Status.Status :=
                 File_Status.Get_File_Status (To_POSIX_String (File));
      NO     : constant String :=
                 PPI.Image (File_Status.Owner_Of (Status));
      NG     : constant String :=
                 PPI.Image (File_Status.Group_Of (Status));
   begin
      Text_IO.Put_Line ("File: " & File);
      Text_IO.Put_Line (" owner: " & NO);
      Text_IO.Put_Line (" group: " & NG);

      if To_String (O) = NO then
         Text_IO.Put_Line ("Error, owner not changed.");
      else
         Text_IO.Put_Line ("OK, owner has been changed.");
      end if;

      if To_String (G) = NG then
         Text_IO.Put_Line ("Error, group not changed.");
      else
         Text_IO.Put_Line ("OK, group has changed.");
      end if;
   end Display_OG;

   Status : File_Status.Status;

begin
   --  Store current owner/group

   Status := File_Status.Get_File_Status ("check1");
   O := To_Unbounded_String (PPI.Image (File_Status.Owner_Of (Status)));
   G := To_Unbounded_String (PPI.Image (File_Status.Group_Of (Status)));

   Files.Change_Owner_And_Group
     ("check1", PPI.Value (Everyone_SID), PPI.Value (Users_SID));
   Display_OG ("check1");
end ChangeO;
