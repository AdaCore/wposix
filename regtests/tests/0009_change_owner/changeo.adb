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
