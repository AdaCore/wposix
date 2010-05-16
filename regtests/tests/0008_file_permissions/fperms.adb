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
with POSIX.Permissions;

procedure FPerms is

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

begin
   Display_Perms ("check1");
   Display_Perms ("check2");
   Display_Perms ("check3");
   Display_Perms ("check4");
   Display_Perms ("check5");
end FPerms;
