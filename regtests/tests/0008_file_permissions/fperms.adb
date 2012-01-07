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
