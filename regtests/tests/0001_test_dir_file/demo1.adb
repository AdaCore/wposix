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

with POSIX.Files;
with POSIX.File_Status;
with POSIX.IO;
with POSIX.Permissions;

procedure Demo1 is

   use Ada;
   use POSIX;
   use POSIX.Files;
   use POSIX.File_Status;

   procedure Check (Path : String);
   --  Check path

   -----------
   -- Check --
   -----------

   procedure Check (Path : String) is
   begin
      Text_IO.Put_Line ("stat : [" & Path & "]");

      if Is_File_Present (To_POSIX_String (Path)) then
         Text_IO.Put_Line (Path & " is present");

         if Is_Regular_File (Get_File_Status (To_POSIX_String (Path))) then
            Text_IO.Put_Line (Path & " is a regular file");
         else
            Text_IO.Put_Line (Path & " is not a regular file");
         end if;

         if Is_Directory (To_POSIX_String (Path)) then
            Text_IO.Put_Line (Path & " is a directory");
         else
            Text_IO.Put_Line (Path & " is not a directory");
         end if;

      else
         Text_IO.Put_Line (Path & " is not present");
      end if;

      Text_IO.New_Line;
   end Check;

   Dirname  : constant String := "adirectory";
   Filename : constant String := Dirname & "\thisisafile.txt";

   File     : IO.File_Descriptor;
   Count    : IO_Count;

begin
   Files.Create_Directory
     (To_POSIX_String (Dirname),
      Permissions.Owner_Permission_Set);

   File := IO.Open_Or_Create
     (To_POSIX_String (Filename),
      IO.Read_Only,
      Permissions.Owner_Permission_Set);
   IO.Close (File);

   Check (Dirname);
   Check (Filename);

   Unlink (To_POSIX_String (Filename));

   Check (Dirname);
   Check (Filename);

   Remove_Directory (To_POSIX_String (Dirname));

   Check (Dirname);
   Check (Filename);
end Demo1;
