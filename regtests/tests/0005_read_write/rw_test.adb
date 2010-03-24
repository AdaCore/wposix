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
with POSIX.IO;
with POSIX.Permissions;

procedure RW_Test is

   use Ada;
   use POSIX;
   use POSIX.Files;

   Filename : constant POSIX_String := "afile.txt";

   File     : IO.File_Descriptor;
   Count    : IO_Count;
   Buffer   : IO.IO_Buffer (1 .. 10) := To_POSIX_String ("0123456789");
   Read     : IO.IO_Buffer (1 .. 100);

begin
   --  Write

   File := IO.Open_Or_Create
     (Filename,
      IO.Write_Only,
      Permissions.Owner_Permission_Set);

   IO.Write (File, Buffer, Count);

   if not IO.Is_Open (File) then
      Text_IO.Put_Line ("File should be opened.");
   end if;

   IO.Close (File);

   if IO.Is_Open (File) then
      Text_IO.Put_Line ("File should be closed.");
   end if;

   if Files.Is_File_Present (Filename) then
      Text_IO.Put_Line ("Ok, file present.");
   else
      Text_IO.Put_Line ("Nok, file is not created");
   end if;

   --  Read

   File := IO.Open_Or_Create
     (Filename,
      IO.Read_Only,
      Permissions.Owner_Permission_Set);

   Count := 0;
   IO.Read (File, Read, Count);
   IO.Close (File);

   if Count = 10 and then Read (1 .. Integer (Count)) = Buffer then
      Text_IO.Put_Line ("Ok, proper content found.");
   else
      Text_IO.Put_Line
        ("Nok, content is '" & To_String (Read (1 .. Integer (Count)))
         & "' size=" & IO_Count'Image (Count));
   end if;

   --  Append

   File := IO.Open (Filename, IO.Write_Only, Options => IO.Append);

   IO.Write (File, Buffer, Count);

   IO.Close (File);

   --  Read

   File := IO.Open (Filename, IO.Read_Only);

   Count := 0;
   IO.Read (File, Read, Count);
   IO.Close (File);

   if Count = 20
     and then Read (1 .. Integer (Count)) = Buffer & Buffer
   then
      Text_IO.Put_Line ("Ok append, proper content found.");
   else
      Text_IO.Put_Line
        ("Nok append, content is '" & To_String (Read (1 .. Integer (Count)))
         & "' size=" & IO_Count'Image (Count));
   end if;

   Unlink (Filename);
end RW_Test;
