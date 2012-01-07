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
