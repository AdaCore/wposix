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

with Ada.Exceptions;
with Ada.Text_IO;

with POSIX;
with POSIX.Files;
with POSIX.IO;
with POSIX.Permissions;
with POSIX.Process_Environment;

procedure Demo2 is

   use Ada;
   use Ada.Text_IO;

   use POSIX;
   use POSIX.Files;

   Dir : constant POSIX_String := To_POSIX_String ("dir");

   -------------
   -- Display --
   -------------

   procedure Display (Dirent :        Directory_Entry;
                      Quit   : in out Boolean) is
   begin
      Put_Line (To_String (Filename_Of (Dirent)));
      Quit := False;
   end Display;

   ------------
   -- Create --
   ------------

   procedure Create (Name : String) is
      P_Name : constant POSIX_String := Dir & "\" & To_POSIX_String (Name);
      File   : IO.File_Descriptor;
   begin
      File := IO.Open_Or_Create
        (P_Name, IO.Read_Only, Permissions.Owner_Permission_Set);
      IO.Close (File);
   end Create;

   procedure Display_Directory is
      new For_Every_Directory_Entry (Action => Display);

begin
   --  First populate a directory with some files

   Create_Directory (Dir, Permissions.Owner_Permission_Set);

   Create ("toto");
   Create ("file.txt");
   Create ("README");

   --  Now list directory content

   Put_Line ("List directory dir");
   Display_Directory (Dir);

   Put_Line ("List directory .");
   Process_Environment.Change_Working_Directory (Dir);
   Display_Directory (To_POSIX_String ("."));

   begin
      New_Line;
      Put_Line ("List directory /tmp-does-not-exists");
      Display_Directory (To_POSIX_String ("/tmp-does-not-exists"));
   exception
      when E : others =>
         Put_Line (Exceptions.Exception_Name (E) & " - " &
                   Exceptions.Exception_Message (E) & " : " &
                   POSIX.Image (POSIX.Get_Error_Code));
   end;

   begin
      New_Line;
      Put_Line ("List directory /tmp/toto-does-not-exists");
      Display_Directory (To_POSIX_String ("/tmp/toto-does-not-exists"));
   exception
      when E : others =>
         Put_Line (Exceptions.Exception_Name (E) & " - " &
                   Exceptions.Exception_Message (E) & " : " &
                   POSIX.Image (POSIX.Get_Error_Code));
   end;
end Demo2;
