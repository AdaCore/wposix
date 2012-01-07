------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Text_IO;

with POSIX.Process_Environment;

procedure Prog is
   use Ada;

   package PPE renames POSIX.Process_Environment;

   Alist : constant POSIX.POSIX_String_List := PPE.Argument_List;

   OS_Exists : Boolean := False;

   procedure Display (Name, Value : POSIX.POSIX_String; Quit : in out Boolean);
   --  Display environment variables

   -------------
   -- Display --
   -------------

   procedure Display
     (Name, Value : POSIX.POSIX_String; Quit : in out Boolean)
   is
      use type POSIX.POSIX_String;
   begin
      if Name'Length > 10
        and then Name (Name'First .. Name'First + 9) = "START_PROC"
      then
         Text_IO.Put_Line (POSIX.To_String (Name & "=" & Value));
      elsif Name = "OS" then
         OS_Exists := True;
      end if;
      Quit := False;
   end Display;

   procedure Display_Env is
      new PPE.For_Every_Current_Environment_Variable (Display);

begin
   --  Using Ada standard

   Text_IO.Put_Line ("Count : " & Natural'Image (Command_Line.Argument_Count));

   for K in 1 .. Command_Line.Argument_Count loop
      Text_IO.Put_Line (" > " & Command_Line.Argument (K));
   end loop;

   --  Using POSIX standard

   Text_IO.Put_Line ("Count : " & Natural'Image (POSIX.Length (Alist) - 1));

   for K in 2 .. POSIX.Length (Alist) loop
      Text_IO.Put_Line (" > " & POSIX.To_String (POSIX.Value (Alist, K)));
   end loop;

   Text_IO.Put_Line ("Display environment:");

   Display_Env;

   Text_IO.Put_Line ("OS exists " & Boolean'Image (OS_Exists));

   Command_Line.Set_Exit_Status (Command_Line.Success);
end Prog;
