------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
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
