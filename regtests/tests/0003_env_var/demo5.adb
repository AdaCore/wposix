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
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;

with POSIX;
with POSIX_Process_Environment;

procedure Demo5 is

   use Ada;
   use Ada.Characters.Handling;

   package PPE renames POSIX_Process_Environment;

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   use String_Sets;

   Vars : Set;

   ------------
   -- Action --
   ------------

   procedure Action (Name, Value :        POSIX.POSIX_String;
                     Quit        : in out Boolean)
   is
      use type POSIX.POSIX_String;
   begin
      if Name = "HOME"
        or else Name = "HOMEDRIVE"
        or else Name = "PWD"
        or else Name = "USER"
      then
         Vars.Include (POSIX.To_String (Name) & " = <" &
                       To_Lower (POSIX.To_String (Name)) & ">");

      elsif Name = "OS"
        or else Name = "ada"
        or else Name = "Only"
        or else Name = "MYENV"
      then
         Vars.Include
           (POSIX.To_String (Name) & " = " & POSIX.To_String (Value));
      end if;
      Quit := False;
   end Action;

   ------------
   -- Output --
   ------------

   procedure Output (Position : Cursor) is
   begin
      Text_IO.Put_Line (Element (Position));
   end Output;

   ----------
   -- Dump --
   ----------

   procedure Dump is
   begin
      Vars.Iterate (Output'Access);
      Vars.Clear;
   end Dump;

   procedure Display_Env is new PPE.For_Every_Environment_Variable (Action);

   procedure Display_Env is new
     PPE.For_Every_Current_Environment_Variable (Action);

   Current_Env : PPE.Environment;

begin
   PPE.Set_Environment_Variable ("MYENV", "val");
   Display_Env;
   Dump;

   Text_IO.Put_Line ("1 ----------");
   PPE.Copy_From_Current_Environment (Current_Env);
   PPE.Set_Environment_Variable ("ada", "this is ada");
   PPE.Delete_Environment_Variable ("MYENV");
   Display_Env (Current_Env);
   Dump;

   Text_IO.Put_Line ("2 ----------");
   Display_Env;
   Dump;

   Text_IO.Put_Line ("3 ----------");
   PPE.Clear_Environment (Current_Env);
   Display_Env (Current_Env);
   Dump;

   Text_IO.Put_Line ("4 ----------");
   PPE.Copy_To_Current_Environment (Current_Env);
   Display_Env;
   Dump;

   Text_IO.Put_Line ("5 ----------");
   PPE.Set_Environment_Variable ("Only", "one", Current_Env);
   Display_Env (Current_Env);
   Dump;
end Demo5;
