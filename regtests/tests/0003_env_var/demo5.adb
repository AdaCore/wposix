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
        or else Name = "PATH"
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

   --------
   -- Ok --
   --------

   procedure Ok (Name, Value :        POSIX.POSIX_String;
                 Quit        : in out Boolean)
   is
      use type POSIX.POSIX_String;
   begin
      Text_IO.Put_Line ("Should not be called");
      Quit := True;
   end Ok;

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

   procedure Check_Env is new PPE.For_Every_Environment_Variable (Ok);

   procedure Display_Env is new
     PPE.For_Every_Current_Environment_Variable (Action);

   Current_Env : PPE.Environment;
   Env2        : PPE.Environment;

begin
   Check_Env (Current_Env);
   PPE.Copy_Environment (Current_Env, Env2);

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
   Text_IO.Put_Line ("length " & PPE.Length (Current_Env)'Img);
   Dump;
end Demo5;
