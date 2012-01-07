------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Exceptions;

with Win32.Winbase;

with POSIX_Win32;

package body POSIX.Process_Environment is

   Current_Environment : Environment;

   procedure Check_Name (Name : POSIX.POSIX_String; Message : String);
   --  Check that the environment variable is well formed (not empty and does
   --  not contains an equal or NULL character).

   procedure Fill_Current_Environment (Name, Value : String);
   --  Fill current environment with the give name/value pair

   --  Process Parameters

   -------------------
   -- Argument_List --
   -------------------

   function Argument_List return POSIX.POSIX_String_List is
      Arg_List : POSIX.POSIX_String_List;
   begin
      --  The first item is the program name

      POSIX.Append (Arg_List, To_POSIX_String (Command_Line.Command_Name));

      --  Then add all program's arguments

      for I in 1 .. Command_Line.Argument_Count loop
         POSIX.Append (Arg_List, To_POSIX_String (Command_Line.Argument (I)));
      end loop;
      return Arg_List;
   end Argument_List;

   --  Environment Variables

   ------------------------------
   -- Change_Working_Directory --
   ------------------------------

   procedure Change_Working_Directory (Directory_Name : POSIX.Pathname) is
      L_Directory_Name : constant String :=
                           POSIX.To_String (Directory_Name) & ASCII.NUL;
      Result           : Win32.BOOL;
   begin
      Result := Win32.Winbase.SetCurrentDirectory
        (Win32.Addr (L_Directory_Name));
      POSIX_Win32.Check_Result (Result, "Change_Working_Directory");
   end Change_Working_Directory;

   ----------------
   -- Check_Name --
   ----------------

   procedure Check_Name
     (Name    : POSIX.POSIX_String;
      Message : String)
   is
      L_Name : constant String := POSIX.To_String (Name);
   begin
      if L_Name = ""
        or else Strings.Fixed.Index (L_Name, "=") /= 0
        or else Strings.Fixed.Index (L_Name, String'(1 => ASCII.NUL)) /= 0
      then
         POSIX.Set_Error_Code (POSIX.Invalid_Argument);
         Exceptions.Raise_Exception (POSIX.POSIX_Error'Identity, Message);
      end if;
   end Check_Name;

   -----------------------
   -- Clear_Environment --
   -----------------------

   procedure Clear_Environment (Env : in out Environment) is
   begin
      Env.Clear;
   end Clear_Environment;

   -----------------------
   -- Clear_Environment --
   -----------------------

   procedure Clear_Environment is
   begin
      Current_Environment.Clear;
   end Clear_Environment;

   ----------------------
   -- Copy_Environment --
   ----------------------

   procedure Copy_Environment
     (Source :        Environment;
      Target : in out Environment)
   is
      procedure Copy (Position : Strings_Map.Cursor);
      --  Copy item at Position into Target

      ----------
      -- Copy --
      ----------

      procedure Copy (Position : Strings_Map.Cursor) is
      begin
         Target.Insert
           (Strings_Map.Key (Position),
            Strings_Map.Element (Position));
      end Copy;

   begin
      Target.Clear;
      Source.Iterate (Copy'Access);
   end Copy_Environment;

   -----------------------------------
   -- Copy_From_Current_Environment --
   -----------------------------------

   procedure Copy_From_Current_Environment (Env : in out Environment) is
   begin
      Copy_Environment (Current_Environment, Env);
   end Copy_From_Current_Environment;

   ---------------------------------
   -- Copy_To_Current_Environment --
   ---------------------------------

   procedure Copy_To_Current_Environment (Env : Environment) is
   begin
      Copy_Environment (Env, Current_Environment);
   end Copy_To_Current_Environment;

   ---------------------------------
   -- Delete_Environment_Variable --
   ---------------------------------

   procedure Delete_Environment_Variable
     (Name :        POSIX.POSIX_String;
      Env  : in out Environment) is
   begin
      Check_Name (Name, "Delete_Environment_Variable");
      Env.Exclude (Name);
   end Delete_Environment_Variable;

   ---------------------------------
   -- Delete_Environment_Variable --
   ---------------------------------

   procedure Delete_Environment_Variable (Name : POSIX.POSIX_String) is
   begin
      Check_Name (Name, "Delete_Environment_Variable");
      Delete_Environment_Variable (Name, Current_Environment);
   end Delete_Environment_Variable;

   --------------------------
   -- Environment_Value_Of --
   --------------------------

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Env       : Environment;
      Undefined : POSIX.POSIX_String := "") return POSIX.POSIX_String
   is
      use type Strings_Map.Cursor;

      Position : constant Strings_Map.Cursor := Env.Find (Name);
   begin
      Check_Name (Name, "Environment_Value_Of");

      if Position = Strings_Map.No_Element then
         return Undefined;
      else
         return Strings_Map.Element (Position);
      end if;
   end Environment_Value_Of;

   --------------------------
   -- Environment_Value_Of --
   --------------------------

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Undefined : POSIX.POSIX_String := "") return POSIX.POSIX_String is
   begin
      return Environment_Value_Of (Name, Current_Environment, Undefined);
   end Environment_Value_Of;

   ------------------------------
   -- Fill_Current_Environment --
   ------------------------------

   procedure Fill_Current_Environment (Name, Value : String) is
   begin
      Current_Environment.Insert
        (POSIX.To_POSIX_String (Name),
         POSIX.To_POSIX_String (Value));
   end Fill_Current_Environment;

   --------------------------------------------
   -- For_Every_Current_Environment_Variable --
   --------------------------------------------

   procedure For_Every_Current_Environment_Variable is
      procedure For_Every_CEV is new For_Every_Environment_Variable (Action);
   begin
      For_Every_CEV (Current_Environment);
   end For_Every_Current_Environment_Variable;

   ------------------------------------
   -- For_Every_Environment_Variable --
   ------------------------------------

   procedure For_Every_Environment_Variable (Env : Environment) is

      use type Strings_Map.Cursor;

      Quit : Boolean := False;

      procedure Action (Position : Strings_Map.Cursor);
      --  Call action with the name/value for the pointed environment variable

      ------------
      -- Action --
      ------------

      procedure Action (Position : Strings_Map.Cursor) is
      begin
         Action
           (Strings_Map.Key (Position), Strings_Map.Element (Position), Quit);
      end Action;

      Position : Strings_Map.Cursor := Env.First;

   begin
      while Position /= Strings_Map.No_Element loop
         Action (Position);
         exit when Quit;
         Strings_Map.Next (Position);
      end loop;
   end For_Every_Environment_Variable;

   ---------------------------
   -- Get_Working_Directory --
   ---------------------------

   function Get_Working_Directory return POSIX.Pathname is
      use type Win32.DWORD;
      use type Win32.INT;
      Max_Len      : constant := 500;
      Buffer       : String (1 .. Max_Len);
      pragma Warnings (Off, Buffer);
      Number_Bytes : Win32.DWORD;
   begin
      Number_Bytes := Win32.Winbase.GetCurrentDirectory
        (Win32.DWORD (Max_Len), Win32.Addr (Buffer));

      if Number_Bytes = 0 then
         POSIX_Win32.Raise_Last_Error ("Get_Working_Directory");
      end if;

      declare
         Pathname : POSIX.Pathname (1 .. Integer (Number_Bytes));
      begin
         for I in Pathname'Range loop
            Pathname (I) := POSIX_Character (Buffer (I));
         end loop;
         return Pathname;
      end;
   end Get_Working_Directory;

   -----------------------------
   -- Is_Environment_Variable --
   -----------------------------

   function Is_Environment_Variable
     (Name : POSIX.POSIX_String;
      Env  : Environment) return Boolean is
   begin
      Check_Name (Name, "Is_Environment_Variable");
      return Env.Contains (Name);
   end Is_Environment_Variable;

   -----------------------------
   -- Is_Environment_Variable --
   -----------------------------

   function Is_Environment_Variable
     (Name : POSIX.POSIX_String) return Boolean is
   begin
      return Is_Environment_Variable (Name, Current_Environment);
   end Is_Environment_Variable;

   ------------
   -- Length --
   ------------

   function Length (Env : Environment) return Natural is
   begin
      return Natural (Strings_Map.Map (Env).Length);
   end Length;

   ------------
   -- Length --
   ------------

   function Length return Natural is
   begin
      return Current_Environment.Length;
   end Length;

   -----------------------
   -- POSIX_String_Hash --
   -----------------------

   function POSIX_String_Hash
     (Str : POSIX.POSIX_String) return Containers.Hash_Type is
   begin
      return Strings.Hash (POSIX.To_String (Str));
   end POSIX_String_Hash;

   ------------------------------
   -- Set_Environment_Variable --
   ------------------------------

   procedure Set_Environment_Variable
     (Name  :        POSIX.POSIX_String;
      Value :        POSIX.POSIX_String;
      Env   : in out Environment) is
   begin
      Check_Name (Name, "Set_Environment_Variable");
      Env.Include (Name, Value);
   end Set_Environment_Variable;

   ------------------------------
   -- Set_Environment_Variable --
   ------------------------------

   procedure Set_Environment_Variable
     (Name  : POSIX.POSIX_String;
      Value : POSIX.POSIX_String) is
   begin
      Check_Name (Name, "Set_Environment_Variable");
      Current_Environment.Include (Name, Value);
   end Set_Environment_Variable;

begin
   Environment_Variables.Iterate (Fill_Current_Environment'Access);
end POSIX.Process_Environment;
