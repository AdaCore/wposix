------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Interfaces.C.Pointers;

with Win32.Winbase;
with Win32.Winerror;

with POSIX_Win32;

package body POSIX.Process_Environment is

   use Ada;

   type Char_Array_Access is access Win32.CHAR_Array;

   procedure Check_Name (Name : POSIX.POSIX_String; Message : String);
   --  Check that the environment variable is well formed (not empty and does
   --  not contains an equal or NULL character).

   function New_Environment (Size : Natural := 2) return Environment;
   --  Create a new Win32 environment

   procedure Free_Environment (Env : Environment);
   --  Free a Win32 environment

   ---------------
   -- Chars_Ptr --
   ---------------

   package Chars_Ptr is new Interfaces.C.Pointers
     (Index              => Natural,
      Element            => Win32.CHAR,
      Element_Array      => Win32.CHAR_Array,
      Default_Terminator => Win32.Nul);

   use type Win32.CHAR;

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
      if Env /= null then
         Free_Environment (Env);
      end if;
      Env := New_Environment;
   end Clear_Environment;

   -----------------------
   -- Clear_Environment --
   -----------------------

   procedure Clear_Environment is

      procedure Delete
        (Name, Value :        POSIX.POSIX_String;
         Quit        : in out Boolean);
      --  Delete environment variable named Name from the current environment

      ------------
      -- Delete --
      ------------

      procedure Delete
        (Name, Value :        POSIX.POSIX_String;
         Quit        : in out Boolean)
      is
         pragma Warnings (Off, Value);
      begin
         Delete_Environment_Variable (Name);
         Quit := False;
      end Delete;

      --------------------------------
      -- Delete_Current_Environment --
      --------------------------------

      procedure Delete_Current_Environment is new
        For_Every_Current_Environment_Variable (Delete);

   begin
      Delete_Current_Environment;
   end Clear_Environment;

   ----------------------
   -- Copy_Environment --
   ----------------------

   procedure Copy_Environment
     (Source :        Environment;
      Target : in out Environment)
   is
      function Size (Env : Environment) return Natural;
      --  Size (in bytes) used by the environment string

      ----------
      -- Size --
      ----------

      function Size (Env : Environment) return Natural is
         Pointer : Chars_Ptr.Pointer := Chars_Ptr.Pointer (Env);
         I       : Natural := 0;
         Prev    : Win32.CHAR := ' ';
      begin
         if Env /= null then
            loop
               I := I + 1;
               Prev := Pointer.all;
               Chars_Ptr.Increment (Pointer);

               --  On Windows the environment variable terminate with two null
               --  characters.
               exit when Prev = Win32.Nul and then Pointer.all = Win32.Nul;
            end loop;

            return I;

         else
            return 0;
         end if;
      end Size;

      Size_Source    : constant Natural := Size (Source);
      Pointer_Source : Chars_Ptr.Pointer := Chars_Ptr.Pointer (Source);
      Pointer_Target : Chars_Ptr.Pointer;

   begin
      if Target /= null then
         Free_Environment (Target);
      end if;

      Target := New_Environment (Size_Source + 1);

      if Size_Source = 0 then
         --  This is an empty environment, nothing to copy
         return;
      end if;

      Pointer_Target := Chars_Ptr.Pointer (Target);

      for I in 1 .. Size_Source + 1 loop
         Pointer_Target.all := Pointer_Source.all;
         Chars_Ptr.Increment (Pointer_Source);
         Chars_Ptr.Increment (Pointer_Target);
      end loop;
   end Copy_Environment;

   -----------------------------------
   -- Copy_From_Current_Environment --
   -----------------------------------

   procedure Copy_From_Current_Environment (Env : in out Environment) is
      Local_Env : Environment;
   begin
      Local_Env := Environment (Win32.Winbase.GetEnvironmentStrings);
      Copy_Environment (Local_Env, Env);
      Free_Environment (Local_Env);
   end Copy_From_Current_Environment;

   ---------------------------------
   -- Copy_To_Current_Environment --
   ---------------------------------

   procedure Copy_To_Current_Environment (Env : Environment) is

      procedure Set
        (Name, Value :        POSIX.POSIX_String;
         Quit        : in out Boolean);
      --  Add environment variable named Name with the given value into the
      --  current environment.

      ---------
      -- Set --
      ---------

      procedure Set
        (Name, Value :        POSIX.POSIX_String;
         Quit        : in out Boolean) is
      begin
         Set_Environment_Variable (Name, Value);
         Quit := False;
      end Set;

      -----------------------------
      -- Set_Current_Environment --
      -----------------------------

      procedure Set_Current_Environment is new
        For_Every_Environment_Variable (Set);

   begin
      Clear_Environment;
      Set_Current_Environment (Env);
   end Copy_To_Current_Environment;

   ---------------------------------
   -- Delete_Environment_Variable --
   ---------------------------------

   procedure Delete_Environment_Variable
     (Name :        POSIX.POSIX_String;
      Env  : in out Environment)
   is
      Current_Environment : Environment;
   begin
      Check_Name (Name, "Delete_Environment_Variable");

      Copy_From_Current_Environment (Current_Environment);
      Copy_To_Current_Environment (Env);
      Delete_Environment_Variable (Name);
      Copy_From_Current_Environment (Env);
      Copy_To_Current_Environment (Current_Environment);
   end Delete_Environment_Variable;

   ---------------------------------
   -- Delete_Environment_Variable --
   ---------------------------------

   procedure Delete_Environment_Variable (Name : POSIX.POSIX_String) is
      use type Win32.BOOL;
      use type Win32.DWORD;
      L_Name : constant String := POSIX.To_String (Name) & ASCII.NUL;
      Result : Win32.BOOL;
   begin
      Check_Name (Name, "Delete_Environment_Variable");
      Result := Win32.Winbase.SetEnvironmentVariable
        (Win32.Addr (L_Name), null);

      if Result = Win32.FALSE
        and then
          Win32.Winbase.GetLastError /= Win32.Winerror.ERROR_ENVVAR_NOT_FOUND
      then
         POSIX_Win32.Check_Result (Result, "Delete_Environment_Variable");
      end if;
   end Delete_Environment_Variable;

   --------------------------
   -- Environment_Value_Of --
   --------------------------

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Env       : Environment;
      Undefined : POSIX.POSIX_String := "") return POSIX.POSIX_String
   is
      use Ada.Strings.Unbounded;

      Value : Unbounded_String;
      Found : Boolean := False;

      procedure Equal
        (N, V :        POSIX.POSIX_String;
         Quit : in out Boolean);
      --  Check if N is equal to Name, in that case set Value to the
      --  corresponding value.

      -----------
      -- Equal --
      -----------

      procedure Equal
        (N, V :        POSIX.POSIX_String;
         Quit : in out Boolean) is
      begin
         if N = Name then
            Found := True;
            Quit  := True;
            Value := To_Unbounded_String (POSIX.To_String (V));
         else
            Quit  := False;
         end if;
      end Equal;

      ------------------
      -- For_Every_EV --
      ------------------

      procedure For_Every_EV is new For_Every_Environment_Variable (Equal);

   begin
      Check_Name (Name, "Environment_Value_Of");

      For_Every_EV (Env);

      if Found then
         return POSIX.To_POSIX_String (To_String (Value));
      else
         return Undefined;
      end if;
   end Environment_Value_Of;

   --------------------------
   -- Environment_Value_Of --
   --------------------------

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Undefined : POSIX.POSIX_String := "") return POSIX.POSIX_String
   is
      Current_Environment : Environment;
   begin
      Copy_From_Current_Environment (Current_Environment);
      declare
         Result : constant POSIX.POSIX_String :=
                    Environment_Value_Of
                      (Name, Current_Environment, Undefined);
      begin
         Free_Environment (Current_Environment);
         return Result;
      end;
   end Environment_Value_Of;

   --------------------------------------------
   -- For_Every_Current_Environment_Variable --
   --------------------------------------------

   procedure For_Every_Current_Environment_Variable is

      Current_Environment : Environment;

      procedure For_Every_CEV is new For_Every_Environment_Variable (Action);

   begin
      Copy_From_Current_Environment (Current_Environment);
      For_Every_CEV (Current_Environment);
      Free_Environment (Current_Environment);
   end For_Every_Current_Environment_Variable;

   ------------------------------------
   -- For_Every_Environment_Variable --
   ------------------------------------

   procedure For_Every_Environment_Variable (Env : Environment) is

      use type Chars_Ptr.Pointer;
      use type Win32.CHAR_Array;
      use type Interfaces.C.ptrdiff_t;

      Pointer : Chars_Ptr.Pointer := Chars_Ptr.Pointer (Env);
      Start   : Chars_Ptr.Pointer;

      function To_POSIX_String (CA : Win32.CHAR_Array) return POSIX_String;
      --  Convert from a CHAR_Array to a POSIX_String

      ---------------------
      -- To_POSIX_String --
      ---------------------

      function To_POSIX_String
        (CA : Win32.CHAR_Array) return POSIX_String is
      begin
         return POSIX.To_POSIX_String
           (Interfaces.C.To_Ada (Win32.To_C (CA & Win32.Nul)));
      end To_POSIX_String;

   begin
      --  if first character is 'nul' then the environment is empty

      if Env = null or else Pointer.all = Win32.Nul then
         return;
      end if;

      For_All_Variable : loop
         Start := Pointer;
         Chars_Ptr.Increment (Pointer);

         --  exit if there is no more environment variables

         exit For_All_Variable when
           Start.all = Win32.Nul and then Pointer.all = Win32.Nul;

         Search_Name_End : loop
            exit Search_Name_End when Pointer.all = '=';
            Chars_Ptr.Increment (Pointer);
         end loop Search_Name_End;

         declare
            Name : constant POSIX.POSIX_String :=
                     To_POSIX_String
                       (Chars_Ptr.Value (Start, Pointer - Start));
         begin
            Chars_Ptr.Increment (Pointer);
            Start := Pointer;

            Search_Value_End : loop
               exit Search_Value_End when Pointer.all = Win32.Nul;
               Chars_Ptr.Increment (Pointer);
            end loop Search_Value_End;

            declare
               Value : constant POSIX.POSIX_String := To_POSIX_String
                 (Chars_Ptr.Value (Start, Pointer - Start));
               Quit : Boolean := False;
            begin
               if Name (Name'First) /= '=' then
                  Action (Name, Value, Quit);
               end if;
               exit For_All_Variable when Quit;
            end;
         end;

         Chars_Ptr.Increment (Pointer);
         exit For_All_Variable when Pointer.all = Win32.Nul;

      end loop For_All_Variable;
   end For_Every_Environment_Variable;

   ----------------------
   -- Free_Environment --
   ----------------------

   procedure Free_Environment (Env : Environment) is
      Result : Win32.BOOL;
      pragma Unreferenced (Result);
   begin
      if Env /= null then
         Result := Win32.Winbase.FreeEnvironmentStrings (Win32.PCHAR (Env));
      end if;
   end Free_Environment;

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
         POSIX_Win32.Check_Retcode
           (POSIX_Win32.Retcode_Error, "Get_Working_Directory");
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
      Env  : Environment) return Boolean
   is

      Found : Boolean := False;

      procedure Equal
        (N, V :        POSIX.POSIX_String;
         Quit : in out Boolean);
      --  Check if N is equal to Name, in that case set Value to the
      --  corresponding value.

      -----------
      -- Equal --
      -----------

      procedure Equal
        (N, V :        POSIX.POSIX_String;
         Quit : in out Boolean)
      is
         pragma Warnings (Off, V);
      begin
         if N = Name then
            Found := True;
            Quit  := True;
         else
            Quit  := False;
         end if;
      end Equal;

      ------------------
      -- For_Every_EV --
      ------------------

      procedure For_Every_EV is new For_Every_Environment_Variable (Equal);

   begin
      Check_Name (Name, "Is_Environment_Variable");
      For_Every_EV (Env);
      return Found;
   end Is_Environment_Variable;

   -----------------------------
   -- Is_Environment_Variable --
   -----------------------------

   function Is_Environment_Variable
     (Name : POSIX.POSIX_String) return Boolean
   is
      Current_Environment : Environment;
      Result              : Boolean;
   begin
      Copy_From_Current_Environment (Current_Environment);
      Result := Is_Environment_Variable (Name, Current_Environment);
      Free_Environment (Current_Environment);
      return Result;
   end Is_Environment_Variable;

   ------------
   -- Length --
   ------------

   function Length (Env : Environment) return Natural is
      Pointer : Chars_Ptr.Pointer := Chars_Ptr.Pointer (Env);
      I       : Natural := 0;
      Prev    : Win32.CHAR := ' ';
   begin
      if Env /= null then
         loop
            Prev := Pointer.all;
            if Prev = '=' then
               I := I + 1;
            end if;

            Chars_Ptr.Increment (Pointer);

            --  On Windows the environment variable terminate with two null
            --  characters.
            exit when Prev = Win32.Nul and then Pointer.all = Win32.Nul;
         end loop;

         return I;

      else
         return 0;
      end if;
   end Length;

   ------------
   -- Length --
   ------------

   function Length return Natural is
      Current_Environment : Environment;
      Size                : Natural;
   begin
      Copy_From_Current_Environment (Current_Environment);
      Size := Length (Current_Environment);
      Free_Environment (Current_Environment);
      return Size;
   end Length;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment (Size : Natural := 2) return Environment is
      Char_Array : Char_Array_Access :=
                     new Win32.CHAR_Array (1 .. Natural'Max (Size, 2));
   begin
      Char_Array.all := (others => Win32.Nul);
      return Char_Array (1)'Access;
   end New_Environment;

   ------------------------------
   -- Set_Environment_Variable --
   ------------------------------

   procedure Set_Environment_Variable
     (Name  :        POSIX.POSIX_String;
      Value :        POSIX.POSIX_String;
      Env   : in out Environment)
   is
      Current_Environment : Environment;
   begin
      Check_Name (Name, "Set_Environment_Variable");

      Copy_From_Current_Environment (Current_Environment);
      Copy_To_Current_Environment (Env);
      Set_Environment_Variable (Name, Value);
      Copy_From_Current_Environment (Env);
      Copy_To_Current_Environment (Current_Environment);
   end Set_Environment_Variable;

   ------------------------------
   -- Set_Environment_Variable --
   ------------------------------

   procedure Set_Environment_Variable
     (Name  : POSIX.POSIX_String;
      Value : POSIX.POSIX_String)
   is
      L_Name  : constant String := POSIX.To_String (Name) & ASCII.NUL;
      L_Value : constant String := POSIX.To_String (Value) & ASCII.NUL;
      Result  : Win32.BOOL;
   begin
      Check_Name (Name, "Set_Environment_Variable");
      Result := Win32.Winbase.SetEnvironmentVariable
        (Win32.Addr (L_Name), Win32.Addr (L_Value));
      POSIX_Win32.Check_Result (Result, "Set_Environment_Variable");
   end Set_Environment_Variable;

end POSIX.Process_Environment;
