
--  $Id$

with System;
with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;

with Win32;
with Win32.Winbase;
with Win32.Winerror;

with POSIX_Win32;

package body POSIX_Process_Environment is


   use Ada;

   ---------------
   -- Chars_Ptr --
   ---------------

   package Chars_Ptr is new Interfaces.C.Pointers
     (Index              => Natural,
      Element            => Win32.CHAR,
      Element_Array      => Win32.Char_Array,
      Default_Terminator => Win32.Nul);

   use type Win32.CHAR;

   Result : Win32.BOOL;

   --  Process Parameters

   -------------------
   -- Argument_List --
   -------------------

   function Argument_List
     return POSIX.POSIX_String_List
   is
      Arg_List : POSIX.POSIX_String_List;
   begin
      for I in 1 .. Command_Line.Argument_Count loop
         POSIX.Append (Arg_List,
                       To_POSIX_String (Command_Line.Argument (I)));
      end loop;
      return Arg_List;
   end Argument_List;


   --  Environment Variables


   ----------------
   -- Check_Name --
   ----------------

   procedure Check_Name (Name    : in POSIX.POSIX_String;
                         Message : in String)
   is
      L_Name : constant String := POSIX.To_String (Name);
   begin
      if L_Name = "" or else Strings.Fixed.Index (L_Name, "=") /= 0 then
         POSIX.Set_Error_Code (POSIX.Invalid_Argument);
         Exceptions.Raise_Exception (POSIX.POSIX_Error'Identity,
                                     Message);
      end if;
   end Check_Name;


   type Char_Array_Access is access Win32.Char_Array;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment (Size : in Positive := 2)
                             return Environment
   is
      Char_Array : Char_Array_Access := new Win32.Char_Array (1 .. Size);
   begin
      Char_Array.all := (others => Win32.Nul);
      return Char_Array (1)'Access;
   end New_Environment;

   procedure Free_Environment (Env : in Environment) is
   begin
      Result := Win32.Winbase.FreeEnvironmentStrings (Win32.PCHAR (Env));
   end Free_Environment;


   -----------------------------------
   -- Copy_From_Current_Environment --
   -----------------------------------

   procedure Copy_From_Current_Environment
     (Env : in out Environment)
   is
      Local_Env : Environment;
   begin
      Local_Env := Environment (Win32.Winbase.GetEnvironmentStrings);
      Copy_Environment (Local_Env, Env);
      Free_Environment (Local_Env);
   end Copy_From_Current_Environment;


   ---------------------------------
   -- Copy_To_Current_Environment --
   ---------------------------------

   procedure Copy_To_Current_Environment
     (Env : in Environment)
   is

      procedure Set (Name, Value : in     POSIX.POSIX_String;
                     Quit        : in out Boolean) is
      begin
         Set_Environment_Variable (Name, Value);
         Quit := False;
      end Set;

      procedure Set_Current_Environment is new
        For_Every_Environment_Variable (Set);

   begin
      Clear_Environment;
      Set_Current_Environment (Env);
   end Copy_To_Current_Environment;


   ----------------------
   -- Copy_Environment --
   ----------------------

   procedure Copy_Environment
     (Source : in     Environment;
      Target : in out Environment)
   is
      Pointer_Source : Chars_Ptr.Pointer := Chars_Ptr.Pointer (Source);
      Pointer_Target : Chars_Ptr.Pointer;
      Length_Source  : Natural := Length (Source);
   begin
      if Target /= null then
         Free_Environment (Target);
      end if;

      Target := New_Environment (Length_Source + 1);
      Pointer_Target := Chars_Ptr.Pointer (Target);

      for I in 1 .. Length_Source + 1 loop
         Pointer_Target.all := Pointer_Source.all;
         Chars_Ptr.Increment (Pointer_Source);
         Chars_Ptr.Increment (Pointer_Target);
      end loop;
   end Copy_Environment;


   --------------------------
   -- Environment_Value_Of --
   --------------------------

   function Environment_Value_Of
     (Name       : POSIX.POSIX_String;
      Env        : Environment;
      Undefined  : POSIX.POSIX_String := "")
      return POSIX.POSIX_String
   is
      use Ada.Strings.Unbounded;

      Value : Unbounded_String;
      Found : Boolean := False;

      procedure Equal (N, V : in     POSIX.POSIX_String;
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

      procedure For_Every_EV is
         new For_Every_Environment_Variable (Equal);

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
      Undefined : POSIX.POSIX_String := "")
      return POSIX.POSIX_String
   is
      Current_Environment : Environment;
   begin
      Copy_From_Current_Environment (Current_Environment);
      declare
         Result : constant POSIX.POSIX_String
           := Environment_Value_Of (Name, Current_Environment);
      begin
         Free_Environment (Current_Environment);
         return Result;
      end;
   end Environment_Value_Of;


   -----------------------------
   -- Is_Environment_Variable --
   -----------------------------

   function Is_Environment_Variable
     (Name      : POSIX.POSIX_String;
      Env       : Environment)
      return Boolean
   is

      Found : Boolean := False;

      procedure Equal (N, V : in     POSIX.POSIX_String;
                       Quit : in out Boolean) is
      begin
         if N = Name then
            Found := True;
            Quit  := True;
         else
            Quit  := False;
         end if;
      end Equal;

      procedure For_Every_EV is
         new For_Every_Environment_Variable (Equal);

   begin
      Check_Name (Name, "Is_Environment_Variable");
      For_Every_EV (Env);
      return Found;
   end Is_Environment_Variable;


   -----------------------------
   -- Is_Environment_Variable --
   -----------------------------

   function Is_Environment_Variable
     (Name      : POSIX.POSIX_String)
      return Boolean
   is
      Current_Environment : Environment;
      Result              : Boolean;
   begin
      Copy_From_Current_Environment (Current_Environment);
      Result := Is_Environment_Variable (Name, Current_Environment);
      Free_Environment (Current_Environment);
      return Result;
   end Is_Environment_Variable;


   -----------------------
   -- Clear_Environment --
   -----------------------

   procedure Clear_Environment
     (Env  : in out Environment)
   is
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

      procedure Delete (Name, Value : in     POSIX.POSIX_String;
                        Quit        : in out Boolean) is
      begin
         Delete_Environment_Variable (Name);
         Quit := False;
      end Delete;

      procedure Delete_Current_Environment is new
        For_Every_Current_Environment_Variable (Delete);

   begin
      Delete_Current_Environment;
   end Clear_Environment;


   ------------------------------
   -- Set_Environment_Variable --
   ------------------------------

   procedure Set_Environment_Variable
     (Name  : in     POSIX.POSIX_String;
      Value : in     POSIX.POSIX_String;
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
     (Name  : in     POSIX.POSIX_String;
      Value : in     POSIX.POSIX_String)
   is
      L_Name  : constant String := POSIX.To_String (Name) & ASCII.Nul;
      L_Value : constant String := POSIX.To_String (Value) & ASCII.Nul;
   begin
      Check_Name (Name, "Set_Environment_Variable");
      Result := Win32.Winbase.SetEnvironmentVariable (Win32.Addr (L_Name),
                                                      Win32.Addr (L_Value));
      POSIX_Win32.Check_Result (Result, "Set_Environment_Variable");
   end Set_Environment_Variable;


   ---------------------------------
   -- Delete_Environment_Variable --
   ---------------------------------

   procedure Delete_Environment_Variable
     (Name : in     POSIX.POSIX_String;
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

   procedure Delete_Environment_Variable
     (Name : in     POSIX.POSIX_String)
   is
      use type Win32.BOOL;
      use type Win32.DWORD;
      L_Name : constant String := POSIX.To_String (Name) & ASCII.Nul;
   begin
      Check_Name (Name, "Delete_Environment_Variable");
      Result := Win32.Winbase.SetEnvironmentVariable (Win32.Addr (L_Name),
                                                      null);
      if Result = Win32.False and
        Win32.Winbase.GetLastError /=
        Win32.Winerror.ERROR_ENVVAR_NOT_FOUND then
         POSIX_Win32.Check_Result (Result, "Delete_Environment_Variable");
      end if;
   end Delete_Environment_Variable;


   ------------
   -- Length --
   ------------

   function Length (Env : Environment)
                    return Natural
   is
      Pointer : Chars_Ptr.Pointer := Chars_Ptr.Pointer (Env);
      I : Natural := 0;
   begin
      loop
         I := I + 1;
         if Pointer.all = Win32.Nul then
            Chars_Ptr.Increment (Pointer);
            exit when Pointer.all = Win32.Nul;
         else
            Chars_Ptr.Increment (Pointer);
         end if;
      end loop;
      return I;
   end Length;

   ------------
   -- Length --
   ------------

   function Length
     return Natural
   is
      Current_Environment : Environment;
      Size                : Natural;
   begin
      Copy_From_Current_Environment (Current_Environment);
      Size := Length (Current_Environment);
      Free_Environment (Current_Environment);
      return Size;
   end Length;


   ------------------------------------
   -- For_Every_Environment_Variable --
   ------------------------------------

   procedure For_Every_Environment_Variable
     (Env : in Environment)
   is

      use type Chars_Ptr.Pointer;
      use type Win32.Char_Array;
      use type Interfaces.C.Ptrdiff_T;

      Pointer : Chars_Ptr.Pointer := Chars_Ptr.Pointer (Env);
      Start   : Chars_Ptr.Pointer;

      function To_POSIX_String (CA : in Win32.Char_Array)
                                return POSIX_String is
      begin
         return POSIX.To_Posix_String
           (Interfaces.C.To_Ada (Win32.To_C (CA & Win32.Nul)));
      end To_POSIX_String;

   begin
      For_All_Variable :
      loop
         Start := Pointer;
         Chars_Ptr.Increment (Pointer);

         --  exit if the Env variable is empty
         exit when Start.all = Win32.Nul and then Pointer.all = Win32.Nul;

         Search_Name_End :
         loop
            exit Search_Name_End when Pointer.all = '=';
            Chars_Ptr.Increment (Pointer);
         end loop Search_Name_End;

         declare
            Name : constant POSIX.POSIX_String := To_Posix_String
              (Chars_Ptr.Value (Start, Pointer - Start));
         begin
            Chars_Ptr.Increment (Pointer);
            Start := Pointer;

            Search_Value_End :
            loop
               exit Search_Value_End when Pointer.all = Win32.Nul;
               Chars_Ptr.Increment (Pointer);
            end loop Search_Value_End;

            declare
               Value : constant POSIX.POSIX_String := To_Posix_String
                 (Chars_Ptr.Value (Start, Pointer - Start));
               Quit : Boolean;
            begin
               if Name (Name'First) /= '=' then
                  Action (Name, Value, Quit);
               else
                  Quit := False;
               end if;
               exit For_All_Variable when Quit;
            end;
         end;

         Chars_Ptr.Increment (Pointer);
         exit For_All_Variable when Pointer.all = Win32.Nul;

      end loop For_All_Variable;
   end For_Every_Environment_Variable;

   --------------------------------------------
   -- For_Every_Current_Environment_Variable --
   --------------------------------------------

   procedure For_Every_Current_Environment_Variable is

      Current_Environment : Environment;

      procedure For_Every_CEV is
        new For_Every_Environment_Variable (Action);

   begin
      Copy_From_Current_Environment (Current_Environment);
      For_Every_CEV (Current_Environment);
      Free_Environment (Current_Environment);
   end For_Every_Current_Environment_Variable;


   --  Process Working Directory

   ------------------------------
   -- Change_Working_Directory --
   ------------------------------

   procedure Change_Working_Directory
     (Directory_Name : in POSIX.Pathname)
   is
      L_Directory_Name : constant String
        := POSIX.To_String (Directory_Name) & ASCII.Nul;
   begin
      Result := Win32.Winbase.SetCurrentDirectory
        (Win32.Addr (L_Directory_Name));
      POSIX_Win32.Check_Result (Result, "Change_Working_Directory");
   end Change_Working_Directory;


   ---------------------------
   -- Get_Working_Directory --
   ---------------------------

   function Get_Working_Directory
     return POSIX.Pathname
   is
      use type Win32.DWORD;
      use type Win32.INT;
      Max_Len      : constant := 500;
      Buffer       : String (1 .. Max_Len);
      Number_Bytes : Win32.DWORD;
   begin
      Number_Bytes := Win32.Winbase.GetCurrentDirectory
        (Win32.DWORD (Max_Len),
         Win32.Addr (Buffer));

      if Number_Bytes = 0 then
         POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error,
                                    "Get_Working_Directory");
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

end POSIX_Process_Environment;
