
--  $Id$

with System;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Interfaces.C.Strings;
with Win32;
with Win32.Direct;
with Win32.Malloc;

with POSIX_Win32;

package body POSIX_Process_Environment is

   pragma Suppress (Index_Check);

   use Ada;
   use type Win32.PCHAR;

   Current_Environment : Win32.Stdlib.String_Array_Access
     renames Win32.Stdlib.Environ;


   --  Process Parameters

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

                -----------------------------------

   procedure Check_Name (Name    : in POSIX.POSIX_String;
                         Message : in String)
   is
      L_Name : constant String := POSIX.To_String (Name);
   begin
      if L_Name = "" or else Strings.Fixed.Index (L_Name, "=") /= 0 then
         Exceptions.Raise_Exception (POSIX.POSIX_Error'Identity,
                                     Message);
         POSIX.Set_Error_Code (POSIX.Invalid_Argument);
      end if;
   end Check_Name;

                -----------------------------------

   function New_Environment (Size : in Positive := 1)
                             return Environment
   is
      type New_Env is array (1 .. Size) of aliased Win32.PSTR;
      type New_Env_Access is access New_Env;

      function To_Environment is
        new Ada.Unchecked_Conversion (New_Env_Access, Environment);

      NEA : New_Env_Access;
      Env : Environment;

   begin
      NEA := new New_Env'(others => null);
      Env := To_Environment (NEA);
      return Env;
   end New_Environment;

   procedure Free_Environ_Array (Env : in Environment) is
   begin
      Win32.Malloc.Free (Env (0)'Address);
   end Free_Environ_Array;

                -----------------------------------

   procedure Copy_From_Current_Environment
     (Env : in out Environment) is
   begin
      Copy_Environment (Source => Environment (Current_Environment),
                        Target => Env);
   end Copy_From_Current_Environment;


   procedure Copy_To_Current_Environment
     (Env : in Environment) is
   begin
      Copy_Environment (Source => Env,
                        Target => Environment (Current_Environment));
   end Copy_To_Current_Environment;


   procedure Copy_Environment
     (Source : in     Environment;
      Target : in out Environment)
   is
      Length_Source : Natural := Length (Source);
   begin
      Clear_Environment (Target);
      Target := New_Environment (Length_Source + 1);
      for I in 0 .. Length_Source loop
         Target (I) := Source (I);
      end loop;
   end Copy_Environment;

                -----------------------------------

   function Environment_Value_Of
     (Name       : POSIX.POSIX_String;
      Env        : Environment;
      Undefined  : POSIX.POSIX_String := "")
      return POSIX.POSIX_String
   is

      use Strings.Unbounded;

      V : Unbounded_String
        := To_Unbounded_String (POSIX.To_String (Undefined));

      procedure Equal (VName, VValue : in     POSIX.POSIX_String;
                       Quit          : in out Boolean) is
      begin
         if Name = VName then
            V := To_Unbounded_String (POSIX.To_String (VValue));
            Quit  := True;
         else
            Quit := False;
         end if;
      end Equal;

      procedure Search_Variable is
         new For_Every_Environment_Variable (Equal);

   begin
      Check_Name (Name, "Environment_Value_Of");
      Search_Variable (Env);
      return POSIX.To_POSIX_String (To_String (V));
   end Environment_Value_Of;


   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Undefined : POSIX.POSIX_String := "")
      return POSIX.POSIX_String is
   begin
      return Environment_Value_Of (Name,
                                   Environment (Current_Environment),
                                   Undefined);
   end Environment_Value_Of;

                -----------------------------------

   function Is_Environment_Variable
     (Name      : POSIX.POSIX_String;
      Env       : Environment)
      return Boolean
   is
      Found : Boolean := False;

      procedure Equal (VName, VValue : in     POSIX.POSIX_String;
                       Quit          : in out Boolean) is
      begin
         if Name = VName then
            Found := True;
            Quit  := True;
         else
            Quit := False;
         end if;
      end Equal;

      procedure Search_Variable is
         new For_Every_Environment_Variable (Equal);

   begin
      Check_Name (Name, "Is_Environment_Variable");
      Search_Variable (Env);
      return Found;
   end Is_Environment_Variable;


   function Is_Environment_Variable
     (Name      : POSIX.POSIX_String)
      return Boolean
   is
   begin
      return Is_Environment_Variable (Name,
                                      Environment (Current_Environment));
   end Is_Environment_Variable;

                -----------------------------------

   procedure Clear_Environment
     (Env  : in out Environment)
   is
      use Interfaces;
      I : Natural := 0;
   begin
      if Env /= null then
         loop
            exit when Env (I) = null;
            declare
               E : C.Strings.Chars_Ptr := Win32.To_Chars_Ptr (Env (I));
            begin
               C.Strings.Free (E);
            end;
            I := I + 1;
         end loop;
         Free_Environ_Array (Env);
      end if;
      Env := New_Environment;
   end Clear_Environment;


   procedure Clear_Environment is
   begin
      Clear_Environment (Environment (Current_Environment));
   end Clear_Environment;

                -----------------------------------

   procedure Set_Environment_Variable
     (Name  : in     POSIX.POSIX_String;
      Value : in     POSIX.POSIX_String;
      Env   : in out Environment)
   is
      use Interfaces;
      Found : Boolean := False;
      I     : Natural := 0;
   begin
      Check_Name (Name, "Set_Environment_Variable");
      loop
         exit when Env (I) = null;

         Get_Variable :
         declare
            E : constant String := POSIX_Win32.Chars_Ptr_To_String (Env (I));
            P : Natural         := Strings.Fixed.Index (E, "=");
         begin
            if E (E'First .. P - 1) = POSIX.To_String (Name) then

               Free_Variable :
               declare
                  E : C.Strings.Chars_Ptr := Win32.To_Chars_Ptr (Env (I));
               begin
                  C.Strings.Free (E);
               end Free_Variable;

               Add_New_Value :
               declare
                  E : String := POSIX.To_String (Name) & '=' &
                    POSIX.To_String (Value);
               begin
                  Env (I) := Win32.To_PSTR (C.Strings.New_String (E));
               end Add_New_Value;

               Found := True;
               exit;
            end if;
         end Get_Variable;
         I := I + 1;
      end loop;

      if not Found then
         declare
            L_Env   : Natural     := Length (Env);
            New_Env : Environment := New_Environment (L_Env + 2);
         begin
            for I in 0 .. L_Env - 1 loop
               New_Env (I) := Env (I);
            end loop;

            Add_New_Variable :
            declare
               E : String := POSIX.To_String (Name) & '=' &
                 POSIX.To_String (Value);
            begin
               New_Env (L_Env) := Win32.To_PSTR (C.Strings.New_String (E));
            end Add_New_Variable;

            Free_Environ_Array (Env);
            Env := New_Env;
         end;
      end if;
   end Set_Environment_Variable;



   procedure Set_Environment_Variable
     (Name  : in     POSIX.POSIX_String;
      Value : in     POSIX.POSIX_String) is
   begin
      Set_Environment_Variable (Name,
                                Value,
                                Environment (Current_Environment));
   end Set_Environment_Variable;

                -----------------------------------

   procedure Delete_Environment_Variable
     (Name : in     POSIX.POSIX_String;
      Env  : in out Environment) is
   begin
      Check_Name (Name, "Delete_Environment_Variable");
      if Is_Environment_Variable (Name, Env) then
         declare
            use Interfaces;
            L_Env   : Natural     := Length (Env);
            New_Env : Environment := New_Environment (L_Env);
            I, J    : Natural     := 0;
         begin
            loop
               exit when Env (I) = null;

               Get_Variable :
               declare
                  E : constant String
                    := POSIX_Win32.Chars_Ptr_To_String (Env (I));
                  P : Natural
                    := Strings.Fixed.Index (E, "=");
               begin
                  if E (E'First .. P - 1) = POSIX.To_String (Name) then

                     Free_Variable :
                     declare
                        E : C.Strings.Chars_Ptr
                          := Win32.To_Chars_Ptr (Env (I));
                     begin
                        C.Strings.Free (E);
                     end Free_Variable;

                  else
                     New_Env (J) := Env (I);
                     J := J + 1;
                  end if;
               end Get_Variable;
               I := I + 1;
            end loop;
            Free_Environ_Array (Env);
            Env := New_Env;
         end;
      end if;
   end Delete_Environment_Variable;

   procedure Delete_Environment_Variable
     (Name : in     POSIX.POSIX_String) is
   begin
      Delete_Environment_Variable (Name, Environment (Current_Environment));
   end Delete_Environment_Variable;

                -----------------------------------

   function Length (Env : Environment)
                    return Natural
   is
      I : Natural := 0;
   begin
      loop
         exit when Env (I) = null;
         I := I + 1;
      end loop;
      return I;
   end Length;

   function Length
     return Natural is
   begin
      return Length (Environment (Current_Environment));
   end Length;

                -----------------------------------

   procedure For_Every_Environment_Variable
     (Env : in Environment)
   is
      pragma Suppress (Index_Check);
      I             : Natural := 0;
      Terminate_Now : Boolean;
   begin
      loop
         exit when Env (I) = null;
         declare
            E : constant String := POSIX_Win32.Chars_Ptr_To_String (Env (I));
            P : Natural         := Strings.Fixed.Index (E, "=");
         begin
            Terminate_Now := False;
            Action (Name  => To_POSIX_String (E (E'First .. P - 1)),
                    Value => To_POSIX_String (E (P + 1 .. E'Last)),
                    Quit  => Terminate_Now);
         end;
         exit when Terminate_Now;
         I := I + 1;
      end loop;
   end For_Every_Environment_Variable;

   procedure For_Every_Current_Environment_Variable is
      procedure For_Every_CEV is new For_Every_Environment_Variable (Action);
   begin
      For_Every_CEV (Environment (Current_Environment));
   end For_Every_Current_Environment_Variable;


                -----------------------------------

   --  Process Working Directory

   Retcode : Win32.INT;

   procedure Change_Working_Directory
     (Directory_Name : in POSIX.Pathname)
   is
      L_Directory_Name : constant String
        := POSIX.To_String (Directory_Name) & ASCII.Nul;
   begin
      Retcode := Win32.Direct.Chdir (Win32.Addr (L_Directory_Name));
      POSIX_Win32.Check_Retcode (Retcode, "Change_Working_Directory");
   end Change_Working_Directory;

                -----------------------------------

   function Get_Working_Directory
     return POSIX.Pathname
   is
      use type Win32.PSTR;
      Max_Len : constant := 500;
      Buffer  : String (1 .. Max_Len);
      Pstr    : Win32.PSTR;
   begin
      Pstr := Win32.Direct.Getcwd (Win32.Addr (Buffer), Win32.INT (Max_Len));
      if Pstr = null then
         POSIX_Win32.Check_Retcode (-1, "Get_Working_Directory");
      end if;

      declare
         Pathname : POSIX.Pathname (1 .. Max_Len);
         I        : Positive := Pathname'First;
      begin
         loop
            exit when Buffer (I) = ASCII.Nul;
            Pathname (I) := POSIX_Character (Buffer (I));
            I := I + 1;
         end loop;
         return Pathname (1 .. I-1);
      end;
   end Get_Working_Directory;

end POSIX_Process_Environment;
