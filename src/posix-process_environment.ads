------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2011, AdaCore                     --
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

private with Ada.Containers.Indefinite_Hashed_Maps;

package POSIX.Process_Environment is

   --  Process Parameters

   function Argument_List return POSIX.POSIX_String_List;

   --  Environment Variables

   type Environment is limited private;

   procedure Copy_From_Current_Environment (Env : in out Environment);

   procedure Copy_To_Current_Environment (Env : Environment);

   procedure Copy_Environment
     (Source :        Environment;
      Target : in out Environment);

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Env       : Environment;
      Undefined : POSIX.POSIX_String := "") return POSIX.POSIX_String;

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Undefined : POSIX.POSIX_String := "") return POSIX.POSIX_String;

   function Is_Environment_Variable
     (Name : POSIX.POSIX_String;
      Env  : Environment) return Boolean;

   function Is_Environment_Variable
     (Name : POSIX.POSIX_String) return Boolean;

   procedure Clear_Environment (Env : in out Environment);

   procedure Clear_Environment;

   procedure Set_Environment_Variable
     (Name  :        POSIX.POSIX_String;
      Value :        POSIX.POSIX_String;
      Env   : in out Environment);

   procedure Set_Environment_Variable
     (Name  : POSIX.POSIX_String;
      Value : POSIX.POSIX_String);

   procedure Delete_Environment_Variable
     (Name :        POSIX.POSIX_String;
      Env  : in out Environment);

   procedure Delete_Environment_Variable (Name : POSIX.POSIX_String);

   function Length (Env : Environment) return Natural;

   function Length return Natural;

   generic
      with procedure Action
        (Name  :        POSIX.POSIX_String;
         Value :        POSIX.POSIX_String;
         Quit  : in out Boolean);
   procedure For_Every_Environment_Variable (Env : Environment);

   generic
      with procedure Action
        (Name  :        POSIX.POSIX_String;
         Value :        POSIX.POSIX_String;
         Quit  : in out Boolean);
   procedure For_Every_Current_Environment_Variable;

   --  Process Working Directory

   procedure Change_Working_Directory (Directory_Name : POSIX.Pathname);

   function Get_Working_Directory return POSIX.Pathname;

private

   use Ada;

   function POSIX_String_Hash
     (Str : POSIX.POSIX_String) return Containers.Hash_Type;

   package Strings_Map is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => POSIX.POSIX_String,
      Element_Type    => POSIX.POSIX_String,
      Hash            => POSIX_String_Hash,
      Equivalent_Keys => "=");

   type Environment is new Strings_Map.Map with null record;

end POSIX.Process_Environment;
