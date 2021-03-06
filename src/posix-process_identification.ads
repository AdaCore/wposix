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

with System;

private with Ada.Strings.Bounded;
private with Win32.Winbase;

package POSIX.Process_Identification is

   --  Process Identification

   type Process_ID is private;

   Null_Process_ID   : constant Process_ID;
   System_Process_ID : constant Process_ID;

   function Get_Process_ID return Process_ID;

   function Get_Parent_Process_ID return Process_ID;

   function Image (ID : Process_ID) return String;

   function Value (Str : String) return Process_ID;

   --  Process Group Identification

   type Process_Group_ID is private;

   function Get_Process_Group_ID return Process_Group_ID;

   procedure Set_Process_Group_ID
     (Process       : Process_ID := Get_Process_ID;
      Process_Group : Process_Group_ID := Get_Process_Group_ID);

   procedure Create_Process_Group
     (Process       :        Process_ID;
      Process_Group :    out Process_Group_ID);

   procedure Create_Session (Session_Leader : out Process_Group_ID);

   function Image (ID : Process_Group_ID) return Standard.String;

   function Value (Str : String) return Process_Group_ID;

   --  User Identification

   type User_ID is private;

   function Get_Real_User_ID return User_ID;

   function Get_Effective_User_ID return User_ID;

   procedure Set_User_ID (ID : User_ID);

   function Get_Login_Name return POSIX_String;

   function Image (ID : User_ID) return String;

   function Value (Str : String) return User_ID;

   --  User Group Identification

   type Group_ID is private;

   function Get_Real_Group_ID return Group_ID;

   function Get_Effective_Group_ID return Group_ID;

   procedure Set_Group_ID (ID : Group_ID);

   subtype Group_List_Index is Positive range 1 .. POSIX.Groups_Maxima'Last;

   type Group_List is array (Group_List_Index range <>) of Group_ID;

   function Get_Groups return Group_List;

   function Image (ID : Group_ID) return String;

   function Value (Str : String) return Group_ID;

private

   type Process_ID is new Win32.Winbase.PROCESS_INFORMATION;

   Null_Process_ID   : constant Process_ID :=
                         (System.Null_Address, System.Null_Address, 0, 0);

   System_Process_ID : constant Process_ID := Null_Process_ID;

   package Sid_Object is
     new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 60);

   type Process_Group_ID is new Sid_Object.Bounded_String;
   type User_ID is new Sid_Object.Bounded_String;
   type Group_ID is new Sid_Object.Bounded_String;

end POSIX.Process_Identification;
