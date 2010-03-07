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

with Ada.Strings.Fixed;

with POSIX_Win32;

package body POSIX.Process_Identification is

   Default_UID : constant := 500;
   Default_GID : constant := 100;

   --  Process Identification

   --------------------------
   -- Create_Process_Group --
   --------------------------

   procedure Create_Process_Group
     (Process       :        Process_ID;
      Process_Group :    out Process_Group_ID)
   is
      pragma Warnings (Off, Process);
   begin
      Process_Group := Default_GID;
   end Create_Process_Group;

   --------------------
   -- Create_Session --
   --------------------

   procedure Create_Session (Session_Leader : out Process_Group_ID) is
   begin
      Session_Leader := Default_GID;
   end Create_Session;

   ----------------------------
   -- Get_Effective_Group_ID --
   ----------------------------

   function Get_Effective_Group_ID return Group_ID is
   begin
      return Get_Real_Group_ID;
   end Get_Effective_Group_ID;

   ---------------------------
   -- Get_Effective_User_ID --
   ---------------------------

   function Get_Effective_User_ID return User_ID is
   begin
      return Get_Real_User_ID;
   end Get_Effective_User_ID;

   ----------------
   -- Get_Groups --
   ----------------

   function Get_Groups return Group_List is
   begin
      return Group_List'(1 .. 0 => 0);
   end Get_Groups;

   --------------------
   -- Get_Login_Name --
   --------------------

   function Get_Login_Name return POSIX_String is
      Buffer : String (1 .. 500);
      pragma Warnings (Off, Buffer);

      Size   : aliased Win32.DWORD := 500;
      Result : Win32.BOOL;
   begin
      Result := Win32.Winbase.GetUserName
        (Win32.Addr (Buffer), Size'Unchecked_Access);

      POSIX_Win32.Check_Result (Result, "Get_Login_Name");
      return POSIX.To_POSIX_String (Buffer (1 .. Positive (Size) - 1));
   end Get_Login_Name;

   ---------------------------
   -- Get_Parent_Process_ID --
   ---------------------------

   function Get_Parent_Process_ID return Process_ID is
   begin
      return Null_Process_ID;
   end Get_Parent_Process_ID;

   --------------------------
   -- Get_Process_Group_ID --
   --------------------------

   function Get_Process_Group_ID return Process_Group_ID is
   begin
      return Process_Group_ID (Default_GID);
   end Get_Process_Group_ID;

   --------------------
   -- Get_Process_ID --
   --------------------

   function Get_Process_ID return Process_ID is
   begin
      return Process_ID'
        (System.Null_Address, System.Null_Address,
         Win32.Winbase.GetCurrentProcessId, 0);
   end Get_Process_ID;

   -----------------------
   -- Get_Real_Group_ID --
   -----------------------

   function Get_Real_Group_ID return Group_ID is
   begin
      return Default_GID;
   end Get_Real_Group_ID;

   ----------------------
   -- Get_Real_User_ID --
   ----------------------

   function Get_Real_User_ID return User_ID is
   begin
      return User_ID (Default_UID);
   end Get_Real_User_ID;

   -----------
   -- Image --
   -----------

   function Image (ID : Process_ID) return Standard.String is
      use Ada;
   begin
      return Strings.Fixed.Trim
        (Win32.DWORD'Image (ID.dwProcessId), Strings.Left);
   end Image;

   function Image (ID : Process_Group_ID) return String is
      use Ada;
   begin
      return Strings.Fixed.Trim (Process_Group_ID'Image (ID), Strings.Left);
   end Image;

   function Image (ID : Group_ID) return String is
      use Ada;
   begin
      return Strings.Fixed.Trim (Group_ID'Image (ID), Strings.Left);
   end Image;

   function Image (ID : User_ID) return String is
      use Ada;
   begin
      return Strings.Fixed.Trim (User_ID'Image (ID), Strings.Left);
   end Image;

   ------------------
   -- Set_Group_ID --
   ------------------

   procedure Set_Group_ID (ID : Group_ID) is
      pragma Warnings (Off, ID);
   begin
      null;
   end Set_Group_ID;

   --------------------------
   -- Set_Process_Group_ID --
   --------------------------

   procedure Set_Process_Group_ID
     (Process       : Process_ID       := Get_Process_ID;
      Process_Group : Process_Group_ID := Get_Process_Group_ID)
   is
      pragma Warnings (Off, Process);
      pragma Warnings (Off, Process_Group);
   begin
      null;
   end Set_Process_Group_ID;

   -----------------
   -- Set_User_ID --
   -----------------

   procedure Set_User_ID (ID : User_ID) is
      pragma Warnings (Off, ID);
   begin
      null;
   end Set_User_ID;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return Group_ID is
   begin
      return Group_ID'Value (Str);
   end Value;

   function Value (Str : String) return Process_Group_ID is
   begin
      return Process_Group_ID'Value (Str);
   end Value;

   function Value (Str : Standard.String) return Process_ID is
   begin
      return Process_ID'
        (System.Null_Address, System.Null_Address, Win32.DWORD'Value (Str), 0);
   end Value;

   function Value (Str : String) return User_ID is
   begin
      return User_ID'Value (Str);
   end Value;

end POSIX.Process_Identification;
