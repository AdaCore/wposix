
--  $Id$
--  Author : Pascal Obry
--  p.obry@wanadoo.fr

with System;
with Ada.Strings.Fixed;

with POSIX_Win32;

with Win32.Winbase;

package body POSIX.Process_Identification is


   Default_UID : constant := 500;
   Default_GID : constant := 100;

   --  Process Identification

   --------------------
   -- Get_Process_ID --
   --------------------

   function Get_Process_ID
     return Process_ID is
   begin
      return Process_ID'(System.Null_Address,
                         System.Null_Address,
                         Win32.Winbase.GetCurrentProcessId,
                         0);
   end Get_Process_ID;


   ---------------------------
   -- Get_Parent_Process_ID --
   ---------------------------

   function Get_Parent_Process_ID
     return Process_ID is
   begin
      return Null_Process_ID;
   end Get_Parent_Process_ID;


   -----------
   -- Image --
   -----------

   function Image (ID : Process_ID)
                   return Standard.String
   is
      use Ada;
   begin
      return Strings.Fixed.Trim (Win32.DWORD'Image (ID.dwProcessId),
                                 Strings.Left);
   end Image;


   -----------
   -- Value --
   -----------

   function Value (Str : Standard.String)
                   return Process_ID is
   begin
      return Process_ID'(System.Null_Address, System.Null_Address,
                         Win32.DWORD'Value (Str), 0);
   end Value;



   --  Process Group Identification

   --------------------------
   -- Get_Process_Group_ID --
   --------------------------

   function Get_Process_Group_ID
     return Process_Group_ID is
   begin
      return Process_Group_ID (Default_GID);
   end Get_Process_Group_ID;


   --------------------------
   -- Set_Process_Group_ID --
   --------------------------

   procedure Set_Process_Group_ID
     (Process       : in Process_ID       := Get_Process_ID;
      Process_Group : in Process_Group_ID := Get_Process_Group_ID) is
   begin
      null;
   end Set_Process_Group_ID;


   --------------------------
   -- Create_Process_Group --
   --------------------------

   procedure Create_Process_Group
     (Process       : in     Process_ID;
      Process_Group :    out Process_Group_ID) is
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


   -----------
   -- Image --
   -----------

   function Image (ID : Process_Group_ID)
                   return Standard.String
   is
      use Ada;
   begin
      return Strings.Fixed.Trim (Process_Group_ID'Image (ID),
                                 Strings.Left);
   end Image;


   -----------
   -- Value --
   -----------

   function Value (Str : Standard.String)
                   return Process_Group_ID is
   begin
      return Process_Group_ID'Value (Str);
   end Value;



   --  User Identification

   function Get_Real_User_ID
     return User_ID is
   begin
      return User_ID (Default_UID);
   end Get_Real_User_ID;


   ---------------------------
   -- Get_Effective_User_ID --
   ---------------------------

   function Get_Effective_User_ID
     return User_ID is
   begin
      return Get_Real_User_ID;
   end Get_Effective_User_ID;


   -----------------
   -- Set_User_ID --
   -----------------

   procedure Set_User_ID (ID : in User_ID) is
   begin
      null;
   end Set_User_ID;


   --------------------
   -- Get_Login_Name --
   --------------------

   function Get_Login_Name
     return POSIX.POSIX_String
   is
      Buffer : String (1 .. 500);
      pragma Warnings (Off, Buffer);
      Size   : aliased Win32.DWORD := 500;
      Result : Win32.BOOL;
   begin
      Result := Win32.Winbase.GetUserName (Win32.Addr (Buffer),
                                           Size'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Get_Login_Name");
      return POSIX.To_POSIX_String (Buffer (1 .. Positive (Size) - 1));
   end Get_Login_Name;


   -----------
   -- Image --
   -----------

   function Image (ID : User_ID)
                   return Standard.String
   is
      use Ada;
   begin
      return Strings.Fixed.Trim (User_ID'Image (ID),
                                 Strings.Left);
   end Image;


   -----------
   -- Value --
   -----------

   function Value (Str : Standard.String)
                   return User_ID is
   begin
      return User_ID'Value (Str);
   end Value;



   --  User Group Identification

   function Get_Real_Group_ID
     return Group_ID is
   begin
      return Default_GID;
   end Get_Real_Group_ID;


   ----------------------------
   -- Get_Effective_Group_ID --
   ----------------------------

   function Get_Effective_Group_ID
     return Group_ID is
   begin
      return Get_Real_Group_ID;
   end Get_Effective_Group_ID;


   ------------------
   -- Set_Group_ID --
   ------------------

   procedure Set_Group_ID (ID : in Group_ID) is
   begin
      null;
   end Set_Group_ID;


   ----------------
   -- Get_Groups --
   ----------------

   function Get_Groups
     return Group_List is
   begin
      return Group_List'(1 .. 0 => 0);
   end Get_Groups;


   -----------
   -- Image --
   -----------

   function Image (ID : Group_ID)
                   return Standard.String
   is
      use Ada;
   begin
      return Strings.Fixed.Trim (Group_ID'Image (ID),
                                 Strings.Left);
   end Image;


   -----------
   -- Value --
   -----------

   function Value (Str : Standard.String)
                   return Group_ID is
   begin
      return Group_ID'Value (Str);
   end Value;

end POSIX.Process_Identification;
