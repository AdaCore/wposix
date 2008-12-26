
--  $Id$


with System;

with POSIX_Win32;
with POSIX;
with Win32.Crt.Process;
with Win32.Winbase;

package body POSIX_Process_Identification is


   Default_UID : constant := 500;
   Default_GID : constant := 100;

   Result : Win32.BOOL;

   --  Process Identification

   function Get_Process_ID
     return Process_ID is
   begin
      return Process_ID'(System.Null_Address, System.Null_Address,
                         Win32.DWORD (Win32.Crt.Process.Getpid), 0);
   end Get_Process_ID;

                -----------------------------------

   function Get_Parent_Process_ID
     return Process_ID is
   begin
      return Null_Process_ID;
   end Get_Parent_Process_ID;

                -----------------------------------

   function Image (ID : Process_ID)
                   return Standard.String is
   begin
      return Win32.DWORD'Image (ID.DwProcessId);
   end Image;

                -----------------------------------

   function Value (Str : Standard.String)
                   return Process_ID is
   begin
      return Process_ID'(System.Null_Address, System.Null_Address,
                         Win32.DWORD'Value (Str), 0);
   end Value;



   --  Process Group Identification

   function Get_Process_Group_ID
     return Process_Group_ID is
   begin
      return Process_Group_ID (Default_GID);
   end Get_Process_Group_ID;

                -----------------------------------

   procedure Set_Process_Group_ID
     (Process       : in Process_ID       := Get_Process_ID;
      Process_Group : in Process_Group_ID := Get_Process_Group_ID) is
   begin
      null;
   end Set_Process_Group_ID;

                -----------------------------------

   procedure Create_Process_Group
     (Process       : in     Process_ID;
      Process_Group :    out Process_Group_ID) is
   begin
      null;
   end Create_Process_Group;

                -----------------------------------

   procedure Create_Session (Session_Leader : out Process_Group_ID) is
   begin
      null;
   end Create_Session;

                -----------------------------------

   function Image (ID : Process_Group_ID)
                   return Standard.String is
   begin
      return Process_Group_ID'Image (ID);
   end Image;

                -----------------------------------

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

                -----------------------------------

   function Get_Effective_User_ID
     return User_ID is
   begin
      return Get_Real_User_ID;
   end Get_Effective_User_ID;

                -----------------------------------

   procedure Set_User_ID (ID : in User_ID) is
   begin
      null;
   end Set_User_ID;

                -----------------------------------

   function Get_Login_Name
     return POSIX.POSIX_String
   is
      Buffer : String (1 .. 500);
      Size   : aliased Win32.DWORD := 500;
   begin
      Result := Win32.Winbase.GetUserName (Win32.Addr (Buffer),
                                           Size'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Get_Login_Name");
      return POSIX.To_POSIX_String (Buffer (1 .. Positive (Size)));
   end Get_Login_Name;

                -----------------------------------

   function Image (ID : User_ID)
                   return Standard.String is
   begin
      return User_ID'Image (ID);
   end Image;

                -----------------------------------

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

                -----------------------------------

   function Get_Effective_Group_ID
     return Group_ID is
   begin
      return Get_Real_Group_ID;
   end Get_Effective_Group_ID;

                -----------------------------------

   procedure Set_Group_ID (ID : in Group_ID) is
   begin
      null;
   end Set_Group_ID;

                -----------------------------------

   function Get_Groups
     return Group_List is
   begin
      return Group_List'(1 .. 0 => 0);
   end Get_Groups;

                -----------------------------------

   function Image (ID : Group_ID)
                   return Standard.String is
   begin
      return Group_ID'Image (ID);
   end Image;

                -----------------------------------

   function Value (Str : Standard.String)
                   return Group_ID is
   begin
      return Group_ID'Value (Str);
   end Value;

end POSIX_Process_Identification;
