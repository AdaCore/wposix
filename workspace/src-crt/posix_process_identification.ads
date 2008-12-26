
--  $Id$

with System;
with POSIX;
with Win32.Winbase;

package POSIX_Process_Identification is

   --  Process Identification

   type Process_ID is private;

   Null_Process_ID   : constant Process_ID;
   System_Process_ID : constant Process_ID;

   function Get_Process_ID
     return Process_ID;

   function Get_Parent_Process_ID
     return Process_ID;

   function Image (ID : Process_ID)
                   return Standard.String;

   function Value (Str : Standard.String)
                   return Process_ID;



   --  Process Group Identification

   type Process_Group_ID is private;

   function Get_Process_Group_ID
     return Process_Group_ID;

   procedure Set_Process_Group_ID
     (Process       : in Process_ID := Get_Process_ID;
      Process_Group : in Process_Group_ID := Get_Process_Group_ID);

   procedure Create_Process_Group
     (Process       : in     Process_ID;
      Process_Group :    out Process_Group_ID);

   procedure Create_Session (Session_Leader : out Process_Group_ID);

   function Image (ID : Process_Group_ID)
                   return Standard.String;

   function Value (Str : Standard.String)
                   return Process_Group_ID;



   --  User Identification

   type User_ID is private;

   function Get_Real_User_ID
     return User_ID;

   function Get_Effective_User_ID
     return User_ID;

   procedure Set_User_ID (ID : in User_ID);

   function Get_Login_Name
     return POSIX.POSIX_String;

   function Image (ID : User_ID)
                   return Standard.String;

   function Value (Str : Standard.String)
                   return User_ID;



   --  User Group Identification

   type Group_ID is private;

   function Get_Real_Group_ID
     return Group_ID;

   function Get_Effective_Group_ID
     return Group_ID;

   procedure Set_Group_ID (ID : in Group_ID);

   subtype Group_List_Index is Positive range 1 .. POSIX.Groups_Maxima'Last;

   type Group_List is array (Group_List_Index range <>) of Group_ID;

   function Get_Groups
     return Group_List;

   function Image (ID : Group_ID)
                   return Standard.String;

   function Value (Str : Standard.String)
                   return Group_ID;

private

   type Process_ID is new Win32.Winbase.PROCESS_INFORMATION;
   Null_Process_ID   : constant Process_ID := (System.Null_Address,
                                               System.Null_Address,
                                               0, 0);
   System_Process_ID : constant Process_ID := (System.Null_Address,
                                               System.Null_Address,
                                               0, 0);

   type Process_Group_ID is new Integer;
   type User_ID is new Integer;
   type Group_ID is new Integer;

end POSIX_Process_Identification;



