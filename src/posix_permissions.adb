
--  $Id$

with System;
--with POSIX_Win32;

package body POSIX_Permissions is

   function Get_Allowed_Process_Permissions
     return Permission_Set
   is
      PS : Permission_Set;
   begin
      return PS;
   end Get_Allowed_Process_Permissions;

                -----------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set)
   is
   begin
      null;
   end Set_Allowed_Process_Permissions;

                -----------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set;
      Old_Perms   :    out Permission_Set)
   is
   begin
      null;
   end Set_Allowed_Process_Permissions;

end POSIX_Permissions;
