
--  $Id$
--  Author : Pascal Obry
--  p.obry@wanadoo.fr

package body POSIX.Permissions is

   Dummy_Process_Permission_Set : Permission_Set := Owner_Permission_Set;

   -------------------------------------
   -- Get_Allowed_Process_Permissions --
   -------------------------------------

   function Get_Allowed_Process_Permissions
     return Permission_Set is
   begin
      return Dummy_Process_Permission_Set;
   end Get_Allowed_Process_Permissions;


   -------------------------------------
   -- Set_Allowed_Process_Permissions --
   -------------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set)
   is
   begin
      Dummy_Process_Permission_Set := Permissions;
   end Set_Allowed_Process_Permissions;


   -------------------------------------
   -- Set_Allowed_Process_Permissions --
   -------------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set;
      Old_Perms   :    out Permission_Set)
   is
   begin
      Old_Perms := Dummy_Process_Permission_Set;
      Dummy_Process_Permission_Set := Permissions;
   end Set_Allowed_Process_Permissions;

end POSIX.Permissions;
