
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

package POSIX_Permissions is

   type Permission is
      (Others_Execute, Others_Write, Others_Read,
       Group_Execute, Group_Write, Group_Read,
       Owner_Execute, Owner_Write, Owner_Read,
       Set_Group_ID, Set_User_ID);

   type Permission_Set is array (Permission) of Boolean;

   Owner_Permission_Set : constant Permission_Set
     := Permission_Set'(Owner_Read | Owner_Write | Owner_Execute => true,
                        others => false);

   Group_Permission_Set : constant Permission_Set
     := Permission_Set'(Group_Read | Group_Write | Group_Execute => true,
                        others => false);

   Others_Permission_Set : constant Permission_Set
     := Permission_Set'(Others_Read | Others_Write | Others_Execute => true,
                        others => false);

   Access_Permission_Set : constant Permission_Set
     := Permission_Set'(Owner_Read  | Owner_Write  | Owner_Execute  => true,
                        Group_Read  | Group_Write  | Group_Execute  => true,
                        Others_Read | Others_Write | Others_Execute => true,
                        others => false);

   Set_Group_ID_Set : constant Permission_Set
     := Permission_Set'(Set_Group_ID => true,
                        others => false);

   Set_User_ID_Set : constant Permission_Set
     := Permission_Set'(Set_User_ID => true,
                        others => false);

   --  POSIX Permission-oriented operations

   function Get_Allowed_Process_Permissions
     return Permission_Set;

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set);

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set;
      Old_Perms   :    out Permission_Set);

end POSIX_Permissions;
