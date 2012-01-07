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

package POSIX.Permissions is

   type Permission is
      (Others_Execute, Others_Write, Others_Read,
       Group_Execute, Group_Write, Group_Read,
       Owner_Execute, Owner_Write, Owner_Read,
       Set_Group_ID, Set_User_ID);

   type Permission_Set is array (Permission) of Boolean;

   Owner_Permission_Set : constant Permission_Set := Permission_Set'
     (Owner_Read | Owner_Write | Owner_Execute => True,
      others                                   => False);

   Group_Permission_Set : constant Permission_Set := Permission_Set'
     (Group_Read | Group_Write | Group_Execute => True,
      others                                   => False);

   Others_Permission_Set : constant Permission_Set := Permission_Set'
     (Others_Read | Others_Write | Others_Execute => True,
      others                                      => False);

   Access_Permission_Set : constant Permission_Set := Permission_Set'
     (Owner_Read  | Owner_Write  | Owner_Execute  => True,
      Group_Read  | Group_Write  | Group_Execute  => True,
      Others_Read | Others_Write | Others_Execute => True,
      others                                      => False);

   Set_Group_ID_Set : constant Permission_Set := Permission_Set'
     (Set_Group_ID => True,
      others       => False);

   Set_User_ID_Set : constant Permission_Set := Permission_Set'
     (Set_User_ID => True,
      others      => False);

   --  POSIX Permission-oriented operations

   function Get_Allowed_Process_Permissions return Permission_Set;

   procedure Set_Allowed_Process_Permissions
     (Permissions : Permission_Set);

   procedure Set_Allowed_Process_Permissions
     (Permissions :        Permission_Set;
      Old_Perms   :    out Permission_Set);

end POSIX.Permissions;
