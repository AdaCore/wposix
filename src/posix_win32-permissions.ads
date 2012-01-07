------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2012-2012, AdaCore                     --
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

with POSIX.Permissions;

package POSIX_Win32.Permissions is

   --  Notes about POSIX permissions mapping on Win32.
   --
   --  POSIX defines Read/Write/Executer permissions for user the group and
   --  others. Win32 ACL is capable to defined far more subtle permissions.
   --  Yet, for the implementation it is needed to map the POSIX permissions to
   --  the Win32 ACL by using some convention. It must be noted that the work
   --  here has been largely inspired by the Cygwin model.
   --
   --  USER:
   --     Permissions for the user are directly mapped to the Win32
   --     permissions granted to the owner of the file.
   --
   --  GROUP:
   --     Permissions for the group are directly mapped to the Win32
   --     permissions granted to the primary group of the file.
   --
   --  OTHERS:
   --     Permissions for the others are directly mapped to the Win32
   --     permissions granted to the Everyone group of the file.
   --
   --  The permissions are used from the Win32 access mask. The ACL can be
   --  inherited, this implementation does not handle the inheritance at all.
   --  The permissions are read only from the explicit access set on a file.
   --
   --  READ:
   --     Uses the FILE_READ_DATA bit on the access mask.
   --
   --  WRITE:
   --     Uses the FILE_WRITE_DATA bit on the access mask.
   --
   --  EXECUTE:
   --     Uses the FILE_EXECUTE bit on the access mask.
   --

   package PP renames POSIX.Permissions;
   package Winnt renames Win32.Winnt;

   type UGO is (U, G, O); -- User, Group, Others
   type RWX is (R, W, X); -- Read, Write, Execute

   type PM is record
      Perm : PP.Permission;
      Mask : Win32.DWORD;
   end record;

   Masks_W2P : constant array (UGO, RWX) of PM :=
             (U => (R => (PP.Owner_Read, Winnt.FILE_READ_DATA),
                    W => (PP.Owner_Write, Winnt.FILE_WRITE_DATA),
                    X => (PP.Owner_Execute, Winnt.FILE_EXECUTE)),
              G => (R => (PP.Group_Read, Winnt.FILE_READ_DATA),
                    W => (PP.Group_Write, Winnt.FILE_WRITE_DATA),
                    X => (PP.Group_Execute, Winnt.FILE_EXECUTE)),
              O => (R => (PP.Others_Read, Winnt.FILE_READ_DATA),
                    W => (PP.Others_Write, Winnt.FILE_WRITE_DATA),
                    X => (PP.Others_Execute, Winnt.FILE_EXECUTE)));

   type GM is record
      Group : UGO;
      Mask  : Win32.DWORD;
   end record;

   Masks_P2W : constant array (PP.Permission) of GM :=
             (PP.Owner_Read     => (U, Winnt.FILE_READ_DATA),
              PP.Owner_Write    => (U, Winnt.FILE_WRITE_DATA),
              PP.Owner_Execute  => (U, Winnt.FILE_EXECUTE),
              PP.Group_Read     => (G, Winnt.FILE_READ_DATA),
              PP.Group_Write    => (G, Winnt.FILE_WRITE_DATA),
              PP.Group_Execute  => (G, Winnt.FILE_EXECUTE),
              PP.Others_Read    => (O, Winnt.FILE_READ_DATA),
              PP.Others_Write   => (O, Winnt.FILE_WRITE_DATA),
              PP.Others_Execute => (O, Winnt.FILE_EXECUTE),
              PP.Set_User_ID    => (U, 0),
              PP.Set_Group_ID   => (U, 0));

end POSIX_Win32.Permissions;
