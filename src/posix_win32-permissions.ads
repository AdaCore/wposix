------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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

end POSIX_Win32.Permissions;
