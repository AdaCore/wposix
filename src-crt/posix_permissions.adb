
--  $Id$

with System;
with POSIX_Win32;
with Win32.Crt.IO;
with Win32.Crt.Stat;

package body POSIX_Permissions is

   function Get_Allowed_Process_Permissions
     return Permission_Set
   is
      Mask : Win32.UINT;
   begin
      Mask := Win32.UINT (Win32.Crt.IO.Umask (0));
      return POSIX_Win32.To_Set (Mask);
   end Get_Allowed_Process_Permissions;

                -----------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set)
   is
      Old_Mask : Win32.INT;
      Mask     : Win32.UINT;
   begin
      Mask := POSIX_Win32.To_Mask (Permissions);
      Old_Mask := Win32.Crt.IO.Umask (Win32.INT (Mask));
   end Set_Allowed_Process_Permissions;

                -----------------------------------

   procedure Set_Allowed_Process_Permissions
     (Permissions : in     Permission_Set;
      Old_Perms   :    out Permission_Set)
   is
      Old_Mask : Win32.UINT;
      Mask     : Win32.UINT;
   begin
      Mask := POSIX_Win32.To_Mask (Permissions);
      Old_Mask := Win32.UINT (Win32.Crt.IO.Umask (Win32.INT (Mask)));
      Old_Perms := POSIX_Win32.To_Set (Old_Mask);
   end Set_Allowed_Process_Permissions;

end POSIX_Permissions;
