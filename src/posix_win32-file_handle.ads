
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Win32.Winnt;
with POSIX_IO;

package POSIX_Win32.File_Handle is

   function Open (H : in Win32.Winnt.HANDLE;
                  F : in POSIX_IO.File_Descriptor := 0)
     return POSIX_IO.File_Descriptor;

   procedure Close (F : in POSIX_IO.File_Descriptor);

   function Get (F : in POSIX_IO.File_Descriptor)
     return Win32.Winnt.HANDLE;

end POSIX_Win32.File_Handle;
