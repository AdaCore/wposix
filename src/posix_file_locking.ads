
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with POSIX;
with POSIX_IO;
with POSIX_Process_Identification;

package POSIX_File_Locking is

   type Lock_Kind is (Read_Lock, Write_Lock, Unlock);

   type File_Lock (Whole_File: Boolean := True) is
      record
         Lock : Lock_Kind;
         case Whole_File is
            when True => null;
            when False =>
               Starting_Point : POSIX_IO.Position;
               Start          : POSIX_IO.IO_Offset;
               Length         : POSIX.IO_Count;
         end case;
      end record;

   procedure Get_Lock
     (File    : in     POSIX_IO.File_descriptor;
      Lock    : in     File_Lock;
      Result  :    out File_Lock;
      Process :    out POSIX_Process_Identification.Process_ID);

   procedure Set_Lock
     (File : in     POSIX_IO.File_Descriptor;
      Lock : in     File_Lock);

   procedure Wait_To_Set_Lock
     (File           : in     POSIX_IO.File_Descriptor;
      Lock           : in     File_Lock;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals);

end POSIX_File_Locking;
