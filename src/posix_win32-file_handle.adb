
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with System;

package body POSIX_Win32.File_Handle is

   type Handle_Table_Type is array (POSIX_IO.File_Descriptor)
     of Win32.Winnt.HANDLE;

   protected Lock is
      entry Get;
      entry Release;
   private
      L : Boolean := False;
   end Lock;

   protected body Lock is

      entry Get when L = False is
      begin
         L := True;
      end Get;

      entry Release when L = True is
      begin
         L := False;
      end Release;

   end Lock;

   Handle_Table : Handle_Table_Type := (others => Null_Handle);

   Number_File_Open : POSIX_IO.File_Descriptor := 3;

   ----------
   -- Open --
   ----------

   function Open (H : in Win32.Winnt.HANDLE;
                  F : in POSIX_IO.File_Descriptor := 0)
                  return POSIX_IO.File_Descriptor
   is

      use type POSIX_IO.File_Descriptor;
      use type Win32.Winnt.HANDLE;

      function Get_New return POSIX_IO.File_Descriptor
      is
         use type Win32.Winnt.HANDLE;
      begin
         for I in 3 .. Handle_Table'Last loop
            if Handle_Table (I) = Null_Handle then
               return I;
            end if;
         end loop;
         -- return POSIX_IO.File_Descriptor (-1);
         raise POSIX.POSIX_Error;
      end Get_New;

      Result : POSIX_IO.File_Descriptor;

   begin -- Open
      Lock.Get;
      if Handle_Table (F) = Null_Handle then
         Result := F;
      else
         Result := Get_New;
      end if;
      Handle_Table (Result) := H;
      Number_File_Open := Number_File_Open + 1;
      Lock.Release;
      return Result;
   end Open;


   -----------
   -- Close --
   -----------

   procedure Close (F : in POSIX_IO.File_Descriptor)
   is
      use type POSIX_IO.File_Descriptor;
   begin -- Close
      Lock.Get;
      Handle_Table (F) := Null_Handle;
      Number_File_Open := Number_File_Open - 1;
      Lock.Release;
   end Close;


   ---------
   -- Get --
   ---------

   function Get (F : in POSIX_IO.File_Descriptor)
                 return Win32.Winnt.HANDLE
   is
   begin -- Get
      return Handle_Table (F);
   end Get;

begin

   Handle_Table (0) := Win32.Winbase.GetStdHandle
     (Win32.Winbase.STD_INPUT_HANDLE);

   Handle_Table (1) := Win32.Winbase.GetStdHandle
     (Win32.Winbase.STD_OUTPUT_HANDLE);

   Handle_Table (2) := Win32.Winbase.GetStdHandle
     (Win32.Winbase.STD_ERROR_HANDLE);

end POSIX_Win32.File_Handle;
