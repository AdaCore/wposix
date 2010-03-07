------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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

with Win32.Winbase;

package body POSIX_Win32.File_Handle is

   type Handle_Table_Type is
     array (POSIX.IO.File_Descriptor) of Win32.Winnt.HANDLE;

   protected Lock is
      entry Get;
      procedure Release;
   private
      L : Boolean := False;
   end Lock;

   protected body Lock is

      ---------
      -- Get --
      ---------

      entry Get when L = False is
      begin
         L := True;
      end Get;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         L := False;
      end Release;

   end Lock;

   Handle_Table : Handle_Table_Type := (others => Null_Handle);

   Number_File_Open : POSIX.IO.File_Descriptor := 3;

   -----------
   -- Close --
   -----------

   procedure Close (F : POSIX.IO.File_Descriptor) is
      use type POSIX.IO.File_Descriptor;
   begin -- Close
      Lock.Get;
      Handle_Table (F) := Null_Handle;
      Number_File_Open := Number_File_Open - 1;
      Lock.Release;
   end Close;

   ---------
   -- Get --
   ---------

   function Get (F : POSIX.IO.File_Descriptor) return Win32.Winnt.HANDLE is
   begin -- Get
      return Handle_Table (F);
   end Get;

   ----------
   -- Open --
   ----------

   function Open
     (H : Win32.Winnt.HANDLE;
      F : POSIX.IO.File_Descriptor := 0) return POSIX.IO.File_Descriptor
   is

      use type POSIX.IO.File_Descriptor;
      use type Win32.Winnt.HANDLE;

      function Get_New return POSIX.IO.File_Descriptor;
      --  ???

      -------------
      -- Get_New --
      -------------

      function Get_New return POSIX.IO.File_Descriptor is
      begin
         for I in 3 .. Handle_Table'Last loop
            if Handle_Table (I) = Null_Handle then
               return I;
            end if;
         end loop;
         raise POSIX.POSIX_Error;
      end Get_New;

      Result : POSIX.IO.File_Descriptor;

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
   exception
      when others =>
         Lock.Release;
         raise;
   end Open;

begin
   Handle_Table (0) := Win32.Winbase.GetStdHandle
     (Win32.Winbase.STD_INPUT_HANDLE);

   Handle_Table (1) := Win32.Winbase.GetStdHandle
     (Win32.Winbase.STD_OUTPUT_HANDLE);

   Handle_Table (2) := Win32.Winbase.GetStdHandle
     (Win32.Winbase.STD_ERROR_HANDLE);
end POSIX_Win32.File_Handle;
