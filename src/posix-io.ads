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

with Ada.Unchecked_Conversion;

with POSIX.Permissions;

private with Win32;

package POSIX.IO is

   --  Common type declarations

   type File_Descriptor is range 0 .. POSIX.Open_Files_Maxima'Last - 1;

   Standard_Input  : constant File_Descriptor := 0;
   Standard_Output : constant File_Descriptor := 1;
   Standard_Error  : constant File_Descriptor := 2;

   type IO_Offset is new Long_Integer;

   --  Files Modes and Options

   type File_Mode is (Read_Only, Write_Only, Read_Write);

   type Open_Option_Set is new POSIX.Option_Set;
   --  Empty_Set, "+" and "-" are derived operations

   Non_Blocking               : constant Open_Option_Set; --  not implemented

   Append                     : constant Open_Option_Set;

   Truncate                   : constant Open_Option_Set;

   Exclusive                  : constant Open_Option_Set;

   Not_Controlling_Terminal   : constant Open_Option_Set; --  not implemented

   --  Operations to open or close file descriptors

   function Open
     (Name           : in POSIX.Pathname;
      Mode           : in File_Mode;
      Options        : in Open_Option_Set := Empty_Set;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   function Open_Or_Create
     (Name           : in POSIX.Pathname;
      Mode           : in File_Mode;
      Permissions    : in POSIX.Permissions.Permission_Set;
      Options        : in Open_Option_Set := Empty_Set;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   function Is_Open (File : in File_Descriptor) return Boolean;

   procedure Close
     (File           : in File_Descriptor;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals);

   function Duplicate
     (File   : in File_Descriptor;
      Target : in File_Descriptor := 0)
      return File_Descriptor;

   function Duplicate_And_Close
     (File           : in File_Descriptor;
      Target         : in File_Descriptor := 0;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   procedure Create_Pipe
     (Read_End  : out File_Descriptor;
      Write_End : out File_Descriptor);

   --  File Input/Output operations

   subtype IO_Buffer is POSIX.POSIX_String;

   procedure Read
     (File           : in     File_Descriptor;
      Buffer         :    out IO_Buffer;
      Last           :    out POSIX.IO_Count;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals);

   procedure Write
     (File           : in     File_Descriptor;
      Buffer         : in     IO_Buffer;
      Last           :    out POSIX.IO_Count;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals);

   generic
      type T is private;
   procedure Generic_Read
     (File           : in     File_Descriptor;
      Item           :    out T;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals);

   generic
      type T is private;
   procedure Generic_Write
     (File           : in     File_Descriptor;
      Item           : in     T;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals);

   --  File position operations

   type Position is (From_Beginning, From_Current_Position, From_End_Of_File);

   procedure Seek
     (File           : in     File_Descriptor;
      Offset         : in     IO_Offset;
      Result         :    out IO_Offset;
      Starting_Point : in     Position := From_Beginning);

   function File_Size (File : in File_Descriptor) return POSIX.IO_Count;

   function File_Position (File : in File_Descriptor) return IO_Offset;

   --  Terminal operations

   function Is_A_Terminal (File : in File_Descriptor) return Boolean;

   function Get_Terminal_Name
     (File : in File_Descriptor) return POSIX.Pathname;

   --  File Control operations

   procedure Get_File_Control
     (File    : in     File_Descriptor;
      Mode    :    out File_Mode;
      Options :    out Open_Option_Set);

   procedure Set_File_Control
     (File    : in     File_Descriptor;
      Options : in     Open_Option_Set);

   function Get_Close_On_Exec (File : in File_Descriptor) return Boolean;

   procedure Set_Close_On_Exec
     (File : in     File_Descriptor;
      To   : in     Boolean := True);

private

   function UINT_To_Open_Option_Set is new Ada.Unchecked_Conversion
     (Win32.UINT, Open_Option_Set);

   Non_Blocking             : constant Open_Option_Set :=
                                UINT_To_Open_Option_Set (1);
   Append                   : constant Open_Option_Set :=
                                UINT_To_Open_Option_Set (2);
   Truncate                 : constant Open_Option_Set :=
                                UINT_To_Open_Option_Set (4);
   Exclusive                : constant Open_Option_Set :=
                                UINT_To_Open_Option_Set (8);
   Not_Controlling_Terminal : constant Open_Option_Set :=
                                UINT_To_Open_Option_Set (16);

end POSIX.IO;
