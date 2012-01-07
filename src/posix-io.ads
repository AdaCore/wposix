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
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   function Open_Or_Create
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Permissions    : POSIX.Permissions.Permission_Set;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   function Is_Open (File : File_Descriptor) return Boolean;

   procedure Close
     (File           : File_Descriptor;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);

   function Duplicate
     (File   : File_Descriptor;
      Target : File_Descriptor := 0)
      return File_Descriptor;

   function Duplicate_And_Close
     (File           : File_Descriptor;
      Target         : File_Descriptor := 0;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor;

   procedure Create_Pipe
     (Read_End  : out File_Descriptor;
      Write_End : out File_Descriptor);

   --  File Input/Output operations

   subtype IO_Buffer is POSIX.POSIX_String;

   procedure Read
     (File           :        File_Descriptor;
      Buffer         :    out IO_Buffer;
      Last           :    out POSIX.IO_Count;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals);

   procedure Write
     (File           :        File_Descriptor;
      Buffer         :        IO_Buffer;
      Last           :    out POSIX.IO_Count;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals);

   generic
      type T is private;
   procedure Generic_Read
     (File           :        File_Descriptor;
      Item           :    out T;
      Masked_Signals :        POSIX.Signal_Masking := POSIX.RTS_Signals);

   generic
      type T is private;
   procedure Generic_Write
     (File           : File_Descriptor;
      Item           : T;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals);

   --  File position operations

   type Position is (From_Beginning, From_Current_Position, From_End_Of_File);

   procedure Seek
     (File           :        File_Descriptor;
      Offset         :        IO_Offset;
      Result         :    out IO_Offset;
      Starting_Point :        Position := From_Beginning);

   function File_Size (File : File_Descriptor) return POSIX.IO_Count;

   function File_Position (File : File_Descriptor) return IO_Offset;

   --  Terminal operations

   function Is_A_Terminal (File : File_Descriptor) return Boolean;

   function Get_Terminal_Name
     (File : File_Descriptor) return POSIX.Pathname;

   --  File Control operations

   procedure Get_File_Control
     (File    :        File_Descriptor;
      Mode    :    out File_Mode;
      Options :    out Open_Option_Set);

   procedure Set_File_Control
     (File    : File_Descriptor;
      Options : Open_Option_Set);

   function Get_Close_On_Exec (File : File_Descriptor) return Boolean;

   procedure Set_Close_On_Exec
     (File : File_Descriptor;
      To   : Boolean := True);

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
