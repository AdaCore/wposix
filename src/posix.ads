------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

pragma Ada_2012;

with Win32;
with Win32.Winerror;

package POSIX is

   --  Symbolic subtypes and constants

   --  Optional Facilities

   subtype Job_Control_Support is Boolean
     range False .. True;

   subtype Saved_IDs_Support is Boolean
     range False .. True;

   subtype Change_Owner_Restriction is Boolean
     range False .. True;

   subtype Filename_Truncation is Boolean
     range False .. True;

   System_POSIX_Version : constant := 1990_09;
   POSIX_Ada_Version    : constant := 1996_07;

   --  I/O Count

   type IO_Count is new Natural range 0 .. Natural'Last;
   subtype IO_count_Maximum is IO_Count range 32767 .. IO_Count'Last;

   --  System Limits

   Portable_Groups_Maximum            : constant Natural := 0;
   subtype Groups_Maxima is Natural range 16 .. 16;

   Portable_Argument_List_Maximum     : constant Natural := 4096;
   subtype Argument_List_Maxima is Natural range 14500 .. 14500;

   Portable_Child_Processes_Maximum   : constant Natural := 6;
   subtype Child_Processes_Maxima is Natural range 6 .. 6;

   Portable_Open_Files_Maximum        : constant Natural := 16;
   subtype Open_Files_Maxima is Natural range 32 .. 32;

   Portable_Stream_Maximum            : constant Natural := 8;
   subtype Stream_Maxima is Natural range 20 .. 20;

   Portable_Time_Zone_String_Maximum  : constant Natural := 3;
   subtype Time_Zone_String_Maxima is Natural range 10 .. 10;

   --  Pathname Variable Values

   Portable_Link_Limit_Maximum        : constant Natural := 8;
   subtype Link_Limit_Maxima is Natural range 8 .. 8;

   Portable_Input_Line_Limit_Maximum  : constant IO_Count := 255;
   subtype Input_Line_Limit_Maxima is IO_Count range 255 .. 255;

   Portable_Input_Queue_Limit_Maximum : constant IO_Count := 255;
   subtype Input_Queue_Limit_Maxima is IO_Count range 255 .. 255;

   Portable_Filename_Limit_Maximum    : constant Natural := 14;
   subtype Filename_Limit_Maxima is Natural range 255 .. 255;

   Portable_Pathname_Limit_Maximum    : constant Natural := 255;
   subtype Pathname_Limit_Maxima is Natural range 512 .. 512;

   Portable_Pipe_Limit_Maximum        : constant IO_Count := 512;
   subtype Pipe_Limit_Maxima is IO_Count range 512 .. 512;

   --  Blocking Behavior Values

   type Blocking_Behavior is (Tasks, Program);

   subtype Text_IO_Blocking_Behavior is Blocking_Behavior
     range Tasks .. Program;

   IO_Blocking_Behavior              : constant Blocking_Behavior  := Tasks;

   File_Lock_Blocking_Behavior       : constant Blocking_Behavior  := Tasks;

   Wait_For_Child_Blocking_Behavior  : constant Blocking_Behavior  := Tasks;

   --  Signal Masking

   type Signal_Masking is (No_Signals, RTS_Signals, All_Signals);

   --  Characters and Strings

   type POSIX_Character is new Standard.Character;

   type POSIX_String is array (Positive range <>) of POSIX_Character;

   function To_POSIX_String (Str : String) return POSIX_String with Inline;

   function To_String (Str : POSIX_String) return String with Inline;

   subtype Filename is POSIX_String;
   subtype Pathname is POSIX_String;

   function Is_Filename (Str : POSIX_String) return Boolean;
   function Is_Pathname (Str : POSIX_String) return Boolean;

   function Is_Portable_Filename (Str : POSIX_String) return Boolean;
   function Is_Portable_Pathname (Str : POSIX_String) return Boolean;

   --  String Lists

   type POSIX_String_List is limited private;
   Empty_String_List : constant POSIX_String_List;

   procedure Make_Empty (List : in out POSIX_String_List);

   procedure Append
     (List : in out POSIX_String_List;
      Str  :        POSIX_String);

   generic
      with procedure Action
        (Item :         POSIX_String;
         Quit : in out Boolean);
   procedure For_Every_Item (List : POSIX_String_List);

   function Length (List : POSIX_String_List) return Natural;

   function Value
     (List  : POSIX_String_List;
      Index : Positive) return POSIX_String;

   --  Option Sets

   type Option_Set is private;

   function Empty_Set return Option_Set;

   function "+" (L, R : Option_Set) return Option_Set;

   function "-" (L, R : Option_Set) return Option_Set;

   --  Exceptions and error codes

   POSIX_Error : exception;

   type Error_Code is new Win32.DWORD;

   function Get_Error_Code return Error_Code;

   procedure Set_Error_Code (Error : Error_Code);

   function Is_POSIX_Error (Error : Error_Code) return Boolean;

   function Image (Error : Error_Code) return String;

   No_Error                            : constant Error_Code
     := Win32.Winerror.NO_ERROR;

   Argument_List_Too_Long              : constant Error_Code
     := 16#F000_0001#;

   Bad_File_Descriptor                 : constant Error_Code
     := Win32.Winerror.ERROR_INVALID_HANDLE;

   Broken_Pipe                         : constant Error_Code
     := Win32.Winerror.ERROR_BROKEN_PIPE;

   Directory_Not_Empty                 : constant Error_Code
     := 16#F000_0002#;

   Exec_Format_Error                   : constant Error_Code
     := 16#F000_0003#;

   File_Exists                         : constant Error_Code
     := Win32.Winerror.ERROR_ALREADY_EXISTS;

   File_Too_Large                      : constant Error_Code
     := 16#F000_0005#;

   Filename_Too_Long                   : constant Error_Code
     := 16#F000_0006#;

   Improper_Link                       : constant Error_Code
     := 16#F000_0007#;

   Inappropriate_IO_Control_Operation  : constant Error_Code
     := 16#F000_0008#;

   Input_Output_Error                  : constant Error_Code
     := 16#F000_0009#;

   Interrupted_Operation               : constant Error_Code
     := 16#F000_000A#;

   Invalid_Argument                    : constant Error_Code
     := 16#F000_000B#;

   Invalid_Seek                        : constant Error_Code
     := Win32.Winerror.ERROR_SEEK_ON_DEVICE;

   Is_A_Directory                      : constant Error_Code
     := 16#F000_000D#;

   No_Child_Process                    : constant Error_Code
     := 16#F000_000E#;

   No_Locks_Available                  : constant Error_Code
     := 16#F000_000F#;

   No_Space_Left_On_Device             : constant Error_Code
     := Win32.Winerror.ERROR_DISK_FULL;

   No_Such_Operation_On_Device         : constant Error_Code
     := 16#F000_0011#;

   No_Such_Device_Or_Address           : constant Error_Code
     := 16#F000_0012#;

   No_Such_File_Or_Directory           : constant Error_Code
     := Win32.Winerror.ERROR_FILE_NOT_FOUND;

   No_Such_Process                     : constant Error_Code
     := 16#F000_0014#;

   Not_A_Directory                     : constant Error_Code
     := Win32.Winerror.ERROR_PATH_NOT_FOUND;

   Not_Enough_Space                    : constant Error_Code
     := Win32.Winerror.ERROR_NOT_ENOUGH_MEMORY;

   Operation_Not_Implemented           : constant Error_Code
     := Win32.Winerror.ERROR_NOT_SUPPORTED;

   Operation_Not_Permitted             : constant Error_Code
     := 16#F000_0018#;

   Permission_Denied                   : constant Error_Code
     := Win32.Winerror.ERROR_ACCESS_DENIED;

   Read_Only_File_System               : constant Error_Code
     := Win32.Winerror.ERROR_DRIVE_LOCKED;

   Resource_Busy                       : constant Error_Code
     := Win32.Winerror.ERROR_DEVICE_IN_USE;

   Resource_Deadlock_Avoided           : constant Error_Code
     := 16#F000_001B#;

   Resource_Temporarily_Unavailable    : constant Error_Code
     := 16#F000_001C#;

   Too_Many_Links                      : constant Error_Code
     := 16#F000_001D#;

   Too_Many_Open_Files                 : constant Error_Code
     := Win32.Winerror.ERROR_TOO_MANY_OPEN_FILES;

   Too_Many_Open_Files_In_System       : constant Error_Code
     := Win32.Winerror.ERROR_NO_MORE_FILES;

   --  System Identification

   function System_Name return POSIX_String;

   function Node_Name return POSIX_String;

   function Release return POSIX_String;

   function Version return POSIX_String;

   function Machine return POSIX_String;

private

   type POSIX_String_Ptr is access POSIX_String;
   type String_Ptr_Array is array (Positive range <>) of POSIX_String_Ptr;
   type String_Ptr_Array_Ptr is access String_Ptr_Array;

   type POSIX_String_List is record
      Last    : Natural := 0;
      Length  : Natural := 0;
      Strings : String_Ptr_Array_Ptr := null;
   end record;

   Empty_String_List : constant POSIX_String_List :=
                         POSIX_String_List'(0, 0, null);

   type Option_Set is new Win32.UINT;

end POSIX;
