
--  $Id$
--  Author : Pascal Obry
--  p.obry@wanadoo.fr

with Interfaces.C;
with Unchecked_Deallocation;
with System;
with Ada.Task_Attributes;

with Win32.Winbase;

package body POSIX is


   --  Make Errno a Task Attribute to be safe wrt multi-tasking

   package Errno is new Ada.Task_Attributes (Error_Code, No_Error);

   --  Characters and String

   ------------------------
   -- To_POSIX_Character --
   ------------------------

   function To_POSIX_Character (Char : in Character) return POSIX_Character is
   begin
      return POSIX_Character'Value (Character'Image (Char));
   exception
      when Constraint_Error =>
         return ' ';
   end To_POSIX_Character;

   ------------------
   -- To_Character --
   ------------------

   function To_Character (Char : in POSIX_Character) return Character is
   begin
      return Character'Value (POSIX_Character'Image (Char));
   exception
      when Constraint_Error =>
         return ' ';
   end To_Character;

   ---------------------
   -- To_POSIX_String --
   ---------------------

   function To_POSIX_String (Str : in String) return POSIX_String is
      Posix_Str : POSIX_String (Str'Range);
   begin
      for I in Posix_Str'Range loop
         Posix_Str (I) := To_POSIX_Character (Str (I));
      end loop;
      return Posix_Str;
   end To_POSIX_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Str : in POSIX_String) return String is
      Sstr : String (Str'Range);
   begin
      for I in Sstr'Range loop
         Sstr (I) := To_Character (Str (I));
      end loop;
      return Sstr;
   end To_String;

   -----------------
   -- Is_Filename --
   -----------------

   --  A valid Filename is everything except the null string,
   --  the strings "." and ".."

   function Is_Filename (Str : in POSIX_String) return Boolean is
   begin
      if Str = (Str'Range => ' ') or else Str = "." or else Str = ".." then
         return False;
      else
         return True;
      end if;
   end Is_Filename;

   -----------------
   -- Is_Pathname --
   -----------------

   --  A valid Pathname is everything except the null string

   function Is_Pathname (Str : in POSIX_String) return Boolean is
   begin
      if Str = (Str'Range => ' ') then
         return False;
      else
         return True;
      end if;
   end Is_Pathname;

   --------------------------
   -- Is_Portable_Filename --
   --------------------------

   function Is_Portable_Filename (Str : in POSIX_String) return Boolean is
   begin
      return Is_Filename (Str) and then
        Str'Length <= POSIX.Portable_Filename_Limit_Maximum;
   end Is_Portable_Filename;

   --------------------------
   -- Is_Portable_Pathname --
   --------------------------

   function Is_Portable_Pathname (Str : in POSIX_String) return Boolean is
   begin
      return Is_Pathname (Str) and then
        Str'Length <= POSIX.Portable_Pathname_Limit_Maximum;
   end Is_Portable_Pathname;

   --  String Lists

   ----------
   -- Free --
   ----------

   procedure Free is new Unchecked_Deallocation
     (String_Ptr_Array, String_Ptr_Array_Ptr);

   ----------------
   -- Make_Empty --
   ----------------

   procedure Make_Empty (List : in out POSIX_String_List) is

      procedure Free is new Unchecked_Deallocation
        (POSIX_String, POSIX_String_Ptr);

   begin
      if List.Strings = null then
         --  Already empty
         return;
      end if;

      --  Free the memory

      for I in List.Strings'First .. List.Last loop
         Free (List.Strings (I));
      end loop;
      Free (List.Strings);

      --  Reinitialize the values

      List.Length := 0;
      List.Last := 0;
      List.Strings := null;
   end Make_Empty;

   ------------
   -- Append --
   ------------

   procedure Append
     (List : in out POSIX_String_List;
      Str  : in     POSIX_String)
   is
      Strings : String_Ptr_Array_Ptr;
   begin
      List.Last := List.Last + 1;

      --  if there is no more space on the list
      if List.Last > List.Length then

         List.Length := List.Length + 10;
         Strings := new String_Ptr_Array (1 .. List.Length);

         --  we don't have to copy the content if empty
         --  i.e. if we insert the first item
         if List.Last > 1 then
            Strings (1 .. List.Last - 1) := List.Strings (1 .. List.Last - 1);
            Free (List.Strings);
         end if;

         List.Strings := Strings;
      end if;

      List.Strings (List.Last) := new POSIX_String'(Str);
   end Append;

   --------------------
   -- For_Every_Item --
   --------------------

   procedure For_Every_Item (List : in POSIX_String_List) is
      Quit : Boolean := False;
   begin
      --  return if list empty
      if List.Last = 0 then
         return;
      end if;
      for I in List.Strings'First .. List.Last loop
         Action (List.Strings (I) . all, Quit);
         exit when Quit;
      end loop;
   end For_Every_Item;

   ------------
   -- Length --
   ------------

   function Length (List : in POSIX_String_List) return Natural is
   begin
      return List.Last;
   end Length;

   -----------
   -- Value --
   -----------

   function Value
     (List  : in POSIX_String_List;
      Index : in Positive)
     return POSIX_String is
   begin
      return List.Strings (Index) . all;
   end Value;

   ---------------
   -- Empty_Set --
   ---------------

   function Empty_Set return Option_Set is
   begin
      return Option_Set'(0);
   end Empty_Set;

   ---------
   -- "+" --
   ---------

   function "+" (L, R : in Option_Set) return Option_Set is
      use type Win32.UINT;
   begin
      return Option_Set (L or R);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : in Option_Set) return Option_Set is
      use type Win32.UINT;
   begin
      return Option_Set (L xor (L and R));
   end "-";

   --  Exceptions and error codes

   --------------------
   -- Get_Error_Code --
   --------------------

   function Get_Error_Code return Error_Code is
   begin
      return Errno.Value;
   end Get_Error_Code;

   --------------------
   -- Set_Error_Code --
   --------------------

   procedure Set_Error_Code (Error : in Error_Code) is
   begin
      Errno.Set_Value (Error);
   end Set_Error_Code;

   --------------------
   -- Is_POSIX_Error --
   --------------------

   function Is_POSIX_Error (Error : in Error_Code) return Boolean is
   begin
      case Error is
         when
             No_Error                  | Argument_List_Too_Long
           | Bad_File_Descriptor       | Broken_Pipe
           | Directory_Not_Empty       | Exec_Format_Error
           | File_Exists               | File_Too_Large
           | Filename_Too_Long         | Improper_Link
           | Inappropriate_IO_Control_Operation
           | Input_Output_Error        | Interrupted_Operation
           | Invalid_Argument          | Invalid_Seek
           | Is_A_Directory            | No_Child_Process
           | No_Locks_Available        | No_Space_Left_On_Device
           | No_Such_Operation_On_Device
           | No_Such_Device_Or_Address | No_Such_File_Or_Directory
           | No_Such_Process           | Not_A_Directory
           | Not_Enough_Space          | Operation_Not_Implemented
           | Operation_Not_Permitted   | Permission_Denied
           | Read_Only_File_System     | Resource_Busy
           | Resource_Deadlock_Avoided | Resource_Temporarily_Unavailable
           | Too_Many_Links            | Too_Many_Open_Files
           | Too_Many_Open_Files_In_System
           =>
            return True;

         when others =>
            return False;
      end case;
   end Is_POSIX_Error;

   -----------
   -- Image --
   -----------

   function Image (Error : in Error_Code) return String is
   begin
      case Error is
         when No_Error =>
            return "NO_ERROR";
         when Argument_List_Too_Long =>
            return "ARGUMENT_LIST_TOO_LONG";
         when Bad_File_Descriptor =>
            return "BAD_FILE_DESCRIPTOR";
         when Broken_Pipe =>
            return "BROKEN_PIPE";
         when Directory_Not_Empty =>
            return "DIRECTORY_NOT_EMPTY";
         when Exec_Format_Error =>
            return "EXEC_FORMAT_ERROR";
         when File_Exists =>
            return "FILE_EXISTS";
         when File_Too_Large =>
            return "FILE_TOO_LARGE";
         when Filename_Too_Long =>
            return "FILENAME_TOO_LONG";
         when Improper_Link =>
            return "IMPROPER_LINK";
         when Inappropriate_IO_Control_Operation =>
            return "INAPPROPRIATE_IO_CONTROL_OPERATION";
         when Input_Output_Error =>
            return "INPUT_OUTPUT_ERROR";
         when Interrupted_Operation =>
            return "INTERRUPTED_OPERATION";
         when Invalid_Argument =>
            return "INVALID_ARGUMENT";
         when Invalid_Seek =>
            return "INVALID_SEEK";
         when Is_A_Directory =>
            return "IS_A_DIRECTORY";
         when No_Child_Process =>
            return "NO_CHILD_PROCESS";
         when No_Locks_Available =>
            return "NO_LOCKS_AVAILABLE";
         when No_Space_Left_On_Device =>
            return "NO_SPACE_LEFT_ON_DEVICE";
         when No_Such_Operation_On_Device =>
            return "NO_SUCH_OPERATION_ON_DEVICE";
         when No_Such_Device_Or_Address =>
            return "NO_SUCH_DEVICE_OR_ADDRESS";
         when No_Such_File_Or_Directory =>
            return "NO_SUCH_FILE_OR_DIRECTORY";
         when No_Such_Process =>
            return "NO_SUCH_PROCESS";
         when Not_A_Directory =>
            return "NOT_A_DIRECTORY";
         when Not_Enough_Space =>
            return "NOT_ENOUGH_SPACE";
         when Operation_Not_Implemented =>
            return "OPERATION_NOT_IMPLEMENTED";
         when Operation_Not_Permitted =>
            return "OPERATION_NOT_PERMITTED";
         when Permission_Denied =>
            return "PERMISSION_DENIED";
         when Read_Only_File_System =>
            return "READ_ONLY_FILE_SYSTEM";
         when Resource_Busy =>
            return "RESOURCE_BUSY";
         when Resource_Deadlock_Avoided =>
            return "RESOURCE_DEADLOCK_AVOIDED";
         when Resource_Temporarily_Unavailable =>
            return "RESOURCE_TEMPORARILY_UNAVAILABLE";
         when Too_Many_Links =>
            return "TOO_MANY_LINKS";
         when Too_Many_Open_Files =>
            return "TOO_MANY_OPEN_FILES";
         when Too_Many_Open_Files_In_System =>
            return "TOO_MANY_OPEN_FILES_IN_SYSTEM";
         when others =>
            return "Error Not Known";
      end case;
   end Image;

   --  System Identification

   ----------------------
   -- Get_Version_Info --
   ----------------------

   procedure Get_Version_Info
     (Version_Information : out Win32.Winbase.OSVERSIONINFOA)
   is
      use type Win32.BOOL;
      VersionInformation : aliased Win32.Winbase.OSVERSIONINFOA;
      Status             : Win32.BOOL;
   begin
      VersionInformation.dwOSVersionInfoSize :=
        Win32.DWORD (VersionInformation'Size / System.Storage_Unit);

      Status := Win32.Winbase.GetVersionEx
        (VersionInformation'Unchecked_Access);

      Version_Information := VersionInformation;

      if Status = 0 then
         raise POSIX_Error;
      end if;
   end Get_Version_Info;

   -----------------
   -- System_Name --
   -----------------

   function System_Name return POSIX_String is
      VersionInformation : Win32.Winbase.OSVERSIONINFOA;
   begin
      Get_Version_Info (VersionInformation);

      case VersionInformation.dwPlatformId is

         when Win32.Winbase.VER_PLATFORM_WIN32S   =>
            return To_POSIX_String ("Win32s on Windows 3.1");
         when 1 =>
            return To_POSIX_String ("Win32 on Windows 95");
         when Win32.Winbase.VER_PLATFORM_WIN32_NT =>
            return To_POSIX_String ("Windows NT");
         when others =>
            return To_POSIX_String ("UNKNOWN !!!");

      end case;

   exception
      when others =>
         return To_POSIX_String ("unknown");
   end System_Name;

   ---------------
   -- Node_Name --
   ---------------

   function Node_Name return POSIX_String is
      use type Interfaces.C.size_t;
      use type Interfaces.C.unsigned_long;
      use type Win32.BOOL;

      Status : Win32.BOOL;
      Buffer : aliased Interfaces.C.char_array
        (1 .. Win32.Winbase.MAX_COMPUTERNAME_LENGTH + 1);
      Size   : aliased Win32.DWORD := Buffer'Length;

   begin
      Status := Win32.Winbase.GetComputerName
        (LpBuffer => Buffer (1)'Unchecked_Access,
         NSize    => Size'Unchecked_Access);

      if Status = 0 then
         return To_POSIX_String ("unknown");
      end if;

      return To_POSIX_String
        (Interfaces.C.To_Ada (Buffer (1 .. Interfaces.C.size_t (Size + 1))));
   end Node_Name;

   ------------------
   -- Remove_First --
   ------------------

   function Remove_First (S : in String) return POSIX_String is
   begin
      return To_POSIX_String (S (2 .. S'Last));
   end Remove_First;

   -------------
   -- Release --
   -------------

   function Release return POSIX_String is
      VersionInformation : Win32.Winbase.OSVERSIONINFOA;
   begin
      Get_Version_Info (VersionInformation);

      return Remove_First
        (Win32.DWORD'Image (VersionInformation.dwMinorVersion));

   exception
      when others =>
         return To_POSIX_String ("unknown");
   end Release;

   -------------
   -- Version --
   -------------

   function Version return POSIX_String is
      VersionInformation : Win32.Winbase.OSVERSIONINFOA;
   begin
      Get_Version_Info (VersionInformation);

      return Remove_First
        (Win32.DWORD'Image (VersionInformation.dwMajorVersion));

   exception
      when others =>
         return To_POSIX_String ("unknown");
   end Version;

   -------------
   -- Machine --
   -------------

   function Machine return POSIX_String is
      SystemInfo : aliased Win32.Winbase.SYSTEM_INFO;
   begin
      Win32.Winbase.GetSystemInfo (SystemInfo'Unchecked_Access);

      return Remove_First
        (Win32.DWORD'Image (SystemInfo.dwProcessorType));
   end Machine;

end POSIX;
