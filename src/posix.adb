
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Interfaces.C;
with Unchecked_Deallocation;
with System;

with Win32.Winbase;

package body POSIX is


   --  constants

   Errno : Error_Code;

   --  Characters and String

   ------------------------
   -- To_POSIX_Character --
   ------------------------

   function To_POSIX_Character (Char : Character)
                                return POSIX_Character is
   begin
      return POSIX_Character'Value (Character'Image (Char));
   exception
      when Constraint_Error =>
         return ' ';
   end To_POSIX_Character;

   ------------------
   -- To_Character --
   ------------------

   function To_Character (Char : POSIX_Character)
                          return Character is
   begin
      return Character'Value (POSIX_Character'Image (Char));
   exception
      when Constraint_Error =>
         return ' ';
   end To_Character;


   ---------------------
   -- To_POSIX_String --
   ---------------------

   function To_POSIX_String (Str : String)
                             return POSIX_String is
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

   function To_String (Str : POSIX_String)
                       return String is
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
   function Is_Filename (Str : POSIX_String)
                         return Boolean is
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
   function Is_Pathname (Str : POSIX_String)
                         return Boolean is
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

   function Is_Portable_Filename (Str : POSIX_String)
                                  return Boolean is
   begin
      return Is_Filename (Str) and then
        Str'Length <= POSIX.Portable_Filename_Limit_Maximum;
   end Is_Portable_Filename;


   --------------------------
   -- Is_Portable_Pathname --
   --------------------------

   function Is_Portable_Pathname (Str : POSIX_String)
                                  return Boolean is
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
      --  free the memory
      for I in List.Strings'First .. List.Last loop
         Free (List.Strings (I));
      end loop;
      Free (List.Strings);
      --  reinitialize the values
      List.Length := 0;
      List.Last := 0;
      List.Strings := null;
   end Make_Empty;


   ------------
   -- Append --
   ------------

   procedure Append  (List : in out POSIX_String_List;
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

   function Length (List : POSIX_String_List)
                    return Natural is
   begin
      return List.Last;
   end Length;


   -----------
   -- Value --
   -----------

   function Value (List  : POSIX_String_List;
                   Index : Positive)
                   return POSIX_String is
   begin
      return List.Strings (Index) . all;
   end Value;


   ---------------
   -- Empty_Set --
   ---------------

   function Empty_Set
     return Option_Set is
   begin
      return Option_Set'(0);
   end Empty_Set;


   ---------
   -- "+" --
   ---------

   function "+" (L, R : Option_Set)
                 return Option_Set is
      use type Win32.UINT;
   begin
      return Option_Set (L or R);
   end "+";


   ---------
   -- "-" --
   ---------

   function "-" (L, R : Option_Set)
                 return Option_Set
   is
      use type Win32.UINT;
   begin
      return Option_Set (L xor (L and R));
   end "-";



   --  Exceptions and error codes

   --------------------
   -- Get_Error_Code --
   --------------------

   function Get_Error_Code
     return Error_Code is
   begin
      return Errno;
   end Get_Error_Code;


   --------------------
   -- Set_Error_Code --
   --------------------

   procedure Set_Error_Code (Error : in Error_Code) is
   begin
      Errno := Error;
   end Set_Error_Code;


   --------------------
   -- Is_POSIX_Error --
   --------------------

   function Is_POSIX_Error (Error : Error_Code)
                            return Boolean is
   begin
      if Error = No_Error then
         return True;
      elsif Error = Argument_List_Too_Long then
         return True;
      elsif Error = Bad_File_Descriptor then
         return True;
      elsif Error = Broken_Pipe then
         return True;
      elsif Error = Directory_Not_Empty then
         return True;
      elsif Error = Exec_Format_Error then
         return True;
      elsif Error = File_Exists then
         return True;
      elsif Error = File_Too_Large then
         return True;
      elsif Error = Filename_Too_Long then
         return True;
      elsif Error = Improper_Link then
         return True;
      elsif Error = Inappropriate_IO_Control_Operation then
         return True;
      elsif Error = Input_Output_Error then
         return True;
      elsif Error = Interrupted_Operation then
         return True;
      elsif Error = Invalid_Argument then
         return True;
      elsif Error = Invalid_Seek then
         return True;
      elsif Error = Is_A_Directory then
         return True;
      elsif Error = No_Child_Process then
         return True;
      elsif Error = No_Locks_Available then
         return True;
      elsif Error = No_Space_Left_On_Device then
         return True;
      elsif Error = No_Such_Operation_On_Device then
         return True;
      elsif Error = No_Such_Device_Or_Address then
         return True;
      elsif Error = No_Such_File_Or_Directory then
         return True;
      elsif Error = No_Such_Process then
         return True;
      elsif Error = Not_A_Directory then
         return True;
      elsif Error = Not_Enough_Space then
         return True;
      elsif Error = Operation_Not_Implemented then
         return True;
      elsif Error = Operation_Not_Permited then
         return True;
      elsif Error = Permission_Denied then
         return True;
      elsif Error = Read_Only_File_System then
         return True;
      elsif Error = Resource_Busy then
         return True;
      elsif Error = Resource_Deadlock_Avoided then
         return True;
      elsif Error = Resource_Temporarily_Unavailable then
         return True;
      elsif Error = Too_Many_Links then
         return True;
      elsif Error = Too_Many_Open_Files then
         return True;
      elsif Error = Too_Many_Open_Files_In_System then
         return True;
      else
         return False;
      end if;
   end Is_POSIX_Error;


   -----------
   -- Image --
   -----------

   function Image (Error : Error_Code)
                   return String is
   begin
      if Error = No_Error then
         return "NO_ERROR";
      elsif Error = Argument_List_Too_Long then
         return "ARGUMENT_LIST_TOO_LONG";
      elsif Error = Bad_File_Descriptor then
         return "BAD_FILE_DESCRIPTOR";
      elsif Error = Broken_Pipe then
         return "BROKEN_PIPE";
      elsif Error = Directory_Not_Empty then
         return "DIRECTORY_NOT_EMPTY";
      elsif Error = Exec_Format_Error then
         return "EXEC_FORMAT_ERROR";
      elsif Error = File_Exists then
         return "FILE_EXISTS";
      elsif Error = File_Too_Large then
         return "FILE_TOO_LARGE";
      elsif Error = Filename_Too_Long then
         return "FILENAME_TOO_LONG";
      elsif Error = Improper_Link then
         return "IMPROPER_LINK";
      elsif Error = Inappropriate_IO_Control_Operation then
         return "INAPPROPRIATE_IO_CONTROL_OPERATION";
      elsif Error = Input_Output_Error then
         return "INPUT_OUTPUT_ERROR";
      elsif Error = Interrupted_Operation then
         return "INTERRUPTED_OPERATION";
      elsif Error = Invalid_Argument then
         return "INVALID_ARGUMENT";
      elsif Error = Invalid_Seek then
         return "INVALID_SEEK";
      elsif Error = Is_A_Directory then
         return "IS_A_DIRECTORY";
      elsif Error = No_Child_Process then
         return "NO_CHILD_PROCESS";
      elsif Error = No_Locks_Available then
         return "NO_LOCKS_AVAILABLE";
      elsif Error = No_Space_Left_On_Device then
         return "NO_SPACE_LEFT_ON_DEVICE";
      elsif Error = No_Such_Operation_On_Device then
         return "NO_SUCH_OPERATION_ON_DEVICE";
      elsif Error = No_Such_Device_Or_Address then
         return "NO_SUCH_DEVICE_OR_ADDRESS";
      elsif Error = No_Such_File_Or_Directory then
         return "NO_SUCH_FILE_OR_DIRECTORY";
      elsif Error = No_Such_Process then
         return "NO_SUCH_PROCESS";
      elsif Error = Not_A_Directory then
         return "NOT_A_DIRECTORY";
      elsif Error = Not_Enough_Space then
         return "NOT_ENOUGH_SPACE";
      elsif Error = Operation_Not_Implemented then
         return "OPERATION_NOT_IMPLEMENTED";
      elsif Error = Operation_Not_Permited then
         return "OPERATION_NOT_PERMITED";
      elsif Error = Permission_Denied then
         return "PERMISSION_DENIED";
      elsif Error = Read_Only_File_System then
         return "READ_ONLY_FILE_SYSTEM";
      elsif Error = Resource_Busy then
         return "RESOURCE_BUSY";
      elsif Error = Resource_Deadlock_Avoided then
         return "RESOURCE_DEADLOCK_AVOIDED";
      elsif Error = Resource_Temporarily_Unavailable then
         return "RESOURCE_TEMPORARILY_UNAVAILABLE";
      elsif Error = Too_Many_Links then
         return "TOO_MANY_LINKS";
      elsif Error = Too_Many_Open_Files then
         return "TOO_MANY_OPEN_FILES";
      elsif Error = Too_Many_Open_Files_In_System then
         return "TOO_MANY_OPEN_FILES_IN_SYSTEM";
      end if;
      return "Error Not Known";
   end Image;



   --  System Identification

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
