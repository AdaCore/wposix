
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Win32.Winbase;
with Win32.Winnt;

with POSIX_Signals;
with POSIX_File_Status;
with POSIX_Win32;
with POSIX_Win32.File_Handle;

package body POSIX_IO is

   type IO_Data is
      record
         Mode          : File_Mode;
         Options       : Open_Option_Set;
         Close_On_Exec : Boolean;
      end record;

   type Files_Data_Set is array (File_Descriptor'Range) of IO_Data;

   protected IO_Info is
      function  Get (FD : in File_Descriptor) return IO_Data;
      procedure Set (FD : in File_Descriptor; IOD     : in IO_Data);
      procedure Set (FD : in File_Descriptor; Options : in Open_Option_Set);
      procedure Set (FD : in File_Descriptor; COE     : in Boolean);
   private
      Files_Data : Files_Data_Set;
   end IO_Info;

   protected body IO_Info is

      function  Get (FD : in File_Descriptor) return IO_Data is
      begin
         return Files_Data (FD);
      end Get;

      procedure Set (FD : in File_Descriptor; IOD : in IO_Data) is
      begin
         Files_Data (FD) := IOD;
      end Set;

      procedure Set (FD : in File_Descriptor; Options : in Open_Option_Set) is
      begin
         Files_Data (FD).Options := Options;
      end Set;

      procedure Set (FD : in File_Descriptor; COE     : in Boolean) is
      begin
         Files_Data (FD).Close_On_Exec := COE;
      end Set;

   end IO_Info;


   --  Operations to open or close file descriptors

   --------------------
   -- Mode_To_Access --
   --------------------

   function Mode_To_Access (Mode : in File_Mode)
                            return Win32.DWORD is
   begin
      case Mode is
         when Read_Only =>
            return Win32.Winnt.GENERIC_READ;
         when Write_Only =>
            return Win32.Winnt.GENERIC_WRITE;
         when Read_Write =>
            return Win32.Winnt.GENERIC_WRITE;
      end case;
   end Mode_To_Access;


   ------------
   -- Is_Set --
   ------------

   function Is_Set (Options : in Open_Option_Set;
                    V       : in Open_Option_Set)
                    return Boolean is
   begin
      if (Options - V) = Options then
         return False;
      else
         return True;
      end if;
   end Is_Set;

   ------------
   -- Shared --
   ------------

   function Shared (Options : in Open_Option_Set)
                    return Win32.DWORD is
      use type Win32.DWORD;
   begin
      if Is_Set (Options, Exclusive) then
         return Win32.DWORD'(0);
      else
         return
           Win32.Winnt.FILE_SHARE_READ +
           Win32.Winnt.FILE_SHARE_WRITE;
      end if;
   end Shared;


   ----------
   -- Open --
   ----------

   function Open
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor
   is
      use type Win32.Winnt.HANDLE;
      Handle : Win32.Winnt.HANDLE;
      L_Name : constant String := POSIX.To_String (Name) & ASCII.Nul;

      function Truncated (Options : in Open_Option_Set)
                          return Win32.DWORD is
      begin
         if Is_Set (Options, Truncate) then
            return Win32.Winbase.TRUNCATE_EXISTING;
         else
            return Win32.Winbase.OPEN_EXISTING;
         end if;
      end Truncated;

   begin
      Handle := Win32.Winbase.CreateFile
        (Win32.Addr (L_Name),
         Mode_To_Access (Mode),
         Shared (Options),
         null, --  Security Attributes
         Truncated (Options),
         Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
         POSIX_Win32.Null_Handle);
      if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
         POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error, "Open");
      end if;

      declare
         FD : File_Descriptor := POSIX_Win32.File_Handle.Open (Handle);
      begin
         IO_Info.Set (FD, (Mode, Options, False));
         return FD;
      end;
   end Open;


   --------------------
   -- Open_Or_Create --
   --------------------

   function Open_Or_Create
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Permissions    : POSIX_Permissions.Permission_Set;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor
   is
      use type Win32.Winnt.HANDLE;
      Handle : Win32.Winnt.HANDLE;
      L_Name : constant String := POSIX.To_String (Name) & ASCII.Nul;

      function Truncated (Options : in Open_Option_Set)
                          return Win32.DWORD is
      begin
         if Is_Set (Options, Truncate) then
            return Win32.Winbase.CREATE_ALWAYS;
         else
            return Win32.Winbase.OPEN_ALWAYS;
         end if;
      end Truncated;

      function Permission_Set_To_Attributes
        (Permissions : in POSIX_Permissions.Permission_Set)
         return Win32.DWORD
      is
         use POSIX_Permissions;
      begin
         if Permissions (Owner_Read) or else Permissions (Owner_Write) then
            return Win32.Winnt.FILE_ATTRIBUTE_NORMAL;
         else
            return Win32.Winnt.FILE_ATTRIBUTE_READONLY;
         end if;
      end Permission_Set_To_Attributes;

   begin
      Handle := Win32.Winbase.CreateFile
        (Win32.Addr (L_Name),
         Mode_To_Access (Mode),
         Shared (Options),
         null, --  Security Attributes
         Truncated (Options),
         Permission_Set_To_Attributes (Permissions),
         POSIX_Win32.Null_Handle);
      if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
         POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error,
                                    "Open_Or_Create");
      end if;

      declare
         FD : File_Descriptor := POSIX_Win32.File_Handle.Open (Handle);
      begin
         IO_Info.Set (FD, (Mode, Options, False));
         return FD;
      end;
   end Open_Or_Create;


   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : File_Descriptor)
                     return Boolean
   is
      use type Win32.Winnt.HANDLE;
   begin
      return POSIX_Win32.File_Handle.Get (File) /= POSIX_Win32.Null_Handle;
   end Is_Open;


   -----------
   -- Close --
   -----------

   procedure Close
     (File    : in File_Descriptor;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Result : Win32.BOOL;
   begin
      Result := Win32.Winbase.CloseHandle
        (POSIX_Win32.File_Handle.Get (File));
      POSIX_Win32.Check_Result (Result, "Close");
      POSIX_Win32.File_Handle.Close (File);
   end Close;


   Handle : aliased Win32.Winnt.HANDLE;

   ---------------
   -- Duplicate --
   ---------------

   function Duplicate
     (File   : File_Descriptor;
      Target : File_Descriptor := 0)
      return File_Descriptor
   is
      use type Win32.DWORD;
      New_File : File_Descriptor;
      Result   : Win32.BOOL;
   begin
      Result := Win32.Winbase.DuplicateHandle
        (Win32.Winbase.GetCurrentProcess,
         POSIX_Win32.File_Handle.Get (File),
         Win32.Winbase.GetCurrentProcess,
         Handle'Access,
         0,
         Win32.TRUE,
         Win32.Winnt.DUPLICATE_SAME_ACCESS);
      POSIX_Win32.Check_Result (Result, "Duplicate");
      New_File := POSIX_Win32.File_Handle.Open (Handle, File);
      return New_File;
   end Duplicate;


   -------------------------
   -- Duplicate_And_Close --
   -------------------------

   function Duplicate_And_Close
     (File           : File_Descriptor;
      Target         : File_Descriptor := 0;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor
   is
      use type Win32.DWORD;
      New_File : File_Descriptor;
      Result   : Win32.BOOL;
   begin
      Result := Win32.Winbase.DuplicateHandle
        (Win32.Winbase.GetCurrentProcess,
         POSIX_Win32.File_Handle.Get (File),
         Win32.Winbase.GetCurrentProcess,
         Handle'Access,
         0,
         Win32.TRUE,
         Win32.Winnt.DUPLICATE_SAME_ACCESS +
         Win32.Winnt.DUPLICATE_CLOSE_SOURCE);
      POSIX_Win32.Check_Result (Result, "Duplicate");
      New_File := POSIX_Win32.File_Handle.Open (Handle, File);
      return New_File;
   end Duplicate_And_Close;


   -----------------
   -- Create_Pipe --
   -----------------

   procedure Create_Pipe
     (Read_End  : out File_Descriptor;
      Write_End : out File_Descriptor)
   is
      Read_Handle, Write_Handle : aliased Win32.Winnt.HANDLE;
      Result                    : Win32.BOOL;
   begin
      Result := Win32.Winbase.CreatePipe
        (Read_Handle'Unchecked_Access,
         Write_Handle'Unchecked_Access,
         null, --  Security Attributes
         4096);
      POSIX_Win32.Check_Result (Result, "Create_Pipe");
      Read_End  := POSIX_Win32.File_Handle.Open (Read_Handle);
      Write_End := POSIX_Win32.File_Handle.Open (Write_Handle);
   end Create_Pipe;




   --  File Input/Output operations

   ----------
   -- Read --
   ----------

   procedure Read
     (File           : in     File_Descriptor;
      Buffer         :    out IO_Buffer;
      Last           :    out POSIX.IO_Count;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Result     : Win32.BOOL;
      Bytes_Read : aliased Win32.DWORD;
   begin
      Result := Win32.Winbase.ReadFile
        (POSIX_Win32.File_Handle.Get (File),
         Buffer (Buffer'First)'Address,
         Win32.DWORD (Buffer'Length),
         Bytes_Read'Unchecked_Access,
         null);

      POSIX_Win32.Check_Result (Result, "Read");

      Last := POSIX.IO_Count (Bytes_Read);
   end Read;


   -----------
   -- Write --
   -----------

   procedure Write
     (File       : in     File_Descriptor;
      Buffer     : in     IO_Buffer;
      Last       :    out POSIX.IO_Count;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Result        : Win32.BOOL;
      Bytes_Written : aliased Win32.DWORD;
   begin
      Result := Win32.Winbase.WriteFile
        (POSIX_Win32.File_Handle.Get (File),
         Buffer (Buffer'First)'Address,
         Win32.DWORD (Buffer'Length),
         Bytes_Written'Unchecked_Access,
         null);

      POSIX_Win32.Check_Result (Result, "Write");

      Last := POSIX.IO_Count (Bytes_Written);
   end Write;


   ------------------
   -- Generic_Read --
   ------------------

   procedure Generic_Read
     (File           : in     File_Descriptor;
      Item           :    out T;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Number_Of_Bytes : Positive;
      Result          : Win32.BOOL;
      Bytes_Read      : aliased Win32.DWORD;
   begin
      Number_Of_Bytes := Item'Size / 8;

      Result := Win32.Winbase.ReadFile
        (POSIX_Win32.File_Handle.Get (File),
         Item'Address,
         Win32.DWORD (Number_Of_Bytes),
         Bytes_Read'Unchecked_Access,
         null);

      POSIX_Win32.Check_Result (Result, "Generic_Read");
   end Generic_Read;


   -------------------
   -- Generic_Write --
   -------------------

   procedure Generic_Write
     (File           : in     File_Descriptor;
      Item           : in     T;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Number_Of_Bytes : Positive;
      Result          : Win32.BOOL;
      Bytes_Written   : aliased Win32.DWORD;
   begin
      Number_Of_Bytes := Item'Size / 8;

      Result := Win32.Winbase.WriteFile
        (POSIX_Win32.File_Handle.Get (File),
         Item'Address,
         Win32.DWORD (Number_Of_Bytes),
         Bytes_Written'Unchecked_Access,
         null);

      POSIX_Win32.Check_Result (Result, "Generic_Write");
   end Generic_Write;





   --  File position operations

   ---------------
   -- To_Origin --
   ---------------

   function To_Origin (SP : Position)
                       return Win32.DWORD is
   begin
      case SP is
         when From_Beginning =>
            return Win32.Winbase.FILE_BEGIN;
         when From_Current_Position =>
            return Win32.Winbase.FILE_CURRENT;
         when From_End_Of_File =>
            return Win32.Winbase.FILE_END;
      end case;
   end To_Origin;

   DistanceToMoveHigh : aliased Win32.LONG := 0;

   ----------
   -- Seek --
   ----------

   procedure Seek
     (File           : in     File_Descriptor;
      Offset         : in     IO_Offset;
      Result         :    out IO_Offset;
      Starting_Point : in     Position := From_Beginning)
   is
      use type Win32.DWORD;
      Low_Position       : Win32.DWORD;
   begin
      Low_Position := Win32.Winbase.SetFilePointer
        (POSIX_Win32.File_Handle.Get (File),
         Win32.LONG (Offset),
         DistanceToMoveHigh'Access,
         To_Origin (Starting_Point));

      if Low_Position = 16#FFFF_FFFF# then
         POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error, "Seek");
      else
         Result := IO_Offset (Low_Position);
      end if;
   end Seek;


   File_Size_High : aliased Win32.DWORD;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (File : File_Descriptor)
                       return POSIX.IO_Count
   is
      use type Win32.DWORD;
      File_Size_Low  : Win32.DWORD;
   begin
      File_Size_Low := Win32.Winbase.GetFileSize
        (POSIX_Win32.File_Handle.Get (File),
         File_Size_High'Access);

      if File_Size_Low = 16#FFFF_FFFF# then
         POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error, "File_Size");
      end if;

      return POSIX.IO_Count (File_Size_Low);
   end File_Size;


   -------------------
   -- File_Position --
   -------------------

   function File_Position (File : File_Descriptor)
                           return POSIX.IO_Count
   is
      use type Win32.DWORD;
      Low_Position : Win32.DWORD;
   begin
      Low_Position := Win32.Winbase.SetFilePointer
        (POSIX_Win32.File_Handle.Get (File),
         Win32.LONG (0),
         DistanceToMoveHigh'Access,
         Win32.Winbase.FILE_CURRENT);

      if Low_Position = 16#FFFF_FFFF# then
         POSIX_Win32.Check_Retcode (POSIX_Win32.Retcode_Error,
                                    "File_Position");
      end if;
      return POSIX.IO_Count (Low_Position);
   end File_Position;




   --  Terminal operations

   -------------------
   -- Is_A_Terminal --
   -------------------

   function Is_A_Terminal (File : File_Descriptor)
                           return Boolean is
   begin
      return POSIX_File_Status.Is_Character_Special_File
        (POSIX_File_Status.Get_File_Status (File));
   end Is_A_Terminal;


   -----------------------
   -- Get_Terminal_Name --
   -----------------------

   function Get_Terminal_Name (File : File_Descriptor)
                               return POSIX.Pathname is
   begin
      return POSIX.To_POSIX_String ("command");
   end Get_Terminal_Name;



   --  File Control operations

   ----------------------
   -- Get_File_Control --
   ----------------------

   procedure Get_File_Control
     (File       : in     File_Descriptor;
      Mode       :    out File_Mode;
      Options    :    out Open_Option_Set)
   is
      IOD : IO_Data := IO_Info.Get (File);
   begin
      Mode    := IOD.Mode;
      Options := IOD.Options;
   end Get_File_Control;


   ----------------------
   -- Set_File_Control --
   ----------------------

   procedure Set_File_Control
     (File    : in     File_Descriptor;
      Options : in     Open_Option_Set) is
   begin
      IO_Info.Set (File, Options);
   end Set_File_Control;


   -----------------------
   -- Get_Close_On_Exec --
   -----------------------

   function Get_Close_On_Exec (File : File_Descriptor)
                               return Boolean
   is
      IOD : IO_Data := IO_Info.Get (File);
   begin
      return IOD.Close_On_Exec;
   end Get_Close_On_Exec;


   -----------------------
   -- Set_Close_On_Exec --
   -----------------------

   procedure Set_Close_On_Exec
     (File : in     File_Descriptor;
      To   : in     Boolean := True) is
   begin
      IO_Info.Set (File, COE => To);
   end Set_Close_On_Exec;

end POSIX_IO;
