
--  $Id$

with POSIX_Signals;
with Win32.Crt.Fcntl;
with Win32.Crt.Stat;
with Win32.Crt.IO;
with Win32.Crt.Stdio;

with POSIX_Win32;

package body POSIX_IO is

   use type Win32.INT;

   Retcode : Win32.INT;


   --  Operations to open or close file descriptors

   function To_Flags (Mode : in File_Mode)
                     return Win32.UINT is
   begin
      case Mode is
         when Read_Only =>
            return Win32.Crt.Fcntl.O_RDONLY;
         when Write_Only =>
            return Win32.Crt.Fcntl.O_WRONLY;
         when Read_Write =>
            return Win32.Crt.Fcntl.O_RDWR;
      end case;
   end To_Flags;

                -----------------------------------

   function To_Flags (Options : in Open_Option_Set)
                     return Win32.UINT
   is
      use type Win32.UINT;
      OOS : Win32.UINT := 0;
   begin
      if Options - Append /= Options then
         OOS := OOS + Win32.Crt.Fcntl.O_APPEND;
      end if;
      if Options - Truncate /= Options then
         OOS := OOS + Win32.Crt.Fcntl.O_TRUNC;
      end if;
      if Options - Exclusive /= Options then
         OOS := OOS + Win32.Crt.Fcntl.O_EXCL;
      end if;
      return OOS;
   end To_Flags;

                -----------------------------------

   function Open
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor
   is
      use type Win32.UINT;
      FD : File_Descriptor;
      L_Name : constant String := POSIX.To_String (Name) & ASCII.Nul;
   begin
      FD := File_Descriptor
        (Win32.Crt.IO.Open
         (Win32.Addr (L_Name),
          To_Flags (Mode) + To_Flags (Options),
          0));
      POSIX_Win32.Check_Retcode (Win32.INT (FD), "Open");
      return FD;
   end Open;

                -----------------------------------

   function Open_Or_Create
     (Name           : POSIX.Pathname;
      Mode           : File_Mode;
      Permissions    : POSIX_Permissions.Permission_Set;
      Options        : Open_Option_Set := Empty_Set;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor
   is
      use type Win32.UINT;
      FD : File_Descriptor;
      L_Name : constant String := POSIX.To_String (Name) & ASCII.Nul;
   begin
      FD := File_Descriptor
        (Win32.Crt.IO.Open
         (Win32.Addr (L_Name),
          Win32.Crt.Fcntl.O_CREAT + To_Flags (Mode) + To_Flags (Options),
          POSIX_Win32.To_Mask (Permissions)));
      POSIX_Win32.Check_Retcode (Win32.INT (FD), "Open_Or_Create");
      return FD;
   end Open_Or_Create;

                -----------------------------------

   function Is_Open (File : File_Descriptor)
                     return Boolean
   is
      use type Win32.INT;
      Data : aliased Win32.Crt.Stat.Struct_Stat;
   begin
      return Win32.Crt.Stat.Fstat (Win32.INT (File), Data'access) /= -1;
   end Is_Open;

                -----------------------------------

   procedure Close
     (File    : in File_Descriptor;
      Masked_Signals : in POSIX.Signal_Masking := POSIX.RTS_Signals) is
   begin
      Retcode := Win32.Crt.IO.Close (Win32.INT (File));
      POSIX_Win32.Check_Retcode (Retcode, "Close");
   end Close;


                -----------------------------------

   function Duplicate
     (File   : File_Descriptor;
      Target : File_Descriptor := 0)
      return File_Descriptor
   is
   begin
      Retcode := Win32.Crt.IO.Dup (Win32.INT (File));
      POSIX_Win32.Check_Retcode (Retcode, "Duplicate");
      return File_Descriptor (Retcode);
   end Duplicate;

                -----------------------------------

   function Duplicate_And_Close
     (File           : File_Descriptor;
      Target         : File_Descriptor := 0;
      Masked_Signals : POSIX.Signal_Masking := POSIX.RTS_Signals)
      return File_Descriptor is
   begin
      Retcode := Win32.Crt.IO.Dup2 (Win32.INT (File), Win32.INT (Target));
      POSIX_Win32.Check_Retcode (Retcode, "Duplicate_And_Close");
      return File_Descriptor (Retcode);
   end Duplicate_And_Close;

                -----------------------------------

   procedure Create_Pipe
     (Read_End  : out File_Descriptor;
      Write_End : out File_Descriptor)
   is
      Pipedes : aliased Win32.Crt.IO.Pipeends;
   begin
      Retcode := Win32.Crt.IO.Pipe (Pipedes'access, 4096, Win32.Crt.Fcntl.O_BINARY);
      POSIX_Win32.Check_Retcode (Retcode, "Create_Pipe");
      Read_End  := File_Descriptor (Pipedes (0));
      Write_End := File_Descriptor (Pipedes (1));
   end Create_Pipe;




   --  File Input/Output operations

   procedure Read
     (File           : in     File_Descriptor;
      Buffer         :    out IO_Buffer;
      Last           :    out POSIX.IO_Count;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Number_Of_Bytes : Win32.UINT;
   begin
      Number_Of_Bytes := Win32.UINT (Buffer'Last);

      Retcode := Win32.Crt.IO.Read (Win32.INT (File),
                                Buffer (1)'Address,
                                Number_Of_Bytes);
      POSIX_Win32.Check_Retcode (Retcode, "Read");

      Last := POSIX.IO_Count (Retcode);
   end Read;

                -----------------------------------

   procedure Write
     (File       : in     File_Descriptor;
      Buffer     : in     IO_Buffer;
      Last       :    out POSIX.IO_Count;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Number_Of_Bytes : Win32.UINT;
   begin
      Number_Of_Bytes := Win32.UINT (Buffer'Last);

      Retcode := Win32.Crt.IO.Write (Win32.INT (File),
                                 Buffer (1)'Address,
                                 Number_Of_Bytes);
      POSIX_Win32.Check_Retcode (Retcode, "Write");

      Last := POSIX.IO_Count (Retcode);
   end Write;

                -----------------------------------

   procedure Generic_Read
     (File           : in     File_Descriptor;
      Item           :    out T;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Number_Of_Bytes : Integer;
   begin
      Number_Of_Bytes := Item'Size / 8;

      Retcode := Win32.Crt.IO.Read (Win32.INT (File),
                                Item'Address,
                                Win32.UINT (Number_Of_Bytes));
      POSIX_Win32.Check_Retcode (Retcode, "Generic_Read");
   end Generic_Read;

                -----------------------------------

   procedure Generic_Write
     (File           : in     File_Descriptor;
      Item           : in     T;
      Masked_Signals : in     POSIX.Signal_Masking := POSIX.RTS_Signals)
   is
      Number_Of_Bytes : Integer;
   begin
      Number_Of_Bytes := Item'Size / 8;

      Retcode := Win32.Crt.IO.Write (Win32.INT (File),
                                 Item'Address,
                                 Win32.UINT (Number_Of_Bytes));
      POSIX_Win32.Check_Retcode (Retcode, "Generic_Write");
   end Generic_Write;





   --  File position operations

   function To_Origin (SP : Position)
                       return Win32.INT is
   begin
      case SP is
         when From_Beginning =>
            return Win32.Crt.Stdio.SEEK_SET;
         when From_Current_Position =>
            return Win32.Crt.Stdio.SEEK_CUR;
         when From_End_Of_File =>
            return Win32.Crt.Stdio.SEEK_END;
      end case;
   end To_Origin;

   procedure Seek
     (File           : in     File_Descriptor;
      Offset         : in     IO_Offset;
      Result         :    out IO_Offset;
      Starting_Point : in     Position := From_Beginning) is
   begin
      Result := IO_Offset (Win32.Crt.IO.Lseek (Win32.INT (File),
                                           Win32.LONG (Offset),
                                           To_Origin (Starting_Point)));
      if Offset = -1 then
         POSIX_Win32.Check_Retcode (-1, "Seek");
      end if;
   end Seek;

                -----------------------------------

   function File_Size (File : File_Descriptor)
                       return POSIX.IO_Count
   is
      Size    : IO_Offset;
      Cur_Pos : IO_Offset;
      Dummy   : IO_Offset;
   begin
      Seek (File, 0, Cur_Pos, From_Current_Position);
      Seek (File, 0, Size, From_End_Of_File);
      Seek (File, Cur_Pos, Dummy, From_Beginning);
      return POSIX.IO_Count (Size);
   end File_Size;

                -----------------------------------

   function File_Position (File : File_Descriptor)
                           return POSIX.IO_Count is
      use type Win32.LONG;
      Position : Win32.LONG;
   begin
      Position := Win32.Crt.IO.Tell (Win32.INT (File));
      if Position = -1 then
         POSIX_Win32.Check_Retcode (-1, "File_Position");
      end if;
      return POSIX.IO_Count (Position);
   end File_Position;




   --  Terminal operations

   function Is_A_Terminal (File : File_Descriptor)
                           return Boolean is
   begin
      Retcode := Win32.Crt.IO.Isatty (Win32.INT (File));
      if Retcode = 0 then
         return False;
      else
         return True;
      end if;
   end Is_A_Terminal;

                -----------------------------------

   function Get_Terminal_Name (File : File_Descriptor)
                               return POSIX.Pathname is
   begin
      return POSIX.To_POSIX_String ("command");
   end Get_Terminal_Name;



   --  File Control operations

   procedure Get_File_Control
     (File       : in     File_Descriptor;
      Mode       :    out File_Mode;
      Options    :    out Open_Option_Set)
   is
   begin
      null;
   end Get_File_Control;

                -----------------------------------

   procedure Set_File_Control
     (File    : in     File_Descriptor;
      Options : in     Open_Option_Set) is
   begin
      null;
   end Set_File_Control;

                -----------------------------------

   function Get_Close_On_Exec (File : File_Descriptor)
                               return Boolean is
   begin
      return False;
  end Get_Close_On_Exec;

                -----------------------------------

   procedure Set_Close_On_Exec
     (File : in     File_Descriptor;
      To   : in     Boolean := True) is
   begin
      null;
   end Set_Close_On_Exec;

end POSIX_IO;
