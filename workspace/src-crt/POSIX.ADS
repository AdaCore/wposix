
--  $Id$

with Win32;
with Win32.Crt.Errno;

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

   System_POSIX_Version        : constant := 1990_09;
   POSIX_Ada_Version           : constant := 1996_07;



   --  I/O Count

   type IO_Count is new Natural range 0 .. Natural'last;
   subtype IO_count_Maximum is IO_Count range 32767 .. IO_Count'last;



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


   --  Characters and String

   --  *** Implementation Defined ***
   --  the following Characters are implementation defined :
   --  '!', '$', '%', '?', '[', ']', '^', '`', '{', '}', '|'


   type POSIX_Character is (' ', '!', '"', '#',   '$', '%', '&', ''', --  "
                            '(', ')', '*', '+',   ',', '-', '.', '/',
                            '0', '1', '2', '3',   '4', '5', '6', '7',
                            '8', '9', ':', ';',   '<', '=', '>', '?',

                            '@', 'A', 'B', 'C',   'D', 'E', 'F', 'G',
                            'H', 'I', 'J', 'K',   'L', 'M', 'N', 'O',
                            'P', 'Q', 'R', 'S',   'T', 'U', 'V', 'W',
                            'X', 'Y', 'Z', '[',   '\', ']', '^', '_',

                            '`', 'a', 'b', 'c',   'd', 'e', 'f', 'g',
                            'h', 'i', 'j', 'k',   'l', 'm', 'n', 'o',
                            'p', 'q', 'r', 's',   't', 'u', 'v', 'w',
                            'x', 'y', 'z', '{',   '|', '}' );

   for POSIX_Character use ( 32,  33,  34,  35,    36,  37,  38,  39,
                             40,  41,  42,  43,    44,  45,  46,  47,
                             48,  49,  50,  51,    52,  53,  54,  55,
                             56,  57,  58,  59,    60,  61,  62,  63,

                             64,  65,  66,  67,    68,  69,  70,  71,
                             72,  73,  74,  75,    76,  77,  78,  79,
                             80,  81,  82,  83,    84,  85,  86,  87,
                             88,  89,  90,  91,    92,  93,  94,  95,

                             96,  97,  98,  99,   100, 101, 102, 103,
                             104, 105, 106, 107,  108, 109, 110, 111,
                             112, 113, 114, 115,  116, 117, 118, 119,
                             120, 121, 122, 123,  124, 125 );

   type POSIX_String is array (Positive range <>) of POSIX_Character;

   function To_POSIX_String (Str : string)
                             return POSIX_String;

   function To_String (Str : POSIX_String)
                       return string;

   subtype Filename is POSIX_String;
   subtype Pathname is POSIX_String;

   function Is_Filename (Str : POSIX_String)
                         return Boolean;
   function Is_Pathname (Str : POSIX_String)
                         return Boolean;

   function Is_Portable_Filename (Str : POSIX_String)
                                  return Boolean;
   function Is_Portable_Pathname (Str : POSIX_String)
                                  return Boolean;



   --  String Lists

   type POSIX_String_List is limited private;
   Empty_String_List : constant POSIX_String_List;

   procedure Make_Empty (List : in out POSIX_String_List);

   procedure Append  (List : in out POSIX_String_List;
                      Str  : in     POSIX_String);

   generic
      with procedure Action
        (Item : in      POSIX_String;
         Quit : in out Boolean);
   procedure For_Every_Item (List : in POSIX_String_List);

   function Length (List : POSIX_String_List)
                    return Natural;

   function Value (List  : POSIX_String_List;
                   Index : Positive)
                   return POSIX_String;



   --  Option Sets

   type Option_Set is private;

   function Empty_Set
     return Option_Set;

   function "+" (L, R : Option_Set)
                 return Option_Set;

   function "-" (L, R : Option_Set)
                 return Option_Set;



   --  Exceptions and error codes

   POSIX_Error : exception;

   type Error_Code is new Natural;

   function Get_Error_Code
     return Error_Code;

   procedure Set_Error_Code (Error : in Error_Code);

   function Is_POSIX_Error (Error : Error_Code)
                            return Boolean;

   function Image (Error : Error_Code)
                   return String;

   No_Error                            : constant Error_Code
     := 0;

   Argument_List_Too_Long              : constant Error_Code
     := Win32.Crt.Errno.E2BIG;

   Bad_File_Descriptor                 : constant Error_Code
     := Win32.Crt.Errno.EBADF;

   Broken_Pipe                         : constant Error_Code
     := Win32.Crt.Errno.EPIPE;

   Directory_Not_Empty                 : constant Error_Code
     := Win32.Crt.Errno.ENOTEMPTY;

   Exec_Format_Error                   : constant Error_Code
     := Win32.Crt.Errno.ENOEXEC;

   File_Exists                         : constant Error_Code
     := Win32.Crt.Errno.EEXIST;

   File_Too_Large                      : constant Error_Code
     := Win32.Crt.Errno.EFBIG;

   Filename_Too_Long                   : constant Error_Code
     := 1001;

   Improper_Link                       : constant Error_Code
     := 1002;

   Inappropriate_IO_Control_Operation  : constant Error_Code
     := 1003;

   Input_Output_Error                  : constant Error_Code
     := Win32.Crt.Errno.EIO;

   Interrupted_Operation               : constant Error_Code
     := Win32.Crt.Errno.EINTR;

   Invalid_Argument                    : constant Error_Code
     := Win32.Crt.Errno.EINVAL;

   Invalid_Seek                        : constant Error_Code
     := Win32.Crt.Errno.ESPIPE;

   Is_A_Directory                      : constant Error_Code
     := Win32.Crt.Errno.EISDIR;

   No_Child_Process                    : constant Error_Code
     := Win32.Crt.Errno.ECHILD;

   No_Locks_Available                  : constant Error_Code
     := Win32.Crt.Errno.ENOLCK;

   No_Space_Left_On_Device             : constant Error_Code
     := Win32.Crt.Errno.ENOSPC;

   No_Such_Operation_On_Device         : constant Error_Code
     := Win32.Crt.Errno.ENODEV;

   No_Such_Device_Or_Address           : constant Error_Code
     := Win32.Crt.Errno.ENXIO;

   No_Such_File_Or_Directory           : constant Error_Code
     := Win32.Crt.Errno.ENOENT;

   No_Such_Process                     : constant Error_Code
     := Win32.Crt.Errno.ESRCH;

   Not_A_Directory                     : constant Error_Code
     := Win32.Crt.Errno.ENOTDIR;

   Not_Enough_Space                    : constant Error_Code
     := Win32.Crt.Errno.ENOSPC;

   Operation_Not_Implemented           : constant Error_Code
     := 1005;

   Operation_Not_Permited              : constant Error_Code
     := Win32.Crt.Errno.EPERM;

   Permission_Denied                   : constant Error_Code
     := Win32.Crt.Errno.EACCES;

   Read_Only_File_System               : constant Error_Code
     := Win32.Crt.Errno.EROFS;

   Resource_Busy                       : constant Error_Code
     := Win32.Crt.Errno.EBUSY;

   Resource_Deadlock_Avoided           : constant Error_Code
     := Win32.Crt.Errno.EDEADLOCK;

   Resource_Temporarily_Unavailable    : constant Error_Code
     := Win32.Crt.Errno.EAGAIN;

   Too_Many_Links                      : constant Error_Code
     := Win32.Crt.Errno.EMLINK;

   Too_Many_Open_Files                 : constant Error_Code
     := Win32.Crt.Errno.EMFILE;

   Too_Many_Open_Files_In_System       : constant Error_Code
     := Win32.Crt.Errno.ENFILE;




     --  System Identification

   function System_Name
     return POSIX_String;

   function Node_Name
     return POSIX_String;

   function Release
     return POSIX_String;

   function Version
     return POSIX_String;

   function Machine
     return POSIX_String;

private

   type POSIX_String_Ptr is access POSIX_String;
   type String_Ptr_Array is array (positive range <>) of POSIX_String_Ptr;
   type String_Ptr_Array_Ptr is access String_Ptr_Array;

   type POSIX_String_List is
      record
         last    : natural := 0;
         length  : natural := 0;
         strings : String_Ptr_Array_Ptr := NULL;
      end record;

   Empty_String_List : constant POSIX_String_List
     := POSIX_String_List'(0, 0, NULL);

   type Option_Set is new Win32.UINT;

end POSIX;
