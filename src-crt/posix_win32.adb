
--  $Id$

with Ada.Exceptions;
with Interfaces.C.Strings;
with Win32.Crt.Errno;
with Win32.Crt.Stat;

with POSIX;

package body POSIX_Win32 is

   package CS renames Interfaces.C.Strings;

   procedure Raise_Not_Yet_Implemented (Message : in String) is
   begin
      Ada.Exceptions.Raise_Exception
        (POSIX_Not_Yet_Implemented'Identity,
         Message => Message);
   end Raise_Not_Yet_Implemented;

   procedure Check_Retcode (RETCODE : in Win32.INT;
                            Fct     : in String)
   is
      use type Win32.INT;
   begin
      if RETCODE = -1 then
         POSIX.Set_Error_Code (POSIX.Error_Code (Win32.Crt.Errno.Errno));
         Ada.Exceptions.Raise_Exception
           (POSIX.POSIX_Error'Identity,
            Message => Fct &
            " : errno = " & Win32.INT'Image (Win32.Crt.Errno.Errno));
      end if;
   end Check_Retcode;

   procedure Check_Result (RETCODE : in Win32.BOOL;
                           Fct     : in String)
   is
      use type Win32.BOOL;
   begin
      if RETCODE = Win32.False then
         POSIX.Set_Error_Code (POSIX.Error_Code (Win32.Crt.Errno.Errno));
         Ada.Exceptions.Raise_Exception
           (POSIX.POSIX_Error'Identity,
            Message => Fct &
            " : errno = " & Win32.INT'Image (Win32.Crt.Errno.Errno));
      end if;
   end Check_Result;

   procedure Raise_Error (Message    : in String;
                          Error_Code : in POSIX.Error_Code) is
   begin
      POSIX.Set_Error_Code (Error_Code);
      Ada.Exceptions.Raise_Exception
        (POSIX.POSIX_Error'Identity,
         Message => Message &
         " : Error_Code = " & POSIX.Error_Code'Image (Error_Code));
   end Raise_Error;

   --  Permissions Set

   use type Win32.UINT;
   use POSIX_Permissions;

   procedure Set_Read (P  : in out Permission_Set) is
   begin
      P (Owner_Read)  := True;
      P (Group_Read)  := True;
      P (Others_Read) := True;
   end Set_Read;

   procedure Set_Write (P  : in out Permission_Set) is
   begin
      P (Owner_Write)  := True;
   end Set_Write;

   function To_Set (Mask : in Win32.UINT)
                    return Permission_Set
   is
      PS : Permission_Set := (others => False);
   begin
      if (Mask and Win32.Crt.Stat.S_IREAD) /= 0 then
         Set_Read (PS);
      end if;
      if (Mask and Win32.Crt.Stat.S_IWRITE) /= 0 then
         Set_Write (PS);
      end if;
      return PS;
   end To_Set;

   function To_Mask (Set : in Permission_Set)
                     return Win32.UINT
   is
      Mask : Win32.UINT := 0;
   begin
      if Set (Owner_Read) then
         Mask := Mask or Win32.Crt.Stat.S_IREAD;
      end if;
      if Set (Owner_Write) then
         Mask := Mask or Win32.Crt.Stat.S_IWRITE;
      end if;
      return Mask;
   end To_Mask;

   function Chars_Ptr_To_String (V : in Win32.PSTR)
                                 return String is
   begin
      return CS.Value (Win32.To_Chars_Ptr (V));
   end Chars_Ptr_To_String;

   function Is_Executable (Pathname : in POSIX.POSIX_String)
                           return Boolean
   is
      Ext : constant String
        := POSIX.To_String (Pathname (Pathname'Last - 3 .. Pathname'Last));
   begin
      if Ext = ".com" then
         return True;
      elsif Ext = ".exe" then
         return True;
      elsif Ext =".bat" then
         return True;
      else
         return False;
      end if;
   end Is_Executable;

end POSIX_Win32;
