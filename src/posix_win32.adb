
--  $Id$

with Ada.Exceptions;
with Interfaces.C.Strings;

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
      if RETCODE = Retcode_Error then
         POSIX.Set_Error_Code (POSIX.Error_Code (Win32.Winbase.getLastError));
         Ada.Exceptions.Raise_Exception
           (POSIX.POSIX_Error'Identity,
            Message => Fct &
            " : errno = " & Win32.DWORD'Image (Win32.Winbase.GetLastError));
      end if;
   end Check_Retcode;

   procedure Check_Result (RETCODE : in Win32.BOOL;
                           Fct     : in String)
   is
      use type Win32.BOOL;
   begin
      if RETCODE = Win32.False then
         POSIX.Set_Error_Code (POSIX.Error_Code (Win32.Winbase.GetLastError));
         Ada.Exceptions.Raise_Exception
           (POSIX.POSIX_Error'Identity,
            Message => Fct &
            " : errno = " & Win32.DWORD'Image (Win32.Winbase.GetLastError));
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



   BinaryType : aliased Win32.DWORD;

   function Is_Executable (Pathname : in POSIX.POSIX_String)
                           return Boolean
   is
   begin
      if Pathname'Length > 4 then
         declare
            Ext : constant String
              := POSIX.To_String
              (Pathname (Pathname'Last - 3 .. Pathname'Last));
         begin
            if Ext = ".com" then
               return True;
            elsif Ext = ".exe" then
               declare
                  use type Win32.BOOL;
                  L_Pathname      : constant String
                    := POSIX.To_String (Pathname) & ASCII.Nul;
               begin
                  return Win32.Winbase.GetBinaryType
                    (Win32.Addr (L_Pathname),
                     BinaryType'Access)        = Win32.True;
               end;
            elsif Ext =".bat" then
               return True;
            else
               return False;
            end if;
         end;
      else
         return False;
      end if;
   end Is_Executable;

end POSIX_Win32;
