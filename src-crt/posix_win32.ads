
--  $Id$

with System;
with Win32;
with Win32.Winbase;
with POSIX;
with POSIX_Permissions;

package POSIX_Win32 is

   POSIX_Not_Yet_Implemented : exception;

   procedure Raise_Not_Yet_Implemented (Message : in String);

   procedure Check_Retcode (RETCODE : in Win32.INT;
                            Fct     : in String);

   procedure Check_Result (RETCODE : in Win32.BOOL;
                           Fct     : in String);

   procedure Raise_Error (Message    : in String;
                          Error_Code : in POSIX.Error_Code);

   function To_Set (Mask : in Win32.UINT)
                    return POSIX_Permissions.Permission_Set;

   function To_Mask (Set : in POSIX_Permissions.Permission_Set)
                     return Win32.UINT;

   function Chars_Ptr_To_String (V : in Win32.PSTR)
                                 return String;

   function Is_Executable (Pathname : in POSIX.POSIX_String)
                           return Boolean;

   use type Win32.ULONG;

   Null_Security_Attributes : aliased Win32.Winbase.SECURITY_ATTRIBUTES :=
     (NLength              => Win32.Winbase.SECURITY_ATTRIBUTES'Size / 8,
      LpSecurityDescriptor => System.Null_Address,
      BInheritHandle       => 0);

   Null_Inherited_Security_Attributes :
     aliased Win32.Winbase.SECURITY_ATTRIBUTES :=
     (NLength              => Win32.Winbase.SECURITY_ATTRIBUTES'Size / 8,
      LpSecurityDescriptor => System.Null_Address,
      BInheritHandle       => Win32.TRUE);

end POSIX_Win32;
