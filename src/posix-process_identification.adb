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

with Ada.Streams;
with Ada.Strings.Fixed;
with Interfaces.C;

with POSIX_Win32;
with Win32.Winnt;
with Win32.Winternl;

package body POSIX.Process_Identification is

   use Ada.Streams;

   --  Process Identification

   Users_GID : constant Group_ID := To_Bounded_String (POSIX_Win32.Users_SID);

   --------------------------
   -- Create_Process_Group --
   --------------------------

   procedure Create_Process_Group
     (Process       :        Process_ID;
      Process_Group :    out Process_Group_ID)
   is
      pragma Warnings (Off, Process);
   begin
      Process_Group := Process_Group_ID (Users_GID);
   end Create_Process_Group;

   --------------------
   -- Create_Session --
   --------------------

   procedure Create_Session (Session_Leader : out Process_Group_ID) is
   begin
      Session_Leader := Process_Group_ID (Users_GID);
   end Create_Session;

   ----------------------------
   -- Get_Effective_Group_ID --
   ----------------------------

   function Get_Effective_Group_ID return Group_ID is
   begin
      return Get_Real_Group_ID;
   end Get_Effective_Group_ID;

   ---------------------------
   -- Get_Effective_User_ID --
   ---------------------------

   function Get_Effective_User_ID return User_ID is
   begin
      return Get_Real_User_ID;
   end Get_Effective_User_ID;

   ----------------
   -- Get_Groups --
   ----------------

   function Get_Groups return Group_List is
   begin
      return Group_List'(1 .. 0 => Users_GID);
   end Get_Groups;

   --------------------
   -- Get_Login_Name --
   --------------------

   function Get_Login_Name return POSIX_String is
      Buffer : String (1 .. 500);
      pragma Warnings (Off, Buffer);

      Size   : aliased Win32.DWORD := 500;
      Result : Win32.BOOL;
   begin
      Result := Win32.Winbase.GetUserName
        (Win32.Addr (Buffer), Size'Unchecked_Access);

      POSIX_Win32.Check_Result (Result, "Get_Login_Name");
      return POSIX.To_POSIX_String (Buffer (1 .. Positive (Size) - 1));
   end Get_Login_Name;

   ---------------------------
   -- Get_Parent_Process_ID --
   ---------------------------

   function Get_Parent_Process_ID return Process_ID is
      use type Win32.DWORD;
      pragma Warnings (Off);
      Pib  : aliased Win32.Winternl.PROCESS_BASIC_INFORMATION;
      Proc : Win32.Winnt.HANDLE;
      Len  : aliased Win32.DWORD;
      Ret  : Win32.DWORD;
   begin
      Proc := Win32.Winbase.GetCurrentProcess;
      Ret := Win32.Winternl.NtQueryInformationProcess
        (Proc, Win32.Winternl.ProcessBasicInformation,
         Pib'Access, Pib'Size / 8, Len'Access);

      POSIX_Win32.Check_Retcode
        (Ret, "Get_Parent_Process_ID.NtQueryInformationProcess");

      return Process_ID'
        (POSIX_Win32.Null_Handle, POSIX_Win32.Null_Handle,
         Interfaces.C.unsigned_long (Pib.InheritedFromUniqueProcessId), 0);
   end Get_Parent_Process_ID;

   --------------------------
   -- Get_Process_Group_ID --
   --------------------------

   function Get_Process_Group_ID return Process_Group_ID is
   begin
      return Process_Group_ID (Get_Real_Group_ID);
   end Get_Process_Group_ID;

   --------------------
   -- Get_Process_ID --
   --------------------

   function Get_Process_ID return Process_ID is
   begin
      return Process_ID'
        (POSIX_Win32.Null_Handle, POSIX_Win32.Null_Handle,
         Win32.Winbase.GetCurrentProcessId, 0);
   end Get_Process_ID;

   -----------------------
   -- Get_Real_Group_ID --
   -----------------------

   function Get_Real_Group_ID return Group_ID is
      Proc  : Win32.Winnt.HANDLE;
      Token : aliased Win32.Winnt.HANDLE;
      Res   : Win32.BOOL;
      pragma Warnings (Off, Res);
   begin
      --  The current process

      Proc := Win32.Winbase.GetCurrentProcess;

      --  Process's information token

      Res := Win32.Winbase.OpenProcessToken
        (Proc, Win32.Winnt.TOKEN_QUERY, Token'Unchecked_Access);

      POSIX_Win32.Check_Result (Res, "Get_Real_Group_ID.OpenProcessToken");

      --  The process information

      declare
         Buffer_Len : constant := 256;
         Buffer     : Stream_Element_Array (1 .. 256);
         Group      : Win32.Winnt.TOKEN_PRIMARY_GROUP;
         for Group'Address use Buffer'Address;
         Len        : aliased Win32.DWORD;
      begin
         Res := Win32.Winbase.GetTokenInformation
           (TokenHandle            => Token,
            TokenInformationClass  => Win32.Winnt.TokenPrimaryGroup,
            TokenInformation       => Group'Address,
            TokenInformationLength => Buffer_Len,
            ReturnLength           => Len'Access);

         POSIX_Win32.Check_Result
           (Res, "Get_Real_Group_ID.GetTokenInformation");

         Res := Win32.Winbase.CloseHandle (Token);
         return Value (POSIX_Win32.To_String (Group.PrimaryGroup));
      end;
   end Get_Real_Group_ID;

   ----------------------
   -- Get_Real_User_ID --
   ----------------------

   function Get_Real_User_ID return User_ID is
      Proc  : Win32.Winnt.HANDLE;
      Token : aliased Win32.Winnt.HANDLE;
      Res   : Win32.BOOL;
      pragma Warnings (Off, Res);
   begin
      --  The current process

      Proc := Win32.Winbase.GetCurrentProcess;

      --  Process's information token

      Res := Win32.Winbase.OpenProcessToken
        (Proc, Win32.Winnt.TOKEN_QUERY, Token'Unchecked_Access);
      POSIX_Win32.Check_Result (Res, "Get_Real_User_ID.OpenProcessToken");

      --  The process information

      declare
         Buffer_Len : constant := 256;
         Buffer     : Stream_Element_Array (1 .. 256);
         User       : Win32.Winnt.TOKEN_USER;
         for User'Address use Buffer'Address;
         Len        : aliased Win32.DWORD;
      begin
         Res := Win32.Winbase.GetTokenInformation
           (TokenHandle            => Token,
            TokenInformationClass  => Win32.Winnt.TokenUser,
            TokenInformation       => User'Address,
            TokenInformationLength => Buffer_Len,
            ReturnLength           => Len'Access);

         POSIX_Win32.Check_Result
           (Res, "Get_Real_User_ID.GetTokenInformation");

         Res := Win32.Winbase.CloseHandle (Token);

         return Value (POSIX_Win32.To_String (User.User.Sid));
      end;
   end Get_Real_User_ID;

   -----------
   -- Image --
   -----------

   function Image (ID : Process_ID) return String is
      use Ada;
   begin
      return Strings.Fixed.Trim
        (Win32.DWORD'Image (ID.dwProcessId), Strings.Left);
   end Image;

   function Image (ID : Process_Group_ID) return String is
      use Ada;
   begin
      return To_String (ID);
   end Image;

   function Image (ID : Group_ID) return String is
   begin
      return To_String (ID);
   end Image;

   function Image (ID : User_ID) return String is
      use Ada;
   begin
      return To_String (ID);
   end Image;

   ------------------
   -- Set_Group_ID --
   ------------------

   procedure Set_Group_ID (ID : Group_ID) is
      pragma Warnings (Off, ID);
   begin
      null;
   end Set_Group_ID;

   --------------------------
   -- Set_Process_Group_ID --
   --------------------------

   procedure Set_Process_Group_ID
     (Process       : Process_ID       := Get_Process_ID;
      Process_Group : Process_Group_ID := Get_Process_Group_ID)
   is
      pragma Warnings (Off, Process);
      pragma Warnings (Off, Process_Group);
   begin
      null;
   end Set_Process_Group_ID;

   -----------------
   -- Set_User_ID --
   -----------------

   procedure Set_User_ID (ID : User_ID) is
      pragma Warnings (Off, ID);
   begin
      null;
   end Set_User_ID;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return Group_ID is
   begin
      return To_Bounded_String (Str);
   end Value;

   function Value (Str : String) return Process_Group_ID is
   begin
      return To_Bounded_String (Str);
   end Value;

   function Value (Str : String) return Process_ID is
   begin
      return Process_ID'
        (POSIX_Win32.Null_Handle, POSIX_Win32.Null_Handle,
         Win32.DWORD'Value (Str), 0);
   end Value;

   function Value (Str : String) return User_ID is
   begin
      return To_Bounded_String (Str);
   end Value;

end POSIX.Process_Identification;
