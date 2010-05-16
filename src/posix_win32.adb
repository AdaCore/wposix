------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;

with Win32.Winbase;
with Win32.Winerror;

package body POSIX_Win32 is

   ---------------------------------------
   -- Process_ID_To_PROCESS_INFORMATION --
   ---------------------------------------

   function Process_ID_To_PROCESS_INFORMATION is new Ada.Unchecked_Conversion
     (PPI.Process_ID, Win32.Winbase.PROCESS_INFORMATION);

   type P_List;
   type P_List_Access is access P_List;

   type P_List is record
      Process : PPI.Process_ID;
      Next    : P_List_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (P_List, P_List_Access);

   ------------------
   -- Process_List --
   ------------------

   protected Process_List is

      procedure Add    (Child  : PPI.Process_ID);

      procedure Remove (Child  : PPI.Process_ID);

      function  Exist  (Child  : PPI.Process_ID) return Boolean;

      procedure Wait
        (Status :    out PPP.Termination_Status;
         Block  :        Boolean);

   private
      Process   : P_List_Access := null;
      N_Process : Natural := 0;
   end Process_List;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child (Child : PPI.Process_ID) is
   begin
      Process_List.Add (Child);
   end Add_Child;

   ------------------
   -- Check_Result --
   ------------------

   procedure Check_Result
     (RETCODE : Win32.BOOL;
      Fct     : String)
   is
      use type Win32.BOOL;
   begin
      if RETCODE = Win32.FALSE then
         declare
            Code : constant Win32.DWORD := Win32.Winbase.GetLastError;
         begin
            POSIX.Set_Error_Code (POSIX.Error_Code (Code));
            Ada.Exceptions.Raise_Exception
              (POSIX.POSIX_Error'Identity,
               Message => Fct &
               " : errno = " & Win32.DWORD'Image (Code));
         end;
      end if;
   end Check_Result;

   -------------------
   -- Check_Retcode --
   -------------------

   procedure Check_Retcode
     (RETCODE : Win32.DWORD;
      Fct     : String) is
   begin
      if RETCODE /= Win32.Winerror.ERROR_SUCCESS then
         POSIX.Set_Error_Code (POSIX.Error_Code (RETCODE));
         Ada.Exceptions.Raise_Exception
           (POSIX.POSIX_Error'Identity,
            Message => Fct & " : errno = " & Win32.DWORD'Image (RETCODE));
      end if;
   end Check_Retcode;

   -----------
   -- Exist --
   -----------

   function  Exist (Child : PPI.Process_ID) return Boolean is
   begin
      return Process_List.Exist (Child);
   end Exist;

   ------------------------
   -- Get_Process_Handle --
   ------------------------

   function Get_Process_Handle
     (Process : PPI.Process_ID) return Win32.Winnt.HANDLE
   is
      function To_Process_Information is new Ada.Unchecked_Conversion
        (PPI.Process_ID, Win32.Winbase.PROCESS_INFORMATION);
   begin
      return To_Process_Information (Process).hProcess;
   end Get_Process_Handle;

   -------------------
   -- Is_Executable --
   -------------------

   function Is_Executable (Pathname : POSIX.POSIX_String) return Boolean is
      BinaryType : aliased Win32.DWORD;
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
                    := POSIX.To_String (Pathname) & ASCII.NUL;
               begin
                  return Win32.Winbase.GetBinaryType
                    (Win32.Addr (L_Pathname),
                     BinaryType'Unchecked_Access) = Win32.TRUE;
               end;

            elsif Ext = ".bat" then
               return True;

            else
               return False;
            end if;
         end;
      else
         return False;
      end if;
   end Is_Executable;

   ------------------
   -- Process_List --
   ------------------

   protected body Process_List is

      ---------
      -- Add --
      ---------

      procedure Add    (Child : PPI.Process_ID) is
      begin
         Process := new P_List'(Child, Next => Process);
         N_Process := N_Process + 1;
      end Add;

      -----------
      -- Exist --
      -----------

      function  Exist  (Child : PPI.Process_ID) return Boolean is
         use type PPI.Process_ID;
         PLa : P_List_Access := Process;
      begin
         Check_Child :
         while PLa /= null loop
            if PLa.Process = Child then
               return True;
            else
               PLa := PLa.Next;
            end if;
         end loop Check_Child;

         return False;
      end Exist;

      --------------------
      -- Get_Process_ID --
      --------------------

      function Get_Process_ID (H : Win32.Winnt.HANDLE) return PPI.Process_ID is
         use type Win32.Winnt.HANDLE;
         PLa : P_List_Access;
      begin
         PLa := Process;
         while PLa /= null loop
            if Process_ID_To_PROCESS_INFORMATION
              (PLa.Process).hProcess = H
            then
               return PLa.Process;
            else
               PLa := PLa.Next;
            end if;
         end loop;
         return PPI.Null_Process_ID;
      end Get_Process_ID;

      ------------
      -- Remove --
      ------------

      procedure Remove (Child  : PPI.Process_ID) is
         use type PPI.Process_ID;
         PLa, PLa_Prev : P_List_Access;
         pragma Warnings (Off, PLa_Prev);
      begin
         PLa := Process;

         Remove_Child :
         while PLa /= null loop

            if PLa.Process = Child then

               if PLa = Process then
                  Process := PLa.Next;
               else
                  PLa_Prev.Next := PLa.Next;
               end if;

               Free (PLa);
               exit Remove_Child;

            else
               PLa_Prev := PLa;
               PLa := PLa.Next;
            end if;

         end loop Remove_Child;
         N_Process := N_Process - 1;
      end Remove;

      ----------
      -- Wait --
      ----------

      procedure Wait
        (Status :    out PPP.Termination_Status;
         Block  :        Boolean)
      is
         type Exit_Stat is mod 2 ** Integer'Size;

         type Termination_Status is record
            Pid         :  PPI.Process_ID := PPI.Null_Process_ID;
            Exit_Status :  Exit_Stat;
         end record;

         function Termination_Status_Conv is new Ada.Unchecked_Conversion
           (Termination_Status, PPP.Termination_Status);

         type Handle_Set is
           array (1 .. N_Process) of aliased Win32.Winnt.HANDLE;

         Handles : aliased Handle_Set;
         PLa     : P_List_Access := Process;

         Retcode   : Win32.DWORD;
         Ok        : Win32.BOOL;
         pragma Unreferenced (Ok);
         Exit_Code : aliased Win32.DWORD;
         H         : Win32.Winnt.HANDLE;
         Child     : PPI.Process_ID;

      begin
         if N_Process = 0 then
            Raise_Error ("No Child Process", POSIX.No_Child_Process);
         end if;

         for K in Handle_Set'Range loop
            Handles (K) :=
              Process_ID_To_PROCESS_INFORMATION (PLa.Process).hProcess;
            PLa := PLa.Next;
         end loop;

         if Block then
            Retcode := Win32.Winbase.WaitForMultipleObjects
              (Interfaces.C.unsigned_long (N_Process),
               Handles (1)'Unchecked_Access,
               Win32.FALSE,
               Win32.Winbase.INFINITE);
         else
            Retcode := Win32.Winbase.WaitForMultipleObjects
              (Interfaces.C.unsigned_long (N_Process),
               Handles (1)'Unchecked_Access,
               Win32.FALSE,
               0);

            if Retcode = Win32.Winbase.WAIT_TIMEOUT then
               Status := Termination_Status_Conv
                 (Termination_Status'(PPI.Null_Process_ID, 0));
               return;
            end if;
         end if;

         H := Handles (Integer (Retcode - Win32.Winbase.WAIT_OBJECT_0 + 1));

         Ok := Win32.Winbase.GetExitCodeProcess
           (H, Exit_Code'Unchecked_Access);

         Ok := Win32.Winbase.CloseHandle (H);

         Child := Get_Process_ID (H);

         Status := Termination_Status_Conv
           (Termination_Status'(Child, Exit_Stat (Exit_Code)));
         Remove (Child);
      end Wait;

   end Process_List;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error
     (Message    : String;
      Error_Code : POSIX.Error_Code) is
   begin
      POSIX.Set_Error_Code (Error_Code);
      Ada.Exceptions.Raise_Exception
        (POSIX.POSIX_Error'Identity,
         Message => Message &
         " : Error_Code = " & POSIX.Error_Code'Image (Error_Code));
   end Raise_Error;

   ----------------------
   -- Raise_Last_Error --
   ----------------------

   procedure Raise_Last_Error (Fct : String) is
   begin
      Raise_Error (Fct, POSIX.Error_Code (Win32.Winbase.GetLastError));
   end Raise_Last_Error;

   -------------------------------
   -- Raise_Not_Yet_Implemented --
   -------------------------------

   procedure Raise_Not_Yet_Implemented (Message : String) is
   begin
      Ada.Exceptions.Raise_Exception
        (POSIX_Not_Yet_Implemented'Identity,
         Message => Message);
   end Raise_Not_Yet_Implemented;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Child : PPI.Process_ID) is
   begin
      Process_List.Remove (Child);
   end Remove_Child;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Status :    out PPP.Termination_Status;
      Block  :        Boolean) is
   begin
      Process_List.Wait (Status, Block);
   end Wait;

end POSIX_Win32;
