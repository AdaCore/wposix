
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with Interfaces.C.Strings;

with POSIX;

package body POSIX_Win32 is

   package CS renames Interfaces.C.Strings;

   -------------------------------
   -- Raise_Not_Yet_Implemented --
   -------------------------------

   procedure Raise_Not_Yet_Implemented (Message : in String) is
   begin
      Ada.Exceptions.Raise_Exception
        (POSIX_Not_Yet_Implemented'Identity,
         Message => Message);
   end Raise_Not_Yet_Implemented;

   -------------------
   -- Check_Retcode --
   -------------------

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

   ------------------
   -- Check_Result --
   ------------------

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

   -----------------
   -- Raise_Error --
   -----------------

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

   -------------------
   -- Is_Executable --
   -------------------

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

   --  process list

   ---------------------------------------
   -- Process_ID_To_PROCESS_INFORMATION --
   ---------------------------------------

   function Process_ID_To_PROCESS_INFORMATION is
     new Ada.Unchecked_Conversion  (POSIX_Process_Identification.Process_ID,
                                    Win32.Winbase.PROCESS_INFORMATION);

   type P_List;
   type P_List_Access is access P_List;

   type P_List is
      record
         Process : PPI.Process_ID;
         Next    : P_List_Access;
      end record;

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation (P_List, P_List_Access);

   ------------------
   -- Process_List --
   ------------------

   protected Process_List is

      procedure Add    (Child  : in     PPI.Process_ID);
      procedure Remove (Child  : in     PPI.Process_ID);
      function  Exist  (Child  : in     PPI.Process_ID) return Boolean;
      procedure Wait   (Status :    out PPP.Termination_Status;
                        Block  : in     Boolean);
   private
      Process   : P_List_Access := null;
      N_Process : Natural := 0;
   end Process_List;

   protected body Process_List is

      ---------
      -- Add --
      ---------

      procedure Add    (Child  : in     PPI.Process_ID) is
      begin
         Process := new P_List'(Child, Next => Process);
         N_Process := N_Process + 1;
      end Add;

      ------------
      -- Remove --
      ------------

      procedure Remove (Child  : in     PPI.Process_ID) is
         use type PPI.Process_ID;
         PLa, PLa_Prev : P_List_Access;
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

      -----------
      -- Exist --
      -----------

      function  Exist  (Child  : in     PPI.Process_ID) return Boolean is
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

      function Get_Process_ID (H : Win32.Winnt.HANDLE)
                               return PPI.Process_ID
      is
         use type Win32.Winnt.Handle;
         PLa, PLa_Prev : P_List_Access;
      begin
         PLa := Process;
         while PLa /= null loop
            if Process_ID_To_PROCESS_INFORMATION
                 (PLa.Process).hProcess = H
            then
               return PLa.Process;
            else
               PLa_Prev := PLa;
               PLa := PLa.Next;
            end if;
         end loop;
         return PPI.Null_Process_ID;
      end Get_Process_ID;

      ----------
      -- Wait --
      ----------

      procedure Wait   (Status :    out PPP.Termination_Status;
                        Block  : in     Boolean)
      is

         use type Interfaces.C.unsigned_long;

         type Exit_Stat is mod 2 ** Integer'Size;
         type Termination_Status is
            record
               Pid         :  PPI.Process_ID := PPI.Null_Process_ID;
               Exit_Status :  Exit_Stat;
            end record;

         function Termination_Status_Conv is
           new Ada.Unchecked_Conversion
           (Termination_Status, PPP.Termination_Status);

         type Handle_Set is array (1 .. N_Process) of
           aliased Win32.Winnt.HANDLE;

         Handles : aliased Handle_Set;
         PLa     : P_List_Access := Process;

         Retcode   : Win32.DWORD;
         Ok        : Win32.BOOL;
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

         Ok := Win32.Winbase.GetExitCodeProcess (H,
                                                 Exit_Code'Unchecked_Access);

         Ok := Win32.Winbase.CloseHandle (H);

         Child := Get_Process_ID (H);

         Status := Termination_Status_Conv
           (Termination_Status'(Child, Exit_Stat (Exit_Code)));
         Remove (Child);
      end Wait;

   end Process_List;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child    (Child  : in     PPI.Process_ID) is
   begin
      Process_List.Add (Child);
   end Add_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child (Child  : in     PPI.Process_ID) is
   begin
      Process_List.Remove (Child);
   end Remove_Child;

   -----------
   -- Exist --
   -----------

   function  Exist  (Child  : in     PPI.Process_ID) return Boolean is
   begin
      return Process_List.Exist (Child);
   end Exist;

   ----------
   -- Wait --
   ----------

   procedure Wait         (Status :    out PPP.Termination_Status;
                           Block  : in     Boolean) is
   begin
      Process_List.Wait (Status, Block);
   end Wait;

end POSIX_Win32;
