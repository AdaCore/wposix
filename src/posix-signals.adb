------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Win32;
with Win32.Winbase;

with POSIX_Win32;

package body POSIX.Signals is

   ---------------------
   -- Add_All_Signals --
   ---------------------

   procedure Add_All_Signals (Set : in out Signal_Set) is
      pragma Warnings (Off, Set);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Add_All_Signals");
   end Add_All_Signals;

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal (Set : in out Signal_Set; Sig : Signal) is
      pragma Warnings (Off, Set);
      pragma Warnings (Off, Sig);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Add_Signal");
   end Add_Signal;

   -------------------
   -- Block_Signals --
   -------------------

   procedure Block_Signals
     (Mask_To_Add :        Signal_Set;
      Old_Mask    :    out Signal_Set)
   is
      pragma Warnings (Off, Mask_To_Add);

      Dummy_Signal_Set : Signal_Set;
   begin
      Old_Mask := Dummy_Signal_Set;
      POSIX_Win32.Raise_Not_Yet_Implemented ("Block_Signals");
   end Block_Signals;

   ---------------------
   -- Blocked_Signals --
   ---------------------

   function Blocked_Signals return Signal_Set is
      Sig_Set : Signal_Set;
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Blocked_Signals");
      return Sig_Set;
   end Blocked_Signals;

   ------------------------
   -- Delete_All_Signals --
   ------------------------

   procedure Delete_All_Signals (Set : in out Signal_Set) is
      pragma Warnings (Off, Set);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Delete_All_Signals");
   end Delete_All_Signals;

   -------------------
   -- Delete_Signal --
   -------------------

   procedure Delete_Signal (Set : in out Signal_Set; Sig : Signal) is
      pragma Warnings (Off, Set);
      pragma Warnings (Off, Sig);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Delete_Signal");
   end Delete_Signal;

   -------------------
   -- Ignore_Signal --
   -------------------

   procedure Ignore_Signal (Sig : Signal) is
      pragma Warnings (Off, Sig);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Ignore_Signal");
   end Ignore_Signal;

   -----------
   -- Image --
   -----------

   function Image (Sig : Signal) return String is
   begin
      if Sig = Signal_Abort then
         return "SIGNAL_ABORT";
      elsif Sig = Signal_Floating_Point_Error then
         return "SIGNAL_FLOATING_POINT_ERROR";
      elsif Sig = Signal_Illegal_Instruction then
         return "SIGNAL_ILLEGAL_INSTRUCTION";
      elsif Sig = Signal_Interrupt then
         return "SIGNAL_INTERRUPT";
      elsif Sig = Signal_Segmentation_Violation then
         return "SIGNAL_SEGMENTATION_VIOLATION";
      elsif Sig = Signal_Terminate then
         return "SIGNAL_TERMINATE";
      else
         return "<unknown>";
      end if;
   end Image;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Sig : Signal) return Boolean is
      pragma Warnings (Off, Sig);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Is_Ignored");
      return False;
   end Is_Ignored;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member (Set : Signal_Set; Sig : Signal) return Boolean is
      pragma Warnings (Off, Set);
      pragma Warnings (Off, Sig);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Is_Member");
      return False;
   end Is_Member;

   ---------------------
   -- Pending_Signals --
   ---------------------

   function Pending_Signals return Signal_Set is
      Sig_Set : Signal_Set;
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Pending_Signals");
      return Sig_Set;
   end Pending_Signals;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Process : POSIX.Process_Identification.Process_ID;
      Sig     : Signal)
   is
      Result : Win32.BOOL;
      pragma Unreferenced (Result);
   begin
      if Sig = Signal_Terminate then
         Result := Win32.Winbase.TerminateProcess
           (POSIX_Win32.Get_Process_Handle (Process),
            Win32.UINT (Signal_Terminate));
      else
         POSIX_Win32.Raise_Not_Yet_Implemented ("Send_Signal " & Image (Sig));
      end if;
   end Send_Signal;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Process : POSIX.Process_Identification.Process_Group_ID;
      Sig     : Signal)
   is
      pragma Warnings (Off, Process);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Send_Signal " & Image (Sig));
   end Send_Signal;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal (Sig : Signal) is
      Result : Win32.BOOL;
      pragma Unreferenced (Result);
   begin
      if Sig = Signal_Terminate then
         Result := Win32.Winbase.TerminateProcess
           (POSIX_Win32.Get_Process_Handle
              (POSIX.Process_Identification.Get_Process_ID),
            Win32.UINT (Signal_Terminate));
      else
         POSIX_Win32.Raise_Not_Yet_Implemented ("Send_Signal " & Image (Sig));
      end if;
   end Send_Signal;

   --  Blocking and Unblocking Signals

   -------------------------
   -- Set_Blocked_Signals --
   -------------------------

   procedure Set_Blocked_Signals
     (New_Mask :        Signal_Set;
      Old_Mask :    out Signal_Set)
   is
      pragma Warnings (Off, New_Mask);

      Dummy_Signal_Set : Signal_Set;
   begin
      Old_Mask := Dummy_Signal_Set;
      POSIX_Win32.Raise_Not_Yet_Implemented ("Set_Blocked_Signals");
   end Set_Blocked_Signals;

   ------------------------------
   -- Set_Stopped_Child_Signal --
   ------------------------------

   procedure Set_Stopped_Child_Signal (Enable : Boolean := True) is
      pragma Warnings (Off, Enable);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Set_Stopped_Child_Signal");
   end Set_Stopped_Child_Signal;

   ----------------------------------
   -- Stopped_Child_Signal_Enabled --
   ----------------------------------

   function Stopped_Child_Signal_Enabled return Boolean is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Stopped_Child_Signal_Enabled");
      return True;
   end Stopped_Child_Signal_Enabled;

   ---------------------
   -- Unblock_Signals --
   ---------------------

   procedure Unblock_Signals
     (Mask_To_Substract :        Signal_Set;
      Old_Mask          :    out Signal_Set)
   is
      pragma Warnings (Off, Mask_To_Substract);

      Dummy_Signal_Set : Signal_Set;
   begin
      Old_Mask := Dummy_Signal_Set;
      POSIX_Win32.Raise_Not_Yet_Implemented ("Unblock_Signals");
   end Unblock_Signals;

   ---------------------
   -- Unignore_Signal --
   ---------------------

   procedure Unignore_Signal (Sig : Signal) is
      pragma Warnings (Off, Sig);
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Unignore_Signal");
   end Unignore_Signal;

   -----------
   -- Value --
   -----------

   function Value (Str : String) return Signal is
   begin
      if Str = "SIGNAL_ABORT" then
         return Signal_Abort;
      elsif Str = "SIGNAL_FLOATING_POINT_ERROR" then
         return Signal_Floating_Point_Error;
      elsif Str = "SIGNAL_ILLEGAL_INSTRUCTION" then
         return Signal_Illegal_Instruction;
      elsif Str = "SIGNAL_INTERRUPT" then
         return Signal_Interrupt;
      elsif Str = "SIGNAL_SEGMENTATION_VIOLATION" then
         return Signal_Segmentation_Violation;
      elsif Str = "SIGNAL_TERMINATE" then
         return Signal_Terminate;
      else
         return Signal_Null;
      end if;
   end Value;

end POSIX.Signals;
