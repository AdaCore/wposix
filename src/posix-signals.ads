------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
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

with POSIX.Process_Identification;

package POSIX.Signals is

   --  Signals type

   type Signal is new Natural;

   function Image (Sig : in Signal) return String;

   function Value (Str : in String) return Signal;

   --  Standard Signals (required)

   Signal_Null, SIGNULL                           : constant Signal := 0;

   Signal_Abort, SIGABRT                          : constant Signal := 22;

   Signal_Floating_Point_Error, SIGFPE            : constant Signal := 8;

   Signal_Illegal_Instruction, SIGILL             : constant Signal := 4;

   Signal_Interrupt, SIGINT                       : constant Signal := 2;

   Signal_Segmentation_Violation, SIGSEGV         : constant Signal := 11;

   Signal_Terminate, SIGTERM                      : constant Signal := 15;

   --  type Signal Sets

   type Signal_Set is private;

   procedure Add_Signal (Set : in out Signal_Set; Sig : in Signal);

   procedure Add_All_Signals (Set : in out Signal_Set);

   procedure Delete_Signal (Set : in out Signal_Set; Sig : in Signal);

   procedure Delete_All_Signals (Set : in out Signal_Set);

   function Is_Member (Set : in Signal_Set; Sig : in Signal) return Boolean;

   --  Sending a Signal

   procedure Send_Signal
     (Process : in POSIX.Process_Identification.Process_ID;
      Sig     : in Signal);

   procedure Send_Signal
     (Process : in POSIX.Process_Identification.Process_Group_ID;
      Sig     : in Signal);

   procedure Send_Signal
     (Sig     : in Signal);

   --  Blocking and Unblocking Signals

   procedure Set_Blocked_Signals
     (New_Mask : in     Signal_Set;
      Old_Mask :    out Signal_Set);

   procedure Block_Signals
     (Mask_To_Add : in     Signal_Set;
      Old_Mask    :    out Signal_Set);

   procedure Unblock_Signals
     (Mask_To_Substract : in     Signal_Set;
      Old_Mask          :    out Signal_Set);

   function Blocked_Signals return Signal_Set;

   --  Ignoring Signals

   procedure Ignore_Signal (Sig : in Signal);

   procedure Unignore_Signal (Sig : in Signal);

   function Is_Ignored (Sig : in Signal) return Boolean;

   --  Controling Delivery of Signal_Child Signal

   procedure Set_Stopped_Child_Signal (Enable : in Boolean := True);

   function Stopped_Child_Signal_Enabled return Boolean;

   --  Examining Pending Signals

   function Pending_Signals return Signal_Set;

private

   type Signal_Set is record
      Values : Integer := 0;
   end record;

end POSIX.Signals;