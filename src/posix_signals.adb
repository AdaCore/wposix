
--  $Id$

with Ada.Unchecked_Conversion;

with Win32;
with Win32.Winbase;
with Win32.Winnt;
with POSIX_Win32;

package body POSIX_Signals is

   ---------------------------------------------
   -- Is_Valid_Signal_For_This_Implementation --
   ---------------------------------------------

   function Is_Valid_Signal_For_This_Implementation (Sig : Signal)
                                                     return Boolean is
   begin
      if Sig = Signal_Abort then
         return True;
      elsif Sig = Signal_Floating_Point_Error then
         return True;
      elsif Sig = Signal_Illegal_Instruction then
         return True;
      elsif Sig = Signal_Interrupt then
         return True;
      elsif Sig = Signal_Segmentation_Violation then
         return True;
      elsif Sig = Signal_Terminate then
         return True;
      else
         return False;
      end if;
   end Is_Valid_Signal_For_This_Implementation;


   -----------
   -- Image --
   -----------

   function Image (Sig : Signal)
                   return String is
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


   -----------
   -- Value --
   -----------

   function Value (Str : String)
                   return Signal is
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
      end if;
   end Value;




   --  type Signal Sets

   ----------------
   -- Add_Signal --
   ----------------

   procedure Add_Signal
     (Set : in out Signal_Set;
      Sig : in    Signal) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Add_Signal");
   end Add_Signal;



   ---------------------
   -- Add_All_Signals --
   ---------------------

   procedure Add_All_Signals (Set : in out Signal_Set) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Add_All_Signals");
   end Add_All_Signals;



   -------------------
   -- Delete_Signal --
   -------------------

   procedure Delete_Signal
     (Set : in out Signal_Set;
      Sig : in    Signal) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Delete_Signal");
   end Delete_Signal;


   ------------------------
   -- Delete_All_Signals --
   ------------------------

   procedure Delete_All_Signals (Set : in out Signal_Set) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Delete_All_Signals");
   end Delete_All_Signals;


   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (Set : Signal_Set;
      Sig : Signal)
      return Boolean is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Is_Member");
      return False;
   end Is_Member;


   --  Sending a Signal

   ------------------------
   -- Get_Process_Handle --
   ------------------------

   function Get_Process_Handle
     (Process : in POSIX_Process_Identification.Process_ID)
      return Win32.Winnt.HANDLE
   is
      function To_Process_Information is new
        Ada.Unchecked_Conversion (POSIX_Process_Identification.Process_ID,
                                  Win32.Winbase.PROCESS_INFORMATION);
   begin
      return To_Process_Information (Process).HProcess;
   end Get_Process_Handle;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Process : in POSIX_Process_Identification.Process_ID;
      Sig     : in Signal)
   is
      Result : Win32.BOOL;
   begin
      if Sig = Signal_Terminate then
         Result := Win32.Winbase.TerminateProcess
           (Get_Process_Handle (Process),
            Win32.UINT (Signal_Terminate));
      else
         POSIX_Win32.Raise_Not_Yet_Implemented ("Send_Signal " & Image (Sig));
      end if;
   end Send_Signal;


   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Process : in POSIX_Process_Identification.Process_Group_ID;
      Sig     : in Signal) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Send_Signal " & Image (Sig));
   end Send_Signal;


   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Sig     : in Signal)
   is
      Result : Win32.BOOL;
   begin
      if Sig = Signal_Terminate then
         Result := Win32.Winbase.TerminateProcess
           (Get_Process_Handle (POSIX_Process_Identification.Get_Process_ID),
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
     (New_Mask : in     Signal_Set;
      Old_Mask :    out Signal_Set) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Set_Blocked_Signals");
   end Set_Blocked_Signals;


   -------------------
   -- Block_Signals --
   -------------------

   procedure Block_Signals
     (Mask_To_Add : in     Signal_Set;
      Old_Mask    :    out Signal_Set) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Block_Signals");
   end Block_Signals;


   ---------------------
   -- Unblock_Signals --
   ---------------------

   procedure Unblock_Signals
     (Mask_To_Substract : in     Signal_Set;
      Old_Mask          :    out Signal_Set) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Unblock_Signals");
   end Unblock_Signals;


   ---------------------
   -- Blocked_Signals --
   ---------------------

   function Blocked_Signals
     return Signal_Set
   is
      Sig_Set : Signal_Set;
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Blocked_Signals");
      return Sig_Set;
   end Blocked_Signals;





   --  Ignoring Signals

   -------------------
   -- Ignore_Signal --
   -------------------

   procedure Ignore_Signal (Sig : in Signal) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Ignore_Signal");
   end Ignore_Signal;


   ---------------------
   -- Unignore_Signal --
   ---------------------

   procedure Unignore_Signal (Sig : in Signal) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Unignore_Signal");
   end Unignore_Signal;


   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Sig : Signal)
                        return Boolean is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Is_Ignored");
      return False;
   end Is_Ignored;



   --  Controling Delivery of Signal_Child Signal

   ------------------------------
   -- Set_Stopped_Child_Signal --
   ------------------------------

   procedure Set_Stopped_Child_Signal
     (Enable : in Boolean := True) is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Set_Stopped_Child_Signal");
   end Set_Stopped_Child_Signal;


   ----------------------------------
   -- Stopped_Child_Signal_Enabled --
   ----------------------------------

   function Stopped_Child_Signal_Enabled
     return Boolean is
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Stopped_Child_Signal_Enabled");
      return True;
   end Stopped_Child_Signal_Enabled;



   --  Examining Pending Signals

   ---------------------
   -- Pending_Signals --
   ---------------------

   function Pending_Signals
     return Signal_Set
   is
      Sig_Set : Signal_Set;
   begin
      POSIX_Win32.Raise_Not_Yet_Implemented ("Pending_Signals");
      return Sig_Set;
   end Pending_Signals;

end POSIX_Signals;
