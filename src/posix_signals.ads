
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with POSIX;
with POSIX_Process_Identification;

package POSIX_Signals is

   --  Signals type

   type Signal is new Natural;

   function Image (Sig : Signal)
                   return String;

   function Value (Str : String)
                   return Signal;


  --  Standard Signals (required)

   Signal_Null, SIGNULL                           : constant Signal
     := 0;

   Signal_Abort, SIGABRT                          : constant Signal
     := 22;

   Signal_Floating_Point_Error, SIGFPE            : constant Signal
     := 8;

   Signal_Illegal_Instruction, SIGILL             : constant Signal
     := 4;

   Signal_Interrupt, SIGINT                       : constant Signal
     := 2;

   Signal_Segmentation_Violation, SIGSEGV         : constant Signal
     := 11;

   Signal_Terminate, SIGTERM                      : constant Signal
     := 15;


  --  Signal Handler References

--   Signal_Abort_Ref                    : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Abort);


--     --  Signal_Alarm intentionally omitted.
--     --  Signal_Floating_Point_Error intentionally omitted.

--   Signal_Hangup_Ref                   : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Hangup);

--     --  Signal_Illegal_Instruction intentionally omitted.

--   Signal_Interrupt_Ref                : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Interrupt);

--     --  Signal_Kill intentionally omitted.

--   Signal_Pipe_Write_Ref               : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Pipe_Write);

--   Signal_Quit_Ref                     : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Quit);

--     --  Signal_Segmentation_Violation intentionally omitted.

--   Signal_Terminate_Ref                : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Terminate);

--   Signal_User_1_Ref                   : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_User_1);

--   Signal_User_2_Ref                   : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_User_2);

--   Signal_Child_Ref                    : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Child);

--   Signal_Continue_Ref                 : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Continue);

--     --  Signal_Stop intentionally omitted.

--   Signal_Terminal_Stop_Ref            : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Terminal_Stop);

--   Signal_Terminal_Input_Ref           : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Terminal_Input);

--   Signal_Terminal_Ouput_Ref           : constant System.Address
--       := System.Address'Ref (C_Signal_Values.Signal_Terminal_Ouput);




   --  type Signal Sets

   type Signal_Set is private;

   procedure Add_Signal
     (Set : in out Signal_Set;
      Sig : in    Signal);

   procedure Add_All_Signals (Set : in out Signal_Set);

   procedure Delete_Signal
     (Set : in out Signal_Set;
      Sig : in    Signal);

   procedure Delete_All_Signals (Set : in out Signal_Set);

   function Is_Member
     (Set : Signal_Set;
      Sig : Signal)
      return Boolean;


   --  Sending a Signal

   procedure Send_Signal
     (Process : in POSIX_Process_Identification.Process_ID;
      Sig     : in Signal);

   procedure Send_Signal
     (Process : in POSIX_Process_Identification.Process_Group_ID;
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

   function Blocked_Signals
     return Signal_Set;




   --  Ignoring Signals

   procedure Ignore_Signal (Sig : in Signal);

   procedure Unignore_Signal (Sig : in Signal);

   function Is_Ignored (Sig : Signal)
                        return Boolean;



   --  Controling Delivery of Signal_Child Signal

   procedure Set_Stopped_Child_Signal
     (Enable : in Boolean := True);

   function Stopped_Child_Signal_Enabled
     return Boolean;



   --  Examining Pending Signals

   function Pending_Signals
     return Signal_Set;


private

  type Signal_Set is
    record
      Values : Integer := 0;
    end record;

end POSIX_Signals;
