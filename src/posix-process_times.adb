
--  $Id$
--  Author : Pascal Obry
--  p.obry@wanadoo.fr

with Win32.Winbase;
with POSIX_Win32;

package body POSIX.Process_Times is

   ----------------------
   -- Filetime_To_Tick --
   ----------------------

   function Filetime_To_Tick
     (Filetime : in Win32.Winbase.FILETIME)
     return Tick_Count
   is
      use type Win32.ULONG;
      Hundred_Nano_To_Tick : constant := 1E7 / Ticks_Per_Second;

      --  ??? change the code to handle DwHighDateTime (see code in v1.4)
   begin
      return Tick_Count (Filetime.dwLowDateTime / Hundred_Nano_To_Tick);
   end Filetime_To_Tick;

   -----------------------
   -- Elapsed_Real_Time --
   -----------------------

   function Elapsed_Real_Time return Tick_Count is
   begin
      return Elapsed_Real_Time_Of (Get_Process_Times);
   end Elapsed_Real_Time;

   -----------------------
   -- Get_Process_Times --
   -----------------------

   function Get_Process_Times return Process_Times is
      Result                   : Win32.BOOL;
      Creation_Time, Exit_Time : aliased Win32.Winbase.FILETIME;
      Kernel_Time, User_Time   : aliased Win32.Winbase.FILETIME;
      PT                       : Process_Times;
   begin
      Result := Win32.Winbase.GetProcessTimes
        (Win32.Winbase.GetCurrentProcess,
         Creation_Time'Unchecked_Access,
         Exit_Time'Unchecked_Access,
         Kernel_Time'Unchecked_Access,
         User_Time'Unchecked_Access);
      POSIX_Win32.Check_Result (Result, "Get_Process_Times");

      PT := (Creation_Time => Creation_Time,
             User_Time     => Filetime_To_Tick (User_Time),
             System_Time   => Filetime_To_Tick (Kernel_Time),
             Children_User_Time |
             Children_System_Time => 0);
      return PT;
   end Get_Process_Times;

   --------------------------
   -- Elapsed_Real_Time_Of --
   --------------------------

   function Elapsed_Real_Time_Of
     (Times : in Process_Times)
     return Tick_Count
   is
      use type Win32.DWORD;

      Ok      : Win32.BOOL;
      Ctimes  : aliased Win32.Winbase.SYSTEMTIME;
      Now     : aliased Win32.Winbase.FILETIME;
      Elapsed : Win32.Winbase.FILETIME;
   begin
      Win32.Winbase.GetSystemTime (Ctimes'Unchecked_Access);
      Ok := Win32.Winbase.SystemTimeToFileTime (Ctimes'Unchecked_Access,
                                                Now'Unchecked_Access);
      POSIX_Win32.Check_Result (Ok, "SystemTimeToFileTime");

      Elapsed.dwLowDateTime  :=
        Now.dwLowDateTime  - Times.Creation_Time.dwLowDateTime;
      Elapsed.dwHighDateTime :=
        Now.dwHighDateTime - Times.Creation_Time.dwHighDateTime;

      if Now.dwLowDateTime < Times.Creation_Time.dwLowDateTime then
         Elapsed.dwHighDateTime := Elapsed.dwHighDateTime - 1;
      end if;

      return Filetime_To_Tick (Elapsed);
   end Elapsed_Real_Time_Of;

   ----------------------
   -- User_CPU_Time_Of --
   ----------------------

   function User_CPU_Time_Of (Times : in Process_Times) return Tick_Count is
   begin
      return Times.User_Time;
   end User_CPU_Time_Of;

   ------------------------
   -- System_CPU_Time_Of --
   ------------------------

   function System_CPU_Time_Of (Times : in Process_Times) return Tick_Count is
   begin
      return Times.System_Time;
   end System_CPU_Time_Of;

   ----------------------------------
   -- Descendants_User_CPU_Time_Of --
   ----------------------------------

   function Descendants_User_CPU_Time_Of
     (Times : in Process_Times)
     return Tick_Count is
   begin
      return Times.Children_User_Time;
   end Descendants_User_CPU_Time_Of;

   ------------------------------------
   -- Descendants_System_CPU_Time_Of --
   ------------------------------------

   function Descendants_System_CPU_Time_Of
     (Times : in Process_Times)
     return Tick_Count is
   begin
      return Times.Children_System_Time;
   end Descendants_System_CPU_Time_Of;

end POSIX.Process_Times;
