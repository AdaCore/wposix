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

with POSIX_Win32;

package body POSIX.Process_Times is

   function Filetime_To_Tick
     (Filetime : Win32.Winbase.FILETIME) return Tick_Count;
   --  ???

   ------------------------------------
   -- Descendants_System_CPU_Time_Of --
   ------------------------------------

   function Descendants_System_CPU_Time_Of
     (Times : Process_Times) return Tick_Count is
   begin
      return Times.Children_System_Time;
   end Descendants_System_CPU_Time_Of;

   ----------------------------------
   -- Descendants_User_CPU_Time_Of --
   ----------------------------------

   function Descendants_User_CPU_Time_Of
     (Times : Process_Times) return Tick_Count is
   begin
      return Times.Children_User_Time;
   end Descendants_User_CPU_Time_Of;

   -----------------------
   -- Elapsed_Real_Time --
   -----------------------

   function Elapsed_Real_Time return Tick_Count is
   begin
      return Elapsed_Real_Time_Of (Get_Process_Times);
   end Elapsed_Real_Time;

   --------------------------
   -- Elapsed_Real_Time_Of --
   --------------------------

   function Elapsed_Real_Time_Of
     (Times : Process_Times) return Tick_Count
   is
      use type Win32.DWORD;

      Ok      : Win32.BOOL;
      Ctimes  : aliased Win32.Winbase.SYSTEMTIME;
      Now     : aliased Win32.Winbase.FILETIME;
      Elapsed : Win32.Winbase.FILETIME;
   begin
      Win32.Winbase.GetSystemTime (Ctimes'Unchecked_Access);
      Ok := Win32.Winbase.SystemTimeToFileTime
        (Ctimes'Unchecked_Access, Now'Unchecked_Access);
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
   -- Filetime_To_Tick --
   ----------------------

   function Filetime_To_Tick
     (Filetime : Win32.Winbase.FILETIME) return Tick_Count
   is
      use type Win32.ULONG;
      Hundred_Nano_To_Tick : constant := 1E7 / Ticks_Per_Second;

      --  ??? change the code to handle DwHighDateTime (see code in v1.4)
   begin
      return Tick_Count (Filetime.dwLowDateTime / Hundred_Nano_To_Tick);
   end Filetime_To_Tick;

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

   ------------------------
   -- System_CPU_Time_Of --
   ------------------------

   function System_CPU_Time_Of (Times : Process_Times) return Tick_Count is
   begin
      return Times.System_Time;
   end System_CPU_Time_Of;

   ----------------------
   -- User_CPU_Time_Of --
   ----------------------

   function User_CPU_Time_Of (Times : Process_Times) return Tick_Count is
   begin
      return Times.User_Time;
   end User_CPU_Time_Of;

end POSIX.Process_Times;
