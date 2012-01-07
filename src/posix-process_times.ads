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

private with Win32.Winbase;

package POSIX.Process_Times is

   Ticks_Per_Second : constant := 1000;

   type Tick_Count is new Long_Integer;

   function Elapsed_Real_Time return Tick_Count;

   type Process_Times is private;

   function Get_Process_Times return Process_Times;

   function Elapsed_Real_Time_Of (Times : Process_Times) return Tick_Count;

   function User_CPU_Time_Of (Times : Process_Times) return Tick_Count;

   function System_CPU_Time_Of (Times : Process_Times) return Tick_Count;

   function Descendants_User_CPU_Time_Of
     (Times : Process_Times) return Tick_Count;

   function Descendants_System_CPU_Time_Of
     (Times : Process_Times) return Tick_Count;

private

   type Process_Times is record
      Creation_Time        : Win32.Winbase.FILETIME;
      User_Time            : Tick_Count;
      System_Time          : Tick_Count;
      Children_User_Time   : Tick_Count;
      Children_System_Time : Tick_Count;
   end record;

end POSIX.Process_Times;
