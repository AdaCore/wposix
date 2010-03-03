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

with Ada.Calendar;

private with Win32.Winbase;

package POSIX.Calendar is

   --  Time information

   type POSIX_Time is private;

   function Clock return POSIX_Time;

   function To_Time (Date : in POSIX_Time) return Ada.Calendar.Time;

   function To_POSIX_Time (Date : in Ada.Calendar.Time) return POSIX_Time;

   --  Operations on POSIX_Time

   subtype Year_Number   is Ada.Calendar.Year_Number;
   subtype Month_Number  is Ada.Calendar.Month_Number;
   subtype Day_Number    is Ada.Calendar.Day_Number;
   subtype Day_Duration  is Ada.Calendar.Day_Duration;

   function Year    (Date : in POSIX_Time) return Year_Number;

   function Month   (Date : in POSIX_Time) return Month_Number;

   function Day     (Date : in POSIX_Time) return Day_Number;

   function Seconds (Date : in POSIX_Time) return Day_Duration;

   procedure Split
     (Date    : in     POSIX_Time;
      Year    :    out Year_Number;
      Month   :    out Month_Number;
      Day     :    out Day_Number;
      Seconds :    out Day_Duration);

   function Time_Of
     (Year    : in Year_Number;
      Month   : in Month_Number;
      Day     : in Day_Number;
      Seconds : in Day_Duration := 0.0) return POSIX_Time;

   function "+" (L : in POSIX_Time; R : in Duration)   return POSIX_Time;
   function "+" (L : in Duration;   R : in POSIX_Time) return POSIX_Time;
   function "-" (L : in POSIX_Time; R : in Duration)   return POSIX_Time;
   function "-" (L : in POSIX_Time; R : in POSIX_Time) return Duration;

   function "<"  (L, R : in POSIX_Time) return Boolean;
   function "<=" (L, R : in POSIX_Time) return Boolean;
   function ">"  (L, R : in POSIX_Time) return Boolean;
   function ">=" (L, R : in POSIX_Time) return Boolean;

   Time_Error : exception renames Ada.Calendar.Time_Error;

private

   type POSIX_Time is new Win32.Winbase.SYSTEMTIME;

end POSIX.Calendar;
