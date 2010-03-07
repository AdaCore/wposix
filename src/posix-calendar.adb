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

package body POSIX.Calendar is

   ---------
   -- "+" --
   ---------

   function "+" (L : POSIX_Time; R : Duration) return POSIX_Time is
      use type Ada.Calendar.Time;
   begin
      return To_POSIX_Time (To_Time (L) + R);
   exception
      when others =>
         raise Time_Error;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (L : Duration; R : POSIX_Time) return POSIX_Time is
      use type Ada.Calendar.Time;
   begin
      return To_POSIX_Time (L + To_Time (R));
   exception
      when others =>
         raise Time_Error;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : POSIX_Time; R : Duration) return POSIX_Time is
      use type Ada.Calendar.Time;
   begin
      return To_POSIX_Time (To_Time (L) - R);
   exception
      when others =>
         raise Time_Error;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (L : POSIX_Time; R : POSIX_Time) return Duration is
      use type Ada.Calendar.Time;
   begin
      return Duration (To_Time (L) - To_Time (R));
   exception
      when others =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) < To_Time (R);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) <= To_Time (R);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) > To_Time (R);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) >= To_Time (R);
   end ">=";

   -----------
   -- Clock --
   -----------

   function Clock return POSIX_Time is
      System_Time : aliased Win32.Winbase.SYSTEMTIME;
   begin
      Win32.Winbase.GetLocalTime (System_Time'Unchecked_Access);
      return POSIX_Time (System_Time);
   end Clock;

   ---------
   -- Day --
   ---------

   function Day (Date : POSIX_Time) return Day_Number is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Day;
   end Day;

   -----------
   -- Month --
   -----------

   function Month (Date : POSIX_Time) return Month_Number is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Month;
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : POSIX_Time) return Day_Duration is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Seconds;
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    :        POSIX_Time;
      Year    :    out Year_Number;
      Month   :    out Month_Number;
      Day     :    out Day_Number;
      Seconds :    out Day_Duration) is
   begin
      Year    := Year_Number  (Date.wYear);
      Month   := Month_Number (Date.wMonth);
      Day     := Day_Number   (Date.wDay);
      Seconds := Day_Duration (Date.wHour) * 3600 +
                 Day_Duration (Date.wMinute) * 60 +
                 Day_Duration (Date.wSecond)      +
                 Day_Duration (Date.wMilliseconds) / 1000;
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return POSIX_Time
   is
      Local_Time : Win32.Winbase.SYSTEMTIME;
      Lsec       : Integer := Integer (Seconds - 0.5);

   begin
      Local_Time.wYear   := Win32.WORD (Year);
      Local_Time.wMonth  := Win32.WORD (Month);
      Local_Time.wDay    := Win32.WORD (Day);

      if Lsec = -1 then
         --  Special case handling to prevent Constraint_Error due to round
         --  down to -1 in Lsec.
         Local_Time.wHour   := 0;
         Local_Time.wMinute := 0;
         Local_Time.wSecond := 0;
         Local_Time.wMilliseconds := 0;

      else
         Local_Time.wHour   := Win32.WORD (Lsec / 3600);
         Lsec := Lsec rem 3600;
         Local_Time.wMinute := Win32.WORD (Lsec / 60);
         Local_Time.wSecond := Win32.WORD (Lsec rem 60);
         Local_Time.wMilliseconds := Win32.WORD
           ((Seconds - Day_Duration (Integer (Seconds - 0.5))) * 1000);
         --  Lsec is rounded, not truncated - hence the "-0.5"
      end if;
      return POSIX_Time (Local_Time);
   end Time_Of;

   -------------------
   -- To_POSIX_Time --
   -------------------

   function To_POSIX_Time (Date : Ada.Calendar.Time) return POSIX_Time is
      Year    : Ada.Calendar.Year_Number;
      Month   : Ada.Calendar.Month_Number;
      Day     : Ada.Calendar.Day_Number;
      Seconds : Ada.Calendar.Day_Duration;
   begin
      Ada.Calendar.Split (Date, Year, Month, Day, Seconds);
      return Time_Of (Year, Month, Day, Seconds);
   end To_POSIX_Time;

   -------------
   -- To_Time --
   -------------

   function To_Time (Date : POSIX_Time) return Ada.Calendar.Time is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Ada.Calendar.Time_Of
        (Ada.Calendar.Year_Number (Year),
         Ada.Calendar.Month_Number (Month),
         Ada.Calendar.Day_Number (Day),
         Ada.Calendar.Day_Duration (Seconds));
   end To_Time;

   ----------
   -- Year --
   ----------

   function Year (Date : POSIX_Time) return Year_Number is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Year;
   end Year;

end POSIX.Calendar;
