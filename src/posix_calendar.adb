
--  $Id$

with Interfaces.C;
with System;

package body POSIX_Calendar is

   --  Time information

   System_Time : aliased Win32.Winbase.SYSTEMTIME;

   -----------
   -- Clock --
   -----------

   function Clock
     return POSIX_Time is
   begin
      Win32.Winbase.GetLocalTime (System_Time'Access);
      return POSIX_Time (System_Time);
   end Clock;


   -------------
   -- To_Time --
   -------------

   function To_Time (Date : POSIX_Time)
                     return Calendar.Time
   is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Calendar.Time_Of (Calendar.Year_Number (Year),
                               Calendar.Month_Number (Month),
                               Calendar.Day_Number (Day),
                               Calendar.Day_Duration (Seconds));
   end To_Time;


   -------------------
   -- To_POSIX_Time --
   -------------------

   function To_POSIX_Time (Date : Calendar.Time)
                           return POSIX_Time
   is
      Year    : Calendar.Year_Number;
      Month   : Calendar.Month_Number;
      Day     : Calendar.Day_Number;
      Seconds : Calendar.Day_Duration;
   begin
      Calendar.Split (Date, Year, Month, Day, Seconds);
      return Time_Of (Year, Month, Day, Seconds);
   end To_POSIX_Time;



   --  Operations on POSIX_Time

   ----------
   -- Year --
   ----------

   function Year (Date : POSIX_Time)
                  return Year_Number
   is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Year;
   end Year;


   -----------
   -- Month --
   -----------

   function Month (Date : POSIX_Time)
                   return Month_Number
   is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Month;
   end Month;


   ---------
   -- Day --
   ---------

   function Day (Date : POSIX_Time)
                 return Day_Number
   is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Day;
   end Day;


   -------------
   -- Seconds --
   -------------

   function Seconds (Date : POSIX_Time)
                     return Day_Duration
   is
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
     (Date    : in     POSIX_Time;
      Year    :   out Year_Number;
      Month   :   out Month_Number;
      Day     :   out Day_Number;
      Seconds :   out Day_Duration)
   is
      use type Win32.WORD;
   begin
      Year    := Year_Number  (Date.wYear);
      Month   := Month_Number (Date.wMonth);
      Day     := Day_Number   (Date.wDay);
      Seconds := Day_Duration (Date.wSecond +
                               Date.wMinute * 60 +
                               Date.wHour * 3600);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
      return POSIX_Time is

      Local_Time : Win32.Winbase.SYSTEMTIME;

      Lsec  : Integer := Integer (Seconds);

   begin
      Local_Time.wYear   := Win32.WORD (Year);
      Local_Time.wMonth  := Win32.WORD (Month);
      Local_Time.wDay    := Win32.WORD (Day);

      Local_Time.wHour   := Win32.WORD (LSec / 3600);
      Lsec := Lsec rem 3600;
      Local_Time.wMinute := Win32.WORD (Lsec / 60);
      Local_Time.wSecond := Win32.WORD (Lsec rem 60);
      Local_Time.wMilliseconds := Win32.WORD (0);
      return POSIX_Time (Local_Time);
   end Time_Of;


   ---------
   -- "+" --
   ---------

   function "+" (L : POSIX_Time; R : Duration)
                 return POSIX_Time
   is
      use type Calendar.Time;
   begin
      return To_POSIX_Time (To_Time (L) + R);
   exception
      when others =>
         raise Time_Error;
   end "+";


   ---------
   -- "+" --
   ---------

   function "+" (L : Duration; R : POSIX_Time)
                 return POSIX_Time
   is
      use type Calendar.Time;
   begin
      return To_POSIX_Time (L + To_Time (R));
   exception
      when others =>
         raise Time_Error;
   end "+";


   ---------
   -- "-" --
   ---------

   function "-" (L : POSIX_Time; R : Duration)
                 return POSIX_Time
   is
      use type Calendar.Time;
   begin
      return To_POSIX_Time (To_Time (L) - R);
   exception
      when others =>
         raise Time_Error;
   end "-";


   ---------
   -- "-" --
   ---------

   function "-" (L : POSIX_Time; R : POSIX_Time)
                 return Duration
   is
      use type Calendar.Time;
   begin
      return Duration (To_Time (L) - To_Time (R));
   exception
      when others =>
         raise Time_Error;
   end "-";


   ---------
   -- "<" --
   ---------

   function "<" (L, R : POSIX_Time)
                 return Boolean
   is
      use type Calendar.Time;
   begin
      return To_Time (L) < To_Time (R);
   end "<";


   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : POSIX_Time)
                  return Boolean
   is
      use type Calendar.Time;
   begin
      return To_Time (L) <= To_Time (R);
   end "<=";


   ---------
   -- ">" --
   ---------

   function ">" (L, R : POSIX_Time)
                 return Boolean
   is
      use type Calendar.Time;
   begin
      return To_Time (L) > To_Time (R);
   end ">";


   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : POSIX_Time)
                  return Boolean
   is
      use type Calendar.Time;
   begin
      return To_Time (L) >= To_Time (R);
   end ">=";

end POSIX_Calendar;
