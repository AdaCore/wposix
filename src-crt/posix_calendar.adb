
--  $Id$

with Interfaces.C;
with System;
with Win32.Crt.Time;

package body POSIX_Calendar is

   --  Time information

   function Clock
     return POSIX_Time is
   begin
      return POSIX_Time (Win32.Crt.Time.Time (null));
   end Clock;

                -----------------------------------

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

                -----------------------------------

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

                -----------------------------------

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

                -----------------------------------

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

                -----------------------------------

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

                -----------------------------------

   procedure Split
     (Date    : in     POSIX_Time;
      Year    :   out Year_Number;
      Month   :   out Month_Number;
      Day     :   out Day_Number;
      Seconds :   out Day_Duration)
   is

      Ldate : aliased Win32.Crt.Types.Time_T := Win32.Crt.Types.Time_T (Date);
      Localtime : Win32.Crt.Time.A_Tm_T;

   begin
      Localtime := Win32.Crt.Time.Localtime (Ldate'Unchecked_Access);
      Year    := Integer (Localtime.Tm_Year);
      Month   := Integer (Localtime.Tm_Mon);
      Day     := Integer (Localtime.Tm_Wday);
      Seconds := Duration (Localtime.Tm_Sec);
   end Split;

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
      return POSIX_Time is

      use type Interfaces.C.Int;
      Ltime : aliased Win32.Crt.Time.Tm;
      Lsec  : Integer := Integer (Seconds);

   begin
      Ltime.Tm_Year  := Win32.INT (Year - 1900);
      Ltime.Tm_Mon   := Win32.INT (Month - 1);
      Ltime.Tm_Mday  := Win32.INT (Day);
      Ltime.Tm_Hour  := Win32.INT (LSec / 3600);
      Lsec := Lsec rem 3600;
      Ltime.Tm_Min   := Win32.INT (Lsec / 60);
      Ltime.Tm_Sec   := Win32.INT (Lsec rem 60);
      Ltime.Tm_Isdst := -1;

      return POSIX_Time (Win32.Crt.Time.Mktime (Ltime'Access));
   end Time_Of;

                -----------------------------------

   function "+" (L : POSIX_Time; R : Duration)
                 return POSIX_Time is
   begin
      return POSIX_Time (Integer (L) + Integer (R));
   exception
      when others =>
         raise Time_Error;
   end "+";

                -----------------------------------

   function "+" (L : Duration; R : POSIX_Time)
                 return POSIX_Time is
   begin
      return POSIX_Time (Integer (R) + Integer (L));
   exception
      when others =>
         raise Time_Error;
   end "+";

                -----------------------------------

   function "-" (L : POSIX_Time; R : Duration)
                 return POSIX_Time is
   begin
      return POSIX_Time (Integer (L) - Integer (R));
   exception
      when others =>
         raise Time_Error;
   end "-";

                -----------------------------------

   function "-" (L : POSIX_Time; R : POSIX_Time)
                 return Duration is
   begin
      return Duration (Integer (L) - Integer (R));
   exception
      when others =>
         raise Time_Error;
   end "-";

                -----------------------------------

   function "<" (L, R : POSIX_Time)
                 return Boolean is
   begin
      return Integer (L) < Integer (R);
   end "<";

                -----------------------------------

   function "<=" (L, R : POSIX_Time)
                  return Boolean is
   begin
      return Integer (L) <= Integer (R);
   end "<=";

                -----------------------------------

   function ">" (L, R : POSIX_Time)
                 return Boolean is
   begin
      return Integer (L) > Integer (R);
   end ">";

                -----------------------------------

   function ">=" (L, R : POSIX_Time)
                  return Boolean is
   begin
      return Integer (L) >= Integer (R);
   end ">=";

end POSIX_Calendar;
