
--  $Id$
--  Author : Pascal Obry
--  pascal_obry@csi.com

with Calendar;
with Win32.Winbase;

package POSIX_Calendar is

   --  Time information

   type POSIX_Time is private;

   function Clock
     return POSIX_Time;

   function To_Time (Date : POSIX_Time)
                     return Calendar.Time;

   function To_POSIX_Time (Date : Calendar.Time)
                           return POSIX_Time;



   --  Operations on POSIX_Time

   subtype Year_Number   is Calendar.Year_Number;
   subtype Month_Number  is Calendar.Month_Number;
   subtype Day_Number    is Calendar.Day_Number;
   subtype Day_Duration  is Calendar.Day_Duration;

   function Year (Date : POSIX_Time)
                  return Year_Number;

   function Month (Date : POSIX_Time)
                   return Month_Number;

   function Day (Date : POSIX_Time)
                 return Day_Number;

   function Seconds (Date : POSIX_Time)
                     return Day_Duration;

   procedure Split
     (Date    : in     POSIX_Time;
      Year    :    out Year_Number;
      Month   :   out Month_Number;
      Day     :   out Day_Number;
      Seconds :   out Day_Duration);

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
      return POSIX_Time;

   function "+" (L : POSIX_Time; R : Duration)
                 return POSIX_Time;
   function "+" (L : Duration; R : POSIX_Time)
                 return POSIX_Time;
   function "-" (L : POSIX_Time; R : Duration)
                 return POSIX_Time;
   function "-" (L : POSIX_Time; R : POSIX_Time)
                 return Duration;

   function "<" (L, R : POSIX_Time)
                 return Boolean;
   function "<=" (L, R : POSIX_Time)
                  return Boolean;
   function ">" (L, R : POSIX_Time)
                 return Boolean;
   function ">=" (L, R : POSIX_Time)
                  return Boolean;

   Time_Error : exception renames Calendar.Time_Error;

private

   type POSIX_Time is new Win32.Winbase.SYSTEMTIME;

end POSIX_Calendar;
