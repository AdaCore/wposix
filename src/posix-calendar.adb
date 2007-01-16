
--  $Id$
--  Author : Pascal Obry
--  p.obry@wanadoo.fr

package body POSIX.Calendar is

   --  Time information

   -----------
   -- Clock --
   -----------

   function Clock return POSIX_Time is
      System_Time : aliased Win32.Winbase.SYSTEMTIME;
   begin
      Win32.Winbase.GetLocalTime (System_Time'Unchecked_Access);
      return POSIX_Time (System_Time);
   end Clock;

   -------------
   -- To_Time --
   -------------

   function To_Time (Date : in POSIX_Time) return Ada.Calendar.Time is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Seconds);
      return Ada.Calendar.Time_Of (Ada.Calendar.Year_Number (Year),
                                   Ada.Calendar.Month_Number (Month),
                                   Ada.Calendar.Day_Number (Day),
                                   Ada.Calendar.Day_Duration (Seconds));
   end To_Time;

   -------------------
   -- To_POSIX_Time --
   -------------------

   function To_POSIX_Time (Date : in Ada.Calendar.Time) return POSIX_Time is
      Year    : Ada.Calendar.Year_Number;
      Month   : Ada.Calendar.Month_Number;
      Day     : Ada.Calendar.Day_Number;
      Seconds : Ada.Calendar.Day_Duration;
   begin
      Ada.Calendar.Split (Date, Year, Month, Day, Seconds);
      return Time_Of (Year, Month, Day, Seconds);
   end To_POSIX_Time;

   --  Operations on POSIX_Time

   ----------
   -- Year --
   ----------

   function Year (Date : in POSIX_Time) return Year_Number is
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

   function Month (Date : in POSIX_Time) return Month_Number is
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

   function Day (Date : in POSIX_Time) return Day_Number is
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

   function Seconds (Date : in POSIX_Time) return Day_Duration is
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
     (Year    : in Year_Number;
      Month   : in Month_Number;
      Day     : in Day_Number;
      Seconds : in Day_Duration := 0.0)
     return POSIX_Time
   is

      Local_Time : Win32.Winbase.SYSTEMTIME;

      Lsec  : Integer := Integer (Seconds);

   begin
      Local_Time.wYear   := Win32.WORD (Year);
      Local_Time.wMonth  := Win32.WORD (Month);
      Local_Time.wDay    := Win32.WORD (Day);

      Local_Time.wHour   := Win32.WORD (Lsec / 3600);
      Lsec := Lsec rem 3600;
      Local_Time.wMinute := Win32.WORD (Lsec / 60);
      Local_Time.wSecond := Win32.WORD (Lsec rem 60);
      Local_Time.wMilliseconds
        := Win32.WORD ((Seconds - Day_Duration (Lsec) - 0.5) * 1000);
      --  Lsec is rounded, not truncated - hence the "-0.5"
      return POSIX_Time (Local_Time);
   end Time_Of;

   ---------
   -- "+" --
   ---------

   function "+" (L : in POSIX_Time; R : in Duration) return POSIX_Time is
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

   function "+" (L : in Duration; R : in POSIX_Time) return POSIX_Time is
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

   function "-" (L : in POSIX_Time; R : in Duration) return POSIX_Time is
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

   function "-" (L : in POSIX_Time; R : in POSIX_Time) return Duration is
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

   function "<" (L, R : in POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) < To_Time (R);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : in POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) <= To_Time (R);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : in POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) > To_Time (R);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : in POSIX_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return To_Time (L) >= To_Time (R);
   end ">=";

end POSIX.Calendar;
