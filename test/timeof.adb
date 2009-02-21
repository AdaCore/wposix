
with Ada.Calendar;   use Ada.Calendar;
with Ada.Text_Io;

with Win32;
with POSIX.Calendar; use POSIX.Calendar;

procedure TimeOf is

   use Ada;

   use type POSIX.Calendar.POSIX_Time;

   Expiry: constant POSIX.Calendar.POSIX_Time := POSIX.Calendar.Clock + 1.0;

   Time_Remaining: Duration := 1.0;
   P : POSIX.Calendar.POSIX_Time;
   T : Calendar.Time := Calendar.Time_Of (2009, 1, 2, 0.0);

   function TimeStamp (Time : in Calendar.Time) return String is

      Day      : Calendar.Day_Number;
      Duration : Calendar.Day_Duration;
      Hour     : Integer range 0..24;
      Min      : Integer range 0..60;
      Month    : Calendar.Month_Number;
      Sec      : Integer range 0..60;
      MSec     : Integer;
      Sec_32   : Integer;
      Sec_A    : Float;
      Year     : Calendar.Year_Number;
      Year_8   : Integer;

      Time_Delimiter : constant Character := ':';

      Result   : String (1 .. 20);
   begin
      --  Split time
      Calendar.Split(Time, Year, Month, Day, Duration);
      Sec_32 := Integer(Duration);
      Hour := Integer ( Sec_32 / 3600 );
      Min := Integer ( ( Sec_32 mod 3600 ) / 60 );
      Sec := Integer ( ( Sec_32 mod 3600 ) mod 60 );
      Sec_A := Float(Duration);
      MSec := Integer( (Sec_A - Float'Floor(Sec_A)) * 1000.0);

      --  Convert to DDMMMYY
      Year_8 := Integer(Year mod 100);
      Result(1..2) := Integer'Image(Integer(Day) + 100)(3..4);
      case Month is
         when 1 =>  Result(3..5) := "JAN";
         when 2 =>  Result(3..5) := "FEB";
         when 3 =>  Result(3..5) := "MAR";
         when 4 =>  Result(3..5) := "APR";
         when 5 =>  Result(3..5) := "MAY";
         when 6 =>  Result(3..5) := "JUN";
         when 7 =>  Result(3..5) := "JUL";
         when 8 =>  Result(3..5) := "AUG";
         when 9 =>  Result(3..5) := "SEP";
         when 10 => Result(3..5) := "OCT";
         when 11 => Result(3..5) := "NOV";
         when 12 => Result(3..5) := "DEC";
         when others => Result(3..5) := "  ?";
      end case;
      Result(6..7) := Integer'Image(Year_8 + 100)(3..4);

      Result(8) := ' ';

      --  Convert to HH:MM:SS.mmm
      Result(9..10) := Integer'Image(Hour + 100)(3..4);
      Result(11) := Time_Delimiter;
      Result(12..13) := Integer'Image(Min + 100)(3..4);
      Result(14) := Time_Delimiter;
      Result(15..16) := Integer'Image(Sec + 100)(3..4);
      Result(17) := '.';
      Result(18..20) := Integer'Image(MSec + 1000)(3..5);

      return Result;
   end;

   Seconds : Calendar.Day_Duration := 0.0;

   LSec : Integer := 0;

   MSecTest : Integer := 0;
begin
   Time_Remaining := Expiry - POSIX.Calendar.Clock;
   Text_Io.Put_Line("The next line shoud be very close to 1.0");
   Text_Io.Put_Line("TR"&Duration'Image(Time_Remaining));

   loop
      T := Calendar.Time_Of (2009, 1, 2, Seconds);
      P := To_POSIX_Time(T);

      if not (To_Time(P) = T) then
         Text_Io.Put_Line ("Ada: " & Timestamp (T));
         Text_Io.Put_Line ("POSIX: " & Timestamp (To_Time (P)));
         Text_Io.Put_Line ("Equality fails");
         exit;
      end if;

      Seconds := Seconds + 0.001;
      MsecTest := MsecTest + 1;

      exit when Seconds > 5.0;
   end loop;

   P := To_POSIX_Time(T);

   if not (To_Time(P) = T) then
      Text_Io.Put_Line("Ada: " & Timestamp (T));
      Text_Io.Put_Line("POSIX: " & Timestamp (To_Time (P)));
      Text_Io.Put_Line("Equality fails");
   end if;

   T := T + 1.0;
   P := P + 1.0;

   if not (To_Time(P) = T) then
      Text_Io.Put_Line("Equality after add fails");
   end if;

   T := T - 1.0;
   P := P - 1.0;

   if not (To_Time(P) = T) then
      Text_Io.Put_Line("Equality after minus fails");
   end if;
end Timeof;
