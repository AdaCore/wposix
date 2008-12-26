
--  with POSIX;

package POSIX_Process_Times is

   Ticks_Per_Second : constant := 1000;

   type Tick_Count is new Long_Integer;

   function Elapsed_Real_Time
     return Tick_Count;

   type Process_Times is private;

   function Get_Process_Times
     return Process_Times;

   function Elapsed_Real_Time_Of (Times : Process_Times)
                                  return Tick_Count;

   function User_CPU_Time_Of (Times : Process_Times)
                              return Tick_Count;

   function System_CPU_Time_Of (Times : Process_Times)
                                return Tick_Count;

   function Descendants_User_CPU_Time_Of (Times : Process_Times)
                                          return Tick_Count;

   function Descendants_System_CPU_Time_Of (Times : Process_Times)
                                            return Tick_Count;

private

  type Process_Times is
    record
      User_Time            : Tick_Count;
      System_Time          : Tick_Count;
      Children_User_Time   : Tick_Count;
      Children_System_Time : Tick_Count;
    end record;

end POSIX_Process_Times;
