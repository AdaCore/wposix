
--  $Id$

with Ada.Text_IO;
with POSIX_IO;
with POSIX_Permissions;
with POSIX_Process_Times;

procedure Demo3 is

   use Ada.Text_IO;
   use POSIX_IO;
   use POSIX_Permissions;
   use POSIX_Process_Times;

   procedure Display_Process_Times is
      PT : Process_Times := Get_Process_Times;
   begin
      Put_Line ("Elapsed_Real_Time_Of : " &
                Tick_Count'Image (Elapsed_Real_Time_Of (PT)));
      Put_Line ("User_CPU_Time_Of : " &
                Tick_Count'Image (User_CPU_Time_Of (PT)));
      Put_Line ("System_CPU_Time_Of : " &
                Tick_Count'Image (System_CPU_Time_Of (PT)));
   end Display_Process_Times;

   File : File_Descriptor;

begin
   File := Open_Or_Create ("fdemo3", Read_Write, Access_Permission_Set);

   Put_Line ("Is_A_Terminal : " & Boolean'Image (Is_A_Terminal (File)));

   Close (File);

   New_Line;
   Display_Process_Times;
   delay 2.0;
   New_Line;
   Display_Process_Times;
end Demo3;
