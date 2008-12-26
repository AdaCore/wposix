
-- $Id$

with Ada.Text_IO;

with POSIX;
with POSIX_Process_Environment;

procedure Demo5 is

   use Ada;

   package PPE renames POSIX_Process_Environment;

   procedure Action (Name, Value : in     POSIX.POSIX_String;
                     Quit        : in out Boolean) is
   begin
      Text_IO.Put_Line (POSIX.To_String (Name) & " = " &
                        POSIX.To_String (Value));
      Quit := False;
   end Action;

   procedure Display_Env is new PPE.For_Every_Environment_Variable (Action);

   procedure Display_Env is new
     PPE.For_Every_Current_Environment_Variable (Action);

   Current_Env : PPE.Environment;

begin
   Display_Env;
   Text_IO.Put_Line ("1 ----------");
   PPE.Copy_From_Current_Environment (Current_Env);
   PPE.Set_Environment_Variable ("ada", "this is ada");
   PPE.Delete_Environment_Variable ("LOGNAME");
   Display_Env (Current_Env);
   Text_IO.Put_Line ("2 ----------");
   Display_Env;
   Text_IO.Put_Line ("3 ----------");
   PPE.Clear_Environment (Current_Env);
   Display_Env (Current_Env);
   Text_IO.Put_Line ("4 ----------");
   PPE.Copy_To_Current_Environment (Current_Env);
   Display_Env;
   Text_IO.Put_Line ("5 ----------");
   PPE.Set_Environment_Variable ("Only", "one", Current_Env);
   Display_Env (Current_Env);
end Demo5;
