
with Ada.Text_IO;

with POSIX;
with POSIX_Process_Environment;

procedure Demo6 is

   use Ada.Text_IO;
   use POSIX;

   package PPE renames POSIX_Process_Environment;

   procedure Display (Name, Value : in     POSIX_String;
                      Quit        : in out Boolean) is
   begin
      Put (To_String (Name));
      Put (" = ");
      Put (To_String (Value));
      New_Line;
      Quit := False;
      Flush;
   end Display;

   procedure Display_Env is
     new PPE.For_Every_Current_Environment_Variable (Display);

   procedure Is_Env (Name : String) is
   begin
      if PPE.Is_Environment_Variable (To_POSIX_String (Name)) then
         Put_Line (Name & " found.");
      else
         Put_Line (Name & " not found.");
      end if;
      Flush;
   end Is_Env;

begin
   PPE.Set_Environment_Variable ("GNAT_DIR", "hello new gnat dir");
   PPE.Set_Environment_Variable ("A NEW One", "Vive POSIX NT !!!");
   PPE.Delete_Environment_Variable ("SDK_DIR");

   Put_Line ("***** Display 1");
   Flush;
   Display_Env;

   New_Line;
   Is_Env ("TRE");
   Is_Env ("ADA_INCLUDE_PATH");
   New_Line;

   PPE.Delete_Environment_Variable ("ADA_INCLUDE_PATH");
   New_Line;
   Is_Env ("TRE");
   Is_Env ("ADA_INCLUDE_PATH");
   New_Line;

   Put_Line ("***** Clear");
   PPE.Clear_Environment;
   Put_Line ("***** Display 2");
   Display_Env;

   Is_Env ("TRE");
   Is_Env ("ADA_INCLUDE_PATH");
end Demo6;
