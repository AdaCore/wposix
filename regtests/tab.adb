
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Interfaces.C;

procedure Tab is

   use Ada.Text_IO;
   use Ada.Integer_Text_IO;

   type C_Tab is array (1 .. 10) of Interfaces.C.Int;
   pragma Convention (C, C_Tab);

   T1 : C_Tab;
   pragma Import (C, T1, "tab1");

   type C_Tab2 is access C_Tab;
   T2 : C_Tab2;
   pragma Import (C, T2, "tab2");

begin
   Put_Line ("tab1");
   for I in T1'range loop
      Put (Integer (T1 (I)));
   end loop;
   New_Line;

   Put_Line ("tab2");
   for I in T2'Range loop
      Put (Integer (T2 (I)));
   end loop;
   New_Line;
end Tab;
