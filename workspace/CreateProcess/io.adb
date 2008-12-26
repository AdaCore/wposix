
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

procedure IO is

   use Ada.Text_IO;
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;

   Buffer : String (1 .. 30);
   Last   : Positive;

   Parameters : Unbounded_String;

begin
   for I in 1 .. Argument_Count loop
      Parameters := Parameters & Argument (I) & "/";
   end loop;
   Get_Line (Standard_Input, Buffer, Last);
   Put_Line (Standard_Output,
             "the standard output : " & Buffer (1 .. Last) &
             "> " & To_String (Parameters));
   Put_Line (Standard_Error,
             "the standard error : " & Buffer (1 .. Last) &
             "> " & To_String (Parameters));
   delay 5.0;
end IO;
