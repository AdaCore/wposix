
-- $Id$

with Ada.Text_IO;
with POSIX;

procedure Demo10 is
   use Ada;
begin
   Text_IO.Put_Line ("System_Name = " & POSIX.To_String (POSIX.System_Name));
   Text_IO.Put_Line ("Node_Name   = " & POSIX.To_String (POSIX.Node_Name));
   Text_IO.Put_Line ("Release     = " & POSIX.To_String (POSIX.Release));
   Text_IO.Put_Line ("Version     = " & POSIX.To_String (POSIX.Version));
   Text_IO.Put_Line ("Machine     = " & POSIX.To_String (POSIX.Machine));
end Demo10;
