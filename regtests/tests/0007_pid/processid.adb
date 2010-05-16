------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;

with POSIX.Process_Identification;

procedure ProcessID is

   use Ada;
   use POSIX;

   package PPI renames POSIX.Process_Identification;

   procedure Validate (PID : PPI.Process_ID; Name : String);
   --  Validate a process id

   --------------
   -- Validate --
   --------------

   procedure Validate (PID : PPI.Process_ID; Name : String) is
      use type PPI.Process_ID;
   begin
      if PID /= PPI.Null_Process_ID
        and then PID /= PPI.System_Process_ID
        and then PPI.Image (PID)'Length > 2
      then
         Text_IO.Put_Line ("OK " & Name);
      else
         Text_IO.Put_Line ("NOK " & Name & ": " & PPI.Image (PID));
      end if;
   end Validate;

   PID  : constant PPI.Process_ID := PPI.Get_Process_ID;
   PPID : constant PPI.Process_ID := PPI.Get_Parent_Process_ID;

begin
   Validate (PID, "PID");
   Validate (PPID, "PPID");
end ProcessID;
