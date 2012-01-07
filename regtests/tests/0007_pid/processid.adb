------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
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
