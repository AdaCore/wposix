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
with Ada.Strings.Fixed;

with POSIX.File_Status;
with POSIX.Process_Identification;

procedure Owner is

   use Ada;
   use POSIX;

   procedure Validate (SID, Name : String);
   --  Validate string structure

   --------------
   -- Validate --
   --------------

   procedure Validate (SID, Name : String) is
   begin
      if SID (SID'First .. SID'First + 7) = "S-1-5-21"
        and then Strings.Fixed.Count (SID, "-") = 7
      then
         Text_IO.Put_Line ("OK " & Name & " seems correct.");
      else
         Text_IO.Put_Line ("NOK " & Name & " seems wrong: " & SID);
      end if;
   end Validate;

   Status : File_Status.Status;
   UID    : Process_Identification.User_ID;
   GID    : Process_Identification.Group_ID;

begin
   Status := File_Status.Get_File_Status ("test.py");
   UID := File_Status.Owner_Of (Status);
   GID := File_Status.Group_Of (Status);

   declare
      F_UID : constant String := Process_Identification.Image (UID);
      F_GID : constant String := Process_Identification.Image (GID);
      P_UID : constant String :=
                Process_Identification.Image
                  (Process_Identification.Get_Real_User_ID);
      P_GID : constant String :=
                Process_Identification.Image
                  (Process_Identification.Get_Real_Group_ID);
   begin
      if F_UID = P_UID then
         Text_IO.Put_Line ("OK, F/P UID");
      else
         Text_IO.Put_Line ("NOK, F/P UID, " & F_UID & "," & P_UID);
      end if;

      if F_GID = P_GID then
         Text_IO.Put_Line ("OK, F/P GID");
      else
         Text_IO.Put_Line ("NOK, F/P GID, " & F_GID & "," & P_GID);
      end if;

      Validate (F_UID, "F_UID");
      Validate (F_GID, "F_GID");
      Validate (P_UID, "P_UID");
      Validate (P_GID, "P_GID");
   end;
end Owner;
