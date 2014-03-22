------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with POSIX.File_Status;
with POSIX.Process_Identification;

procedure Owner is

   use Ada;
   use POSIX;

   procedure Validate (SID, Name : String; RSID : String := "");
   --  Validate string structure

   function Root_SID (SID : String) return String
     is (SID (SID'First .. SID'First + 7));

   --------------
   -- Validate --
   --------------

   procedure Validate (SID, Name : String; RSID : String := "") is
      Check : constant String := (if RSID = "" then "S-1-5-21" else RSID);
   begin
      if SID (SID'First .. SID'First + 7) = Check
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

      Validate (F_UID, "F_UID", Root_SID (P_UID));
      Validate (F_GID, "F_GID", Root_SID (P_GID));
      Validate (P_UID, "P_UID");

      if P_GID (P_GID'First .. P_GID'First + 7)
        in "S-1-5-21" | "S-1-5-32"
      then
         Text_IO.Put_Line ("OK P_GID seems correct.");
      else
         Text_IO.Put_Line ("NOK P_GID seems wrong.");
      end if;
   end;
end Owner;
