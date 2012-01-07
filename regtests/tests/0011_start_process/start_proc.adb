------------------------------------------------------------------------------
--                                  wPOSIX                                  --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;

with POSIX;
with POSIX.IO;
with POSIX.Process_Environment;
with POSIX.Process_Identification;
with POSIX.Process_Primitives;

procedure Start_Proc is

   use Ada;
   use Ada.Exceptions;

   package PPP renames POSIX.Process_Primitives;
   package PPI renames POSIX.Process_Identification;
   package PPE renames POSIX.Process_Environment;

   Prog_Template : PPP.Process_Template;
   Prog_Id       : PPI.Process_ID;
   Prog_Status   : PPP.Termination_Status;
   Prog_Args     : POSIX.POSIX_String_List;
   Prog_Env      : PPE.Environment;

begin
   Text_IO.Put_Line ("Start_Proc...");

   PPP.Open_Template (Prog_Template);

   PPP.Set_File_Action_To_Open
     (Prog_Template,
      POSIX.IO.Standard_Error,
      POSIX.To_POSIX_String ("nul"),
      POSIX.IO.Write_Only);

   POSIX.Append (Prog_Args, "prog.exe");
   POSIX.Append (Prog_Args, "arg1");
   POSIX.Append (Prog_Args, "arg2");

   --  Start process

   PPP.Start_Process
     (Child    => Prog_Id,
      Pathname => POSIX.To_POSIX_String ("./prog.exe"),
      Template => Prog_Template,
      Arg_List => Prog_Args);

   PPP.Wait_For_Child_Process (Prog_Status, Prog_Id);

   --  Likewise but with an environment this time

   PPE.Set_Environment_Variable ("START_PROC_1", "one", Prog_Env);
   PPE.Set_Environment_Variable ("START_PROC_2", "two", Prog_Env);

   PPP.Start_Process
     (Child    => Prog_Id,
      Pathname => POSIX.To_POSIX_String ("./prog.exe"),
      Template => Prog_Template,
      Arg_List => Prog_Args,
      Env_List => Prog_Env);

   PPP.Wait_For_Child_Process (Prog_Status, Prog_Id);

   PPP.Close_Template (Prog_Template);

   Text_IO.Put_Line ("End Start_Proc");
exception
   when E : others =>
      Text_IO.Put_Line ("Error : " & Exception_Information (E));
end Start_Proc;
