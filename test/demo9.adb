
--  $Id$

with Ada.Text_IO;

with POSIX;
with POSIX_IO;
with POSIX_Process_Primitives;
with POSIX_Process_Identification;

procedure Demo9 is

   use Ada;

   package PPP renames POSIX_Process_Primitives;
   package PPI renames POSIX_Process_Identification;

   GMAKE_Template : PPP.Process_Template;
   GMAKE_Id       : PPI.Process_ID;
   GMAKE_Status   : PPP.Termination_Status;
   GMAKE_Args1    : POSIX.POSIX_String_List;
   GMAKE_Args2    : POSIX.POSIX_String_List;

begin

   PPP.Open_Template (GMAKE_Template);

   PPP.Set_File_Action_To_Open (GMAKE_Template,
                                POSIX_IO.Standard_Output,
                                POSIX.To_POSIX_String ("nul"),
                                POSIX_IO.Write_Only);

   PPP.Set_File_Action_To_Open (GMAKE_Template,
                                POSIX_IO.Standard_Error,
                                POSIX.To_POSIX_String ("nul"),
                                POSIX_IO.Write_Only);

   POSIX.Append (GMAKE_Args1, "-i");
   POSIX.Append (GMAKE_Args1, "-I../src");
   POSIX.Append (GMAKE_Args1, "demo1");

   PPP.Start_Process
     (Child    => GMAKE_Id,
      Pathname => POSIX.To_POSIX_String ("/usr/bin/gnatmake.exe"),
      Template => GMAKE_Template,
      Arg_List => GMAKE_Args1);
   Text_IO.Put_Line ("Process 1 : " & PPI.Image (GMAKE_Id));

   POSIX.Append (GMAKE_Args2, "-i");
   POSIX.Append (GMAKE_Args1, "-I../src");
   POSIX.Append (GMAKE_Args2, "demo2");

   PPP.Start_Process
     (Child    => GMAKE_Id,
      Pathname => POSIX.To_POSIX_String ("/usr/bin/gnatmake.exe"),
      Template => GMAKE_Template,
      Arg_List => GMAKE_Args2);
   Text_IO.Put_Line ("Process 2 : " & PPI.Image (GMAKE_Id));

   PPP.Wait_For_Child_Process (GMAKE_Status);
   Text_IO.Put_Line (PPI.Image (PPP.Process_ID_Of (GMAKE_Status)));

   PPP.Wait_For_Child_Process (GMAKE_Status);
   Text_IO.Put_Line (PPI.Image (PPP.Process_ID_Of (GMAKE_Status)));

   PPP.Close_Template (GMAKE_Template);

end Demo9;
