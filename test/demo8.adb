
with Ada.Text_IO;

with POSIX;
with POSIX_IO;
with POSIX_Process_Primitives;
with POSIX_Process_Identification;

procedure Demo8 is

   use Ada;

   package PPP renames POSIX_Process_Primitives;
   package PPI renames POSIX_Process_Identification;

   GMAKE_Template : PPP.Process_Template;
   GMAKE_Id       : PPI.Process_ID;
   GMAKE_Status   : PPP.Termination_Status;
   GMAKE_Args     : POSIX.POSIX_String_List;

begin

   Text_IO.Put_Line ("demo8");

   PPP.Open_Template (GMAKE_Template);

   PPP.Set_File_Action_To_Open (GMAKE_Template,
                                POSIX_IO.Standard_Output,
                                POSIX.To_POSIX_String ("stdout"),
                                POSIX_IO.Write_Only);

   PPP.Set_File_Action_To_Open (GMAKE_Template,
                                POSIX_IO.Standard_Error,
                                POSIX.To_POSIX_String ("stderr"),
                                POSIX_IO.Write_Only);

   POSIX.Append (GMAKE_Args, "-i");
   POSIX.Append (GMAKE_Args, "demo1");

   PPP.Start_Process
     (Child    => GMAKE_Id,
      Pathname => POSIX.To_POSIX_String ("/usr/bin/gnatmake.exe"),
      Template => GMAKE_Template,
      Arg_List => GMAKE_Args);

   PPP.Wait_For_Child_Process (GMAKE_Status, GMAKE_Id);

   PPP.Close_Template (GMAKE_Template);

end Demo8;
