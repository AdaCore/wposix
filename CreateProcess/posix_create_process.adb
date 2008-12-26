
with Ada.Text_IO;
with Ada.Exceptions;

with POSIX;
with POSIX_IO;
with POSIX_Process_Primitives;
with POSIX_Process_Identification;

procedure POSIX_Create_Process is

   use Ada;
   use Ada.Exceptions;
   use POSIX;

   package PPP renames POSIX_Process_Primitives;
   package PPI renames POSIX_Process_Identification;
   use type PPP.Exit_Status;

--    IO_Path   : constant POSIX_String := "h:\Ada.Libraries\POSIX\CreateProcess";
--    IO_Binary : constant POSIX_String := IO_Path & "\io.exe";

   IO_Binary : constant POSIX_String := "io.exe";

   Input_Filename   : constant POSIX_String := "_input";
   Output_Filename  : constant POSIX_String := "_output";
   Null_Filename    : constant POSIX_String := "nul";

   IO_Process_Template   : PPP.Process_Template;
   IO_Process_ID         : PPI.Process_ID;
   IO_Process_Parameters : POSIX_String_List;

   IO_Process_Status     : PPP.Termination_Status;

   procedure Trace (M : in String) is
   begin
      Text_IO.Put_Line (M);
      Text_IO.Flush;
   end Trace;

begin
   Trace ("start of program");
   PPP.Open_Template (IO_Process_Template);
   PPP.Set_Keep_Effective_IDs (IO_Process_Template);

   PPP.Set_File_Action_To_Open (IO_Process_Template,
                                POSIX_IO.Standard_Input,
                                Input_Filename,
                                POSIX_IO.Write_Only);

   PPP.Set_File_Action_To_Open (IO_Process_Template,
                                POSIX_IO.Standard_Output,
                                Output_Filename,
                                POSIX_IO.Write_Only);

   PPP.Set_File_Action_To_Open (IO_Process_Template,
                                POSIX_IO.Standard_Error,
                                Null_Filename,
                                POSIX_IO.Write_Only);

   --  the first parameter is the program name
   Append (IO_Process_Parameters, IO_Binary);

   Append (IO_Process_Parameters, "p1");
   Append (IO_Process_Parameters, "p2");

   Trace ("start process");
   PPP.Start_Process (IO_Process_ID,
                      IO_Binary,
                      IO_Process_Template,
                      IO_Process_Parameters);

   -- Wait for it to complete.
   Trace ("wait for child");
   PPP.Wait_For_Child_Process (IO_Process_Status,
                               IO_Process_ID,
                               Trace_Stopped  => False,
                               Masked_Signals => All_Signals);

   -- Free memory.
   Trace ("close template");
   PPP.Close_Template (IO_Process_Template);
   Trace ("free process parameters");
   Make_Empty (IO_Process_Parameters);

   Trace ("test exit status");
   if PPP.Exit_Status_Of (IO_Process_Status) /= PPP.Normal_Exit then
      Trace ("raise exception");
      Raise_Exception (POSIX_Error'Identity,
                       Message => "the binary " & To_String (IO_Binary) &
                       " produce an error.");
   end if;

   Trace ("try to open output file");
   declare
      I_File : Text_IO.File_Type;
   begin
      Text_IO.Open (Name => To_String (Output_Filename),
                    Mode => Text_IO.In_File,
                    File => I_File);
   end;
   Trace ("end of program");
end POSIX_Create_Process;
