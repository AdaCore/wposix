
--  $Id$

with Win32.Stdlib;

with POSIX;
use POSIX;

package POSIX_Process_Environment is

   --  Process Parameters

   function Argument_List
     return POSIX.POSIX_String_List;

   --  Environment Variables

   type Environment is limited private;

   procedure Copy_From_Current_Environment
     (Env : in out Environment);

   procedure Copy_To_Current_Environment
     (Env : in Environment);

   procedure Copy_Environment
     (Source : in     Environment;
      Target : in out Environment);

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Env       : Environment;
      Undefined : POSIX.POSIX_String := "")
      return POSIX.POSIX_String;

   function Environment_Value_Of
     (Name      : POSIX.POSIX_String;
      Undefined : POSIX.POSIX_String := "")
      return POSIX.POSIX_String;

   function Is_Environment_Variable
     (Name      : POSIX.POSIX_String;
      Env       : Environment)
      return Boolean;

   function Is_Environment_Variable
     (Name      : POSIX.POSIX_String)
      return Boolean;

   procedure Clear_Environment
     (Env   : in out Environment);

   procedure Clear_Environment;

   procedure Set_Environment_Variable
     (Name  : in     POSIX.POSIX_String;
      Value : in     POSIX.POSIX_String;
      Env   : in out Environment);

   procedure Set_Environment_Variable
     (Name  : in     POSIX.POSIX_String;
      Value : in     POSIX.POSIX_String);

   procedure Delete_Environment_Variable
     (Name : in     POSIX.POSIX_String;
      Env  : in out Environment);

   procedure Delete_Environment_Variable
     (Name : in     POSIX.POSIX_String);

   function Length (Env : Environment)
                    return Natural;

   function Length
     return Natural;

   generic
      with procedure Action
        (Name  : in     POSIX.POSIX_String;
         Value : in     POSIX.POSIX_String;
         Quit  : in out Boolean);
   procedure For_Every_Environment_Variable
     (Env : in Environment);

   generic
      with procedure Action
        (Name  : in     POSIX.POSIX_String;
         Value : in     POSIX.POSIX_String;
         Quit  : in out Boolean);
   procedure For_Every_Current_Environment_Variable;

   --  Process Working Directory

   procedure Change_Working_Directory
     (Directory_Name : in POSIX.Pathname);

   function Get_Working_Directory
     return POSIX.Pathname;

private

   type Environment is new Win32.Stdlib.String_Array_Access;

end POSIX_Process_Environment;
