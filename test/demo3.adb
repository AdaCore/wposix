
with Ada.Text_IO;
with Interfaces.C.Strings;
with Win32;
with Win32.Stdlib;

procedure Demo3 is

   use Ada.Text_IO;
   use Interfaces;
   use Win32;

   procedure Meth1 is

      type String_Array is access all Win32.PSTR;
      pragma Convention (C, String_Array);

      type SAP is access String_Array;

      function environ_addr return SAP;
      pragma Import (C, environ_addr, "__environ_addr");

      environ : String_Array := environ_addr.all;
      pragma Convention (C, Environ);

      Vc : C.Strings.Chars_Ptr := To_Chars_Ptr (Environ.all);
      Va : String              := C.Strings.Value (Vc);

   begin
      Put_Line (Va);
   end Meth1;

   procedure Meth2 is

      pragma Suppress (Index_Check);

      type String_Array is array (Integer range 0 .. -1) of Win32.PSTR;
      pragma Convention (C, String_Array);

      type String_Array_Access is access String_Array;
      type SAP is access String_Array_Access;

      function environ_addr return SAP;
      pragma Import (C, environ_addr, "__environ_addr");

      environ : String_Array_Access := Environ_Addr.all;
      pragma Convention (C, Environ);

      I : Integer := 0;

   begin
      loop
         exit when Environ (I) = null;
         declare
            Vc : C.Strings.Chars_Ptr := To_Chars_Ptr (Environ (I));
            Va : String              := C.Strings.Value (Vc);
         begin
            Put_Line (Va);
         end;
         I := I + 1;
      end loop;
   end Meth2;

   procedure Display_Argv is
      pragma Suppress (Index_Check);
      I : Natural := 0;
   begin
      loop
         exit when Stdlib.Argv (I) = null;
         declare
            Vc : C.Strings.Chars_Ptr := To_Chars_Ptr (Stdlib.Argv (I));
            Va : String              := C.Strings.Value (Vc);
         begin
            Put_Line (Va);
         end;
         I := I + 1;
      end loop;
   end Display_Argv;

   procedure Display_Sys_ErrList is
      pragma Suppress (Index_Check);
      I : Natural := 0;
   begin
      for I in Integer range 0 .. Integer (Stdlib.Sys_Nerr) - 1 loop
         declare
            Vc : C.Strings.Chars_Ptr := To_Chars_Ptr (Stdlib.Sys_ErrList (I));
            Va : String              := C.Strings.Value (Vc);
         begin
            Put_Line (Va);
         end;
      end loop;
   end Display_Sys_ErrList;

begin
   Put_Line ("Argc = " & INT'Image (Stdlib.Argc));
   Display_Argv;
   New_Line;

   Display_Sys_ErrList;
   New_Line;

   Put_Line ("Meth1");
   Meth1;
   New_Line;

   Put_Line ("Meth2");
   Meth2;
end Demo3;
