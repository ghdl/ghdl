with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Ghdlfilter is
   type Mode_Kind is (Mode_93, Mode_87);
   Mode : Mode_Kind;

   Line : String (1 .. 128);
   Len : Natural;

   Comment : Boolean;
   Block_Comment : Boolean;
begin
   if Argument_Count /= 1 then
      Put_Line (Standard_Error, "usage: " & Command_Name & " -v93|-v87");
      return;
   end if;

   if Argument (1) = "-v93" then
      Mode := Mode_93;
   elsif Argument (1) = "-v87" then
      Mode := Mode_87;
   else
      Put_Line (Standard_Error, "bad mode");
      return;
   end if;

   Block_Comment := False;

   loop
      exit when End_Of_File;
      Get_Line (Line, Len);

      Comment := Block_Comment;

      if Len > 5 then
         if Mode = Mode_87 and Line (Len - 4 .. Len) = "--V93" then
            Comment := True;
         elsif Mode = Mode_93 and Line (Len - 4 .. Len) = "--V87" then
            Comment := True;
         end if;
      end if;
      if Len = 11
        and then Mode = Mode_87
        and then Line (1 .. 11) = "--START-V93" then
         Block_Comment := True;
      end if;

      if Len = 9 and then Line (1 .. 9) = "--END-V93" then
         Block_Comment := False;
      end if;

      if Comment then
         Put ("-- ");
      end if;
      Put_Line (Line (1 .. Len));
   end loop;
end Ghdlfilter;
