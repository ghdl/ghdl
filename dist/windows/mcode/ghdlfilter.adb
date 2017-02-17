--
-- Preprocessor to handle library source metacomments
--
-- Limitations:
--   - line metacomments must occur at end of line with no trailing space before the line break
--   - block metacomments must start in column 1
--
-- Supported line metacomments:
--
--   --!V87
--   --V87
--   --V93
--   --V08
--
-- Supported block metacomments:
--
--   --START-!V87
--   --END-!V87
--
--   --START-V93
--   --END-V93
--
--   --START-V08
--   --END-V08
--
--
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Ghdlfilter is
   type Mode_Kind is (Mode_93, Mode_87, Mode_08);
   Mode : Mode_Kind;

   Line : String (1 .. 128);
   Len : Natural;

   Comment : Boolean;
   Block_Comment : Boolean;
begin
   if Argument_Count /= 1 then
      Put_Line (Standard_Error, "usage: " & Command_Name & " -v93|-v87|-v08");
      return;
   end if;

   if Argument (1) = "-v93" then
      Mode := Mode_93;
   elsif Argument (1) = "-v87" then
      Mode := Mode_87;
   elsif Argument (1) = "-v08" then
      Mode := Mode_08;
   else
      Put_Line (Standard_Error, "bad mode");
      return;
   end if;

   Block_Comment := False;

   loop
      exit when End_Of_File;
      Get_Line (Line, Len);

      Comment := Block_Comment;

      --
      -- look for line metacomments
      --
      if Len > 6 then

         if Mode = Mode_87 and ( Line (Len - 5 .. Len) = "--!V87" )  then
            Comment := True;
         end if;

      end if;


      if Len > 5 then

         if Mode = Mode_87    and ( (Line (Len - 4 .. Len) = "--V93") or (Line (Len - 4 .. Len) = "--V08") ) then
            Comment := True;

         elsif Mode = Mode_93 and ( (Line (Len - 4 .. Len) = "--V87") or (Line (Len - 4 .. Len) = "--V08") ) then
            Comment := True;

         elsif Mode = Mode_08 and ( (Line (Len - 4 .. Len) = "--V87") or (Line (Len - 4 .. Len) = "--V93") ) then
            Comment := True;
         end if;

      end if;

      --
      -- look for block metacomment start
      --
      if Len = 12
        and then Mode /= Mode_93
        and then Line (1 .. 12) = "--START-!V87" then
         Block_Comment := True;
      end if;

      if Len = 11
        and then Mode /= Mode_93
        and then Line (1 .. 11) = "--START-V93" then
         Block_Comment := True;
      end if;

      if Len = 11
        and then Mode /= Mode_08
        and then Line (1 .. 11) = "--START-V08" then
         Block_Comment := True;
      end if;

      --
      -- look for block metacomment end
      --
      if Len = 9 and then ( (Line (1 .. 9) = "--END-V93") or (Line (1 .. 9) = "--END-V08") )  then
         Block_Comment := False;
      end if;

      if Len = 10 and then ( Line (1 .. 10) = "--END-!V87" ) then
         Block_Comment := False;
      end if;

      --
      -- comment output lines as needed
      --
      if Comment then
         Put ("-- ");
      end if;

      Put_Line (Line (1 .. Len));

   end loop;
end Ghdlfilter;
