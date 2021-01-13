--  Back-end for translation.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Simple_IO;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Back_End;

package body Trans_Be is
   procedure Sem_Foreign (Decl : Iir)
   is
      use Translation;
      Fi : Foreign_Info_Type;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Architecture_Body =>
            Error_Msg_Sem (+Decl, "FOREIGN architectures are not yet handled");
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            null;
         when others =>
            Error_Kind ("sem_foreign", Decl);
      end case;
      --  Let it generate error messages.
      Fi := Translate_Foreign_Id (Decl);

      if Sem_Foreign_Hook /= null then
         Sem_Foreign_Hook.all (Decl, Fi);
      end if;
   end Sem_Foreign;

   function Parse_Option (Opt : String) return Boolean is
   begin
      if Opt = "--dump-drivers" then
         Translation.Flag_Dump_Drivers := True;
      elsif Opt = "--no-direct-drivers" then
         Translation.Flag_Direct_Drivers := False;
      elsif Opt = "--no-range-checks" then
         Translation.Flag_Range_Checks := False;
      elsif Opt = "--no-index-checks" then
         Translation.Flag_Index_Checks := False;
      elsif Opt = "--no-identifiers" then
         Translation.Flag_Discard_Identifiers := True;
      else
         return False;
      end if;
      return True;
   end Parse_Option;

   procedure Disp_Option
   is
      procedure P (Str : String) renames Simple_IO.Put_Line;
   begin
      P ("  --dump-drivers     dump processes drivers");
   end Disp_Option;

   procedure Register_Translation_Back_End is
   begin
      Vhdl.Back_End.Sem_Foreign := Sem_Foreign'Access;
      Vhdl.Back_End.Parse_Option := Parse_Option'Access;
      Vhdl.Back_End.Disp_Option := Disp_Option'Access;
   end Register_Translation_Back_End;
end Trans_Be;
