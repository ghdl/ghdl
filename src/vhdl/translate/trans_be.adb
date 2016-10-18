--  Back-end for translation.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Iirs; use Iirs;
with Translation;
with Errorout; use Errorout;
with Ada.Text_IO;
with Back_End;

package body Trans_Be is
   procedure Sem_Foreign (Decl : Iir)
   is
      use Translation;
      Fi : Foreign_Info_Type;
      pragma Unreferenced (Fi);
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
      --  Let is generate error messages.
      Fi := Translate_Foreign_Id (Decl);
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
      procedure P (Str : String) renames Ada.Text_IO.Put_Line;
   begin
      P ("  --dump-drivers     dump processes drivers");
   end Disp_Option;

   procedure Register_Translation_Back_End is
   begin
      Back_End.Sem_Foreign := Sem_Foreign'Access;
      Back_End.Parse_Option := Parse_Option'Access;
      Back_End.Disp_Option := Disp_Option'Access;
   end Register_Translation_Back_End;
end Trans_Be;
