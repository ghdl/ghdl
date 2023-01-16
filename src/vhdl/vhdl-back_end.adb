--  Back-end specialization
--  Copyright (C) 2023 Tristan Gingold
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

with Types; use Types;
with Str_Table;
with Std_Names;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Sem_Specs;

package body Vhdl.Back_End is
   function Get_String_As_String (Expr : Iir) return String is
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_String_Literal8 =>
            declare
               Len : constant Natural := Natural (Get_String_Length (Expr));
               Id : constant String8_Id := Get_String8_Id (Expr);
               Res : String (1 .. Len);
            begin
               for I in 1 .. Len loop
                  Res (I) := Str_Table.Char_String8 (Id, Pos32 (I));
               end loop;
               return Res;
            end;
         when Iir_Kind_Simple_Aggregate =>
            declare
               List : constant Iir_Flist := Get_Simple_Aggregate_List (Expr);
               Len : constant Natural := Get_Nbr_Elements (List);
               Res : String (1 .. Len);
               El : Iir;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  pragma Assert (Get_Kind (El) = Iir_Kind_Enumeration_Literal);
                  Res (I - Flist_First + 1) :=
                    Character'Val (Get_Enum_Pos (El));
               end loop;
               return Res;
            end;
         when others =>
            if Get_Expr_Staticness (Expr) /= Locally then
               Error_Msg_Sem
                 (+Expr, "value of FOREIGN attribute must be locally static");
               return "";
            else
               raise Internal_Error;
            end if;
      end case;
   end Get_String_As_String;

   function Translate_Foreign_Id (Decl : Iir) return Foreign_Info_Type
   is
      --  Look for 'FOREIGN.
      Attr : constant Iir_Attribute_Value :=
        Vhdl.Sem_Specs.Find_Attribute_Value (Decl, Std_Names.Name_Foreign);
      pragma Assert (Attr /= Null_Iir);
      Spec : constant Iir_Attribute_Specification :=
        Get_Attribute_Specification (Attr);
      Name : constant String := Get_String_As_String (Get_Expression (Spec));
      Length : constant Natural := Name'Length;
   begin
      if Length = 0 then
         return Foreign_Bad;
      end if;

      pragma Assert (Name'First = 1);

      --  Only 'VHPIDIRECT' is recognized.
      if Length >= 10 and then Name (1 .. 10) = "VHPIDIRECT" then
         declare
            Info : Foreign_Info_Type (Foreign_Vhpidirect);
            P : Natural;
            Sf, Sl : Natural;
            Lf, Ll : Natural;
         begin
            P := 11;

            --  Skip spaces.
            while P <= Length and then Name (P) = ' ' loop
               P := P + 1;
            end loop;
            if P > Length then
               Error_Msg_Sem
                 (+Spec, "missing subprogram/library name after VHPIDIRECT");
               Info.Lib_Len := 0;
               Info.Subprg_Len := 0;
               return Info;
            end if;
            --  Extract library.
            Lf := P;
            while P <= Length and then Name (P) /= ' ' loop
               P := P + 1;
            end loop;
            Ll := P - 1;
            --  Extract subprogram.
            while P <= Length and then Name (P) = ' ' loop
               P := P + 1;
            end loop;
            Sf := P;
            while P <= Length and then Name (P) /= ' ' loop
               P := P + 1;
            end loop;
            Sl := P - 1;
            if P <= Length then
               Error_Msg_Sem (+Spec, "garbage at end of VHPIDIRECT");
            end if;

            --  Accept empty library.
            if Sf > Length then
               Sf := Lf;
               Sl := Ll;
               Lf := 1;
               Ll := 0;
            end if;

            Info.Lib_Len := Ll - Lf + 1;
            Info.Lib_Name (1 .. Info.Lib_Len) := Name (Lf .. Ll);

            Info.Subprg_Len := Sl - Sf + 1;
            Info.Subprg_Name (1 .. Info.Subprg_Len) := Name (Sf .. Sl);
            return Info;
         end;
      elsif Length = 14
        and then Name (1 .. 14) = "GHDL intrinsic"
      then
         return Foreign_Info_Type'(Kind => Foreign_Intrinsic);
      else
         Error_Msg_Sem
           (+Spec,
            "value of 'FOREIGN attribute does not begin with VHPIDIRECT");
         return Foreign_Bad;
      end if;
   end Translate_Foreign_Id;

   procedure Sem_Foreign_Wrapper (Decl : Iir)
   is
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
   end Sem_Foreign_Wrapper;
end Vhdl.Back_End;
