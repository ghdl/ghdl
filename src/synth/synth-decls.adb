--  Create declarations for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Types; use Types;
with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;
with Errorout; use Errorout;
with Synth.Context; use Synth.Context;
with Synth.Types; use Synth.Types;
with Synth.Environment; use Synth.Environment;
with Simul.Environments; use Simul.Environments;
with Simul.Annotations; use Simul.Annotations;

package body Synth.Decls is
   procedure Create_Var_Wire
     (Syn_Inst : Synth_Instance_Acc; Decl : Iir; Init : Iir_Value_Literal_Acc)
   is
      Val : constant Value_Acc := Get_Value (Syn_Inst, Decl);
      Value : Net;
      Ival : Net;
      W : Width;
      Name : Sname;
   begin
      case Val.Kind is
         when Value_Wire =>
            W := Get_Width (Syn_Inst, Get_Type (Decl));
            Name := New_Sname (Syn_Inst.Name, Get_Identifier (Decl));
            if Init /= null then
               Ival := Get_Net (Create_Value_Lit (Init, Get_Type (Decl)));
               pragma Assert (Get_Width (Ival) = W);
               Value := Build_Isignal (Build_Context, Name, Ival);
            else
               Value := Build_Signal (Build_Context, Name, W);
            end if;
            Wire_Id_Table.Table (Val.W).Gate := Value;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Var_Wire;

   procedure Synth_Declaration (Syn_Inst : Synth_Instance_Acc; Decl : Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Variable_Declaration =>
            declare
               Def : constant Iir := Get_Default_Value (Decl);
               Slot : constant Object_Slot_Type := Get_Info (Decl).Slot;
               Init : Iir_Value_Literal_Acc;
            begin
               Make_Object (Syn_Inst, Wire_Variable, Decl);
               if Is_Valid (Def) then
                  Init := Syn_Inst.Sim.Objects (Slot);
               else
                  Init := null;
               end if;
               Create_Var_Wire (Syn_Inst, Decl, Init);
            end;
         when Iir_Kind_Interface_Variable_Declaration =>
            --  Ignore default value.
            Make_Object (Syn_Inst, Wire_Variable, Decl);
            Create_Var_Wire (Syn_Inst, Decl, null);
         when Iir_Kind_Signal_Declaration =>
            declare
               Def : constant Iir := Get_Default_Value (Decl);
               Slot : constant Object_Slot_Type := Get_Info (Decl).Slot;
               Init : Iir_Value_Literal_Acc;
            begin
               Make_Object (Syn_Inst, Wire_Signal, Decl);
               if Is_Valid (Def) then
                  Init := Syn_Inst.Sim.Objects (Slot + 1);
               else
                  Init := null;
               end if;
               Create_Var_Wire (Syn_Inst, Decl, Init);
            end;
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            --  TODO: elaborate interfaces
            null;
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            null;
         when others =>
            Error_Kind ("synth_declaration", Decl);
      end case;
   end Synth_Declaration;

   procedure Synth_Declarations (Syn_Inst : Synth_Instance_Acc; Decls : Iir)
   is
      Decl : Iir;
   begin
      Decl := Decls;
      while Is_Valid (Decl) loop
         Synth_Declaration (Syn_Inst, Decl);

         Decl := Get_Chain (Decl);
      end loop;
   end Synth_Declarations;
end Synth.Decls;
