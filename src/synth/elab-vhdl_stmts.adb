--  Elaborate statements
--  Copyright (C) 2021 Tristan Gingold
--
--  This file is part of GHDL.
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

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Decls; use Elab.Vhdl_Decls;
with Elab.Vhdl_Insts; use Elab.Vhdl_Insts;

with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;

package body Elab.Vhdl_Stmts is
   function Elab_Generate_Statement_Body (Syn_Inst : Synth_Instance_Acc;
                                          Bod : Node;
                                          Config : Node;
                                          Iterator : Node := Null_Node;
                                          Iterator_Val : Valtyp := No_Valtyp)
                                         return Synth_Instance_Acc
   is
      Decls_Chain : constant Node := Get_Declaration_Chain (Bod);
      Bod_Inst : Synth_Instance_Acc;
   begin
      Bod_Inst := Make_Elab_Instance (Syn_Inst, Bod, Config);

      if Iterator /= Null_Node then
         --  Add the iterator (for for-generate).
         Create_Object (Bod_Inst, Iterator, Iterator_Val);
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      Elab_Declarations (Bod_Inst, Decls_Chain);

      pragma Assert (Is_Expr_Pool_Empty);

      Elab_Concurrent_Statements
        (Bod_Inst, Get_Concurrent_Statement_Chain (Bod));

      return Bod_Inst;
   end Elab_Generate_Statement_Body;

   procedure Elab_For_Generate_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Iterator : constant Node := Get_Parameter_Specification (Stmt);
      Bod : constant Node := Get_Generate_Statement_Body (Stmt);
      Configs : constant Node := Get_Generate_Block_Configuration (Bod);
      It_Type : constant Node := Get_Declaration_Type (Iterator);
      Gen_Inst : Synth_Instance_Acc;
      Sub_Inst : Synth_Instance_Acc;
      Config : Node;
      It_Rng : Type_Acc;
      Val : Valtyp;
      Dval : Int64;
      Len : Uns32;
   begin
      if It_Type /= Null_Node then
         Synth_Subtype_Indication (Syn_Inst, It_Type);
      end if;

      --  Initial value.
      It_Rng := Get_Subtype_Object (Syn_Inst, Get_Type (Iterator));
      Len := Get_Range_Length (It_Rng.Drange);
      Dval := It_Rng.Drange.Left;

      Gen_Inst := Make_Elab_Generate_Instance
        (Syn_Inst, Stmt, Configs, Natural (Len));

      Create_Sub_Instance (Syn_Inst, Stmt, Gen_Inst);

      for I in 1 .. Len loop
         --  Find and apply the config block.
         declare
            Spec : Node;
            Default : Node;
            Idxes : Node_Flist;
            Drng : Discrete_Range_Type;
            Marker : Mark_Type;
         begin
            --  TODO: do not recompute indexes and ranges for each sub-block
            Mark_Expr_Pool (Marker);
            Default := Null_Node;
            Config := Configs;
            while Config /= Null_Node loop
               Spec := Get_Block_Specification (Config);
               case Get_Kind (Spec) is
                  when Iir_Kind_Simple_Name =>
                     exit;
                  when Iir_Kind_Indexed_Name =>
                     Idxes := Get_Index_List (Spec);
                     if Idxes = Iir_Flist_Others then
                        Default := Config;
                     else
                        Val := Synth_Expression_With_Type
                          (Syn_Inst, Get_Nth_Element (Idxes, 0), It_Rng);
                        exit when Read_Discrete (Val) = Dval;
                     end if;
                  when Iir_Kind_Slice_Name =>
                     Synth_Discrete_Range (Syn_Inst, Get_Suffix (Spec), Drng);
                     exit when In_Range (Drng, Dval);
                  when others =>
                     Error_Kind ("elab_for_generate_statement", Spec);
               end case;
               Config := Get_Prev_Block_Configuration (Config);
            end loop;
            Release_Expr_Pool (Marker);

            if Config = Null_Node then
               Config := Default;
            end if;
            Apply_Block_Configuration (Config, Bod);
         end;

         --  Allocate the iterator value for the body.
         Current_Pool := Instance_Pool;
         Val := Create_Value_Discrete (Dval, It_Rng);
         Current_Pool := Expr_Pool'Access;

         Sub_Inst := Elab_Generate_Statement_Body
           (Gen_Inst, Bod, Config, Iterator, Val);
         Set_Generate_Sub_Instance (Gen_Inst, Positive (I), Sub_Inst);

         --  Update index.
         case It_Rng.Drange.Dir is
            when Dir_To =>
               Dval := Dval + 1;
            when Dir_Downto =>
               Dval := Dval - 1;
         end case;
      end loop;
   end Elab_For_Generate_Statement;

   procedure Elab_If_Generate_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Marker : Mark_Type;
      Gen : Node;
      Bod : Node;
      Icond : Node;
      Cond : Valtyp;
      Cond_Val : Boolean;
      Config : Node;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Mark_Expr_Pool (Marker);
      Gen := Stmt;

      loop
         Icond := Get_Condition (Gen);
         if Icond /= Null_Node then
            Cond := Synth_Expression (Syn_Inst, Icond);
            Strip_Const (Cond);
            Cond_Val := Read_Discrete (Cond) = 1;
         else
            --  It is the else generate.
            Cond_Val := True;
         end if;
         Release_Expr_Pool (Marker);

         if Cond_Val then
            Bod := Get_Generate_Statement_Body (Gen);
            Config := Get_Generate_Block_Configuration (Bod);

            Apply_Block_Configuration (Config, Bod);
            Sub_Inst := Elab_Generate_Statement_Body (Syn_Inst, Bod, Config);
            Create_Sub_Instance (Syn_Inst, Bod, Sub_Inst);
            return;
         end if;
         Gen := Get_Generate_Else_Clause (Gen);
         exit when Gen = Null_Node;
      end loop;

      --  Not generated.
      Create_Sub_Instance (Syn_Inst, Stmt, null);
   end Elab_If_Generate_Statement;

   procedure Elab_Block_Statement (Syn_Inst : Synth_Instance_Acc; Blk : Node)
   is
      Hdr : constant Node := Get_Block_Header (Blk);
      Guard : constant Node := Get_Guard_Decl (Blk);
      Blk_Inst : Synth_Instance_Acc;
      Assoc : Node;
      Inter : Node;
   begin
      Apply_Block_Configuration
        (Get_Block_Block_Configuration (Blk), Blk);

      Blk_Inst := Make_Elab_Instance (Syn_Inst, Blk, Null_Iir);
      Create_Sub_Instance (Syn_Inst, Blk, Blk_Inst);

      if Guard /= Null_Node then
         Create_Signal (Blk_Inst, Guard, Boolean_Type, null);
      end if;

      if Hdr /= Null_Node then
         Inter := Get_Generic_Chain (Hdr);
         if Inter /= Null_Node then
            Assoc := Get_Generic_Map_Aspect_Chain (Hdr);
            Elab_Generics_Association (Blk_Inst, Syn_Inst, Inter, Assoc);
         end if;

         Inter := Get_Port_Chain (Hdr);
         if Inter /= Null_Node then
            Assoc := Get_Port_Map_Aspect_Chain (Hdr);
            Elab_Ports_Association_Type (Blk_Inst, Syn_Inst, Inter, Assoc);
         end if;
      end if;

      Elab_Declarations (Blk_Inst, Get_Declaration_Chain (Blk));
      Elab_Concurrent_Statements
        (Blk_Inst, Get_Concurrent_Statement_Chain (Blk));
   end Elab_Block_Statement;

   procedure Elab_Concurrent_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node) is
   begin
      case Get_Kind (Stmt) is
         when Iir_Kinds_Process_Statement =>
            null;
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Declaration =>
            null;
         when Iir_Kind_Component_Instantiation_Statement =>
            if Is_Component_Instantiation (Stmt) then
               Elab_Component_Instantiation_Statement (Syn_Inst, Stmt);
            else
               Elab_Design_Instantiation_Statement (Syn_Inst, Stmt);
            end if;

         when Iir_Kind_For_Generate_Statement =>
            Elab_For_Generate_Statement (Syn_Inst, Stmt);

         when Iir_Kind_If_Generate_Statement =>
            Elab_If_Generate_Statement (Syn_Inst, Stmt);

         when Iir_Kind_Block_Statement =>
            Elab_Block_Statement (Syn_Inst, Stmt);

         when others =>
            Error_Kind ("elab_concurrent_statement", Stmt);
      end case;
      pragma Assert (Is_Expr_Pool_Empty);
   end Elab_Concurrent_Statement;

   procedure Elab_Concurrent_Statements
     (Syn_Inst : Synth_Instance_Acc; Chain : Node)
   is
      Stmt : Node;
   begin
      if Chain = Null_Node then
         return;
      end if;

      Stmt := Chain;
      while Stmt /= Null_Node loop
         Elab_Concurrent_Statement (Syn_Inst, Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Elab_Concurrent_Statements;
end Elab.Vhdl_Stmts;
