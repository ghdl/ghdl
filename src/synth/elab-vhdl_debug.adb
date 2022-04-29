--  Debug utilities on elaborated design
--  Copyright (C) 2019 Tristan Gingold
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
with Name_Table; use Name_Table;
with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;
with Libraries;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors;

package body Elab.Vhdl_Debug is
   procedure Disp_Discrete_Value (Val : Int64; Btype : Node) is
   begin
      case Get_Kind (Btype) is
         when Iir_Kind_Integer_Type_Definition =>
            Put_Int64 (Val);
         when Iir_Kind_Enumeration_Type_Definition =>
            declare
               Pos : constant Natural := Natural (Val);
               Enums : constant Node_Flist :=
                 Get_Enumeration_Literal_List (Btype);
               Id : constant Name_Id :=
                 Get_Identifier (Get_Nth_Element (Enums, Pos));
            begin
               Put (Name_Table.Image (Id));
            end;
         when others =>
            Vhdl.Errors.Error_Kind ("disp_discrete_value", Btype);
      end case;
   end Disp_Discrete_Value;

   procedure Disp_Value_Vector (Mem : Memtyp; A_Type: Node; Bound : Bound_Type)
   is
      El_Type : constant Node := Get_Base_Type (Get_Element_Subtype (A_Type));
      El_Typ : constant Type_Acc := Get_Array_Element (Mem.Typ);
      type Last_Enum_Type is (None, Char, Identifier);
      Last_Enum : Last_Enum_Type;
      Enum_List : Node_Flist;
      El_Id : Name_Id;
      El_Pos : Natural;
   begin
      --  Pretty print vectors of enumerated types
      if Get_Kind (El_Type) = Iir_Kind_Enumeration_Type_Definition then
         Last_Enum := None;
         Enum_List := Get_Enumeration_Literal_List (El_Type);
         for I in 1 .. Bound.Len loop
            El_Pos := Natural
              (Read_Discrete
                 (Memtyp'(El_Typ, Mem.Mem + Size_Type (I - 1) * El_Typ.Sz)));
            El_Id := Get_Identifier (Get_Nth_Element (Enum_List, El_Pos));
            if Name_Table.Is_Character (El_Id) then
               case Last_Enum is
                  when None =>
                     Put ("""");
                  when Identifier =>
                     Put (" & """);
                  when Char =>
                     null;
               end case;
               Put (Name_Table.Get_Character (El_Id));
               Last_Enum := Char;
            else
               case Last_Enum is
                  when None =>
                     null;
                  when Identifier =>
                     Put (" & ");
                  when Char =>
                     Put (""" & ");
               end case;
               Put (Name_Table.Image (El_Id));
               Last_Enum := Identifier;
            end if;
         end loop;
         case Last_Enum is
            when None =>
               Put ("""""");  --  Simply ""
            when Identifier =>
               null;
            when Char =>
               Put ("""");
         end case;
      else
         Put ("(");
         for I in 1 .. Bound.Len loop
            if I /= 1 then
               Put (", ");
            end if;
            Disp_Memtyp ((El_Typ, Mem.Mem + Size_Type (I - 1) * Mem.Typ.Sz),
                         El_Type);
         end loop;
         Put (")");
      end if;
   end Disp_Value_Vector;

   procedure Disp_Value_Array (Mem : Memtyp; A_Type: Node; Dim: Dim_Type)
   is
      Stride : Size_Type;
   begin
      if Dim = Mem.Typ.Abounds.Ndim then
         --  Last dimension
         Disp_Value_Vector (Mem, A_Type, Mem.Typ.Abounds.D (Dim));
      else
         Stride := Mem.Typ.Arr_El.Sz;
         for I in Dim + 1 .. Mem.Typ.Abounds.Ndim loop
            Stride := Stride * Size_Type (Mem.Typ.Abounds.D (I).Len);
         end loop;

         Put ("(");
         for I in 1 .. Mem.Typ.Abounds.D (Dim).Len loop
            if I /= 1 then
               Put (", ");
            end if;
            Disp_Value_Array ((Mem.Typ, Mem.Mem + Stride), A_Type, Dim + 1);
         end loop;
         Put (")");
      end if;
   end Disp_Value_Array;

   procedure Disp_Memtyp (M : Memtyp; Vtype : Node) is
   begin
      if M.Mem = null then
         Put ("*NULL*");
         return;
      end if;

      case M.Typ.Kind is
         when Type_Discrete
           | Type_Bit
           | Type_Logic =>
            Disp_Discrete_Value (Read_Discrete (M), Get_Base_Type (Vtype));
         when Type_Vector =>
            Disp_Value_Vector (M, Vtype, M.Typ.Vbound);
         when Type_Array =>
            Disp_Value_Array (M, Vtype, 1);
         when Type_Float =>
            Put ("*float*");
         when Type_Slice =>
            Put ("*slice*");
         when Type_File =>
            Put ("*file*");
         when Type_Record =>
            Put ("*record*");
         when Type_Access =>
            Put ("*access*");
         when Type_Protected =>
            Put ("*protected*");
         when Type_Unbounded_Array
            | Type_Unbounded_Record
            | Type_Unbounded_Vector =>
            Put ("*unbounded*");
      end case;
   end Disp_Memtyp;

   procedure Disp_Value (Vt : Valtyp; Vtype : Node) is
   begin
      if Vt.Val = null then
         Put ("*NULL*");
         return;
      end if;

      case Vt.Val.Kind is
         when Value_Net =>
            Put ("net");
         when Value_Wire =>
            Put ("wire");
         when Value_Signal =>
            Put ("signal");
         when Value_File =>
            Put ("file");
         when Value_Const =>
            Put ("const: ");
            Disp_Memtyp (Get_Memtyp (Vt), Vtype);
         when Value_Alias =>
            Put ("alias");
            Disp_Memtyp (Get_Memtyp (Vt), Vtype);
         when Value_Memory =>
            Disp_Memtyp (Get_Memtyp (Vt), Vtype);
      end case;
   end Disp_Value;

   procedure Disp_Bound_Type (Bound : Bound_Type) is
   begin
      Put_Int32 (Bound.Left);
      Put (' ');
      case Bound.Dir is
         when Dir_To =>
            Put ("to");
         when Dir_Downto =>
            Put ("downto");
      end case;
      Put (' ');
      Put_Int32 (Bound.Right);
   end Disp_Bound_Type;

   procedure Disp_Type (Typ : Type_Acc; Vtype : Node)
   is
      pragma Unreferenced (Vtype);
   begin
      case Typ.Kind is
         when Type_Bit =>
            Put ("bit");
         when Type_Logic =>
            Put ("logic");
         when Type_Discrete =>
            Put ("discrete");
         when Type_Float =>
            Put ("float");
         when Type_Vector =>
            Put ("vector (");
            Disp_Bound_Type (Typ.Vbound);
            Put (')');
         when Type_Unbounded_Vector =>
            Put ("unbounded_vector");
         when Type_Array =>
            Put ("array");
         when Type_Unbounded_Array =>
            Put ("unbounded_array");
         when Type_Unbounded_Record =>
            Put ("unbounded_record");
         when Type_Record =>
            Put ("record");
         when Type_Slice =>
            Put ("slice");
         when Type_Access =>
            Put ("access");
         when Type_File =>
            Put ("file");
         when Type_Protected =>
            Put ("protected");
      end case;
   end Disp_Type;

   procedure Disp_Declaration_Object
     (Instance : Synth_Instance_Acc; Decl : Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_File_Declaration =>
            declare
               Val : constant Valtyp := Get_Value (Instance, Decl);
               Dtype : constant Node := Get_Type (Decl);
            begin
               Put (Vhdl.Errors.Disp_Node (Decl));
               Put (": ");
               Disp_Type (Val.Typ, Dtype);
               Put (" = ");
               Disp_Value (Val, Dtype);
               New_Line;
            end;
         when Iir_Kinds_Signal_Attribute =>
            --  FIXME: todo ?
            null;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            --  FIXME: disp ranges
            null;
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Procedure_Body =>
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("disp_declaration_object", Decl);
      end case;
   end Disp_Declaration_Object;

   procedure Disp_Declaration_Objects
     (Instance : Synth_Instance_Acc; Decl_Chain : Iir)
   is
      El : Iir;
   begin
      El := Decl_Chain;
      while El /= Null_Iir loop
         Disp_Declaration_Object (Instance, El);
         El := Get_Chain (El);
      end loop;
   end Disp_Declaration_Objects;

   procedure Disp_Hierarchy_Statements
     (Inst : Synth_Instance_Acc; Stmts : Node; Indent : Natural)
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Stmt /= Null_Node loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Component_Instantiation_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
                  Sub_Node : constant Node := Get_Source_Scope (Sub);
                  Comp_Inst : Synth_Instance_Acc;
               begin
                  Put_Indent (Indent);
                  Put (Image (Get_Label (Stmt)));
                  case Get_Kind (Sub_Node) is
                     when Iir_Kind_Component_Declaration =>
                        Put (": component ");
                        Put (Image (Get_Identifier (Sub_Node)));
                        Comp_Inst := Get_Component_Instance (Sub);
                        if Comp_Inst = null then
                           Put_Line (" [not bound]");
                        else
                           New_Line;
                           Disp_Hierarchy (Comp_Inst, Indent + 1, False);
                        end if;
                     when Iir_Kind_Architecture_Body =>
                        Put (": entity ");
                        Put (Image (Get_Identifier (Get_Entity (Sub_Node))));
                        Put ('(');
                        Put (Image (Get_Identifier (Sub_Node)));
                        Put (')');
                        New_Line;
                        Disp_Hierarchy_Statements
                          (Sub,
                           Get_Concurrent_Statement_Chain (Sub_Node),
                           Indent + 1);
                     when others =>
                        raise Internal_Error;
                  end case;
               end;
            when Iir_Kind_If_Generate_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
               begin
                  if Sub = null then
                     return;
                  end if;
                  Put_Indent (Indent);
                  Put (Image (Get_Label (Stmt)));
                  Put (": if-generate");
                  if Sub = null then
                     Put_Line (" [false]");
                  else
                     Put_Line (" [true]");
                     Disp_Hierarchy (Sub, Indent + 1, False);
                  end if;
               end;
            when Iir_Kind_For_Generate_Statement =>
               declare
                  It : constant Node := Get_Parameter_Specification (Stmt);
                  It_Rng : Type_Acc;
                  It_Len : Natural;
                  Gen_Inst : Synth_Instance_Acc;
               begin
                  Put_Indent (Indent);
                  Put (Image (Get_Label (Stmt)));
                  Put (": for-generate");
                  New_Line;

                  It_Rng := Get_Subtype_Object (Inst, Get_Type (It));
                  It_Len := Natural (Get_Range_Length (It_Rng.Drange));
                  Gen_Inst := Get_Sub_Instance (Inst, Stmt);
                  for I in 1 .. It_Len loop
                     Disp_Hierarchy (Get_Generate_Sub_Instance (Gen_Inst, I),
                                     Indent + 1, False);
                  end loop;
               end;
            when Iir_Kind_Block_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
               begin
                  Put_Indent (Indent);
                  Put (Image (Get_Label (Stmt)));
                  Put_Line (": block");
                  Disp_Hierarchy_Statements
                    (Sub, Get_Concurrent_Statement_Chain (Stmt), Indent + 1);
               end;
            when Iir_Kinds_Concurrent_Signal_Assignment
              | Iir_Kind_Concurrent_Assertion_Statement
              | Iir_Kind_Concurrent_Procedure_Call_Statement =>
               null;
            when Iir_Kinds_Process_Statement =>
               null;
            when others =>
               Vhdl.Errors.Error_Kind ("disp_hierarchy_statements", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Disp_Hierarchy_Statements;

   procedure Disp_Hierarchy
     (Inst : Synth_Instance_Acc; Indent : Natural; With_Objs : Boolean)
   is
      N : constant Node := Get_Source_Scope (Inst);
   begin
      case Get_Kind (N) is
         when Iir_Kind_Architecture_Body =>
            Put_Indent (Indent);
            Put ("architecture ");
            Put (Image (Get_Identifier (N)));
            Put (" of ");
            Put (Image (Get_Identifier (Get_Entity (N))));
            New_Line;
            Disp_Hierarchy_Statements
              (Inst, Get_Concurrent_Statement_Chain (N), Indent + 1);
         when Iir_Kind_Component_Declaration =>
            Put_Indent (Indent);
            Put ("component ");
            Put (Image (Get_Identifier (N)));
            New_Line;
            Disp_Hierarchy
              (Get_Component_Instance (Inst), Indent + 1, With_Objs);
         when Iir_Kind_Generate_Statement_Body =>
            Disp_Hierarchy_Statements
              (Inst, Get_Concurrent_Statement_Chain (N), Indent);
         when others =>
            Vhdl.Errors.Error_Kind ("disp_hierarchy", N);
      end case;
   end Disp_Hierarchy;

   function Walk_Files (Cb : Walk_Cb) return Walk_Status
   is
      Lib : Iir_Library_Declaration := Libraries.Get_Libraries_Chain;
      File : Iir_Design_File;
   begin
      while Lib /= Null_Iir loop
         File := Get_Design_File_Chain (Lib);
         while File /= Null_Iir loop
            case Cb.all (File) is
               when Walk_Continue =>
                  null;
               when Walk_Up =>
                  exit;
               when Walk_Abort =>
                  return Walk_Abort;
            end case;
            File := Get_Chain (File);
         end loop;
         Lib := Get_Chain (Lib);
      end loop;
      return Walk_Continue;
   end Walk_Files;

   Walk_Units_Cb : Walk_Cb;

   function Cb_Walk_Units (Design_File : Iir) return Walk_Status
   is
      Unit : Iir_Design_Unit;
   begin
      Unit := Get_First_Design_Unit (Design_File);
      while Unit /= Null_Iir loop
         case Walk_Units_Cb.all (Get_Library_Unit (Unit)) is
            when Walk_Continue =>
               null;
            when Walk_Abort =>
               return Walk_Abort;
            when Walk_Up =>
               exit;
         end case;
         Unit := Get_Chain (Unit);
      end loop;
      return Walk_Continue;
   end Cb_Walk_Units;

   function Walk_Units (Cb : Walk_Cb) return Walk_Status is
   begin
      Walk_Units_Cb := Cb;
      return Walk_Files (Cb_Walk_Units'Access);
   end Walk_Units;

   Walk_Declarations_Cb : Walk_Cb;

   function Cb_Walk_Declarations (Unit : Iir) return Walk_Status
   is
      function Walk_Decl_Chain (Chain : Iir) return Walk_Status
      is
         Decl : Iir;
      begin
         Decl := Chain;
         while Decl /= Null_Iir loop
            case Walk_Declarations_Cb.all (Decl) is
               when Walk_Abort =>
                  return Walk_Abort;
               when Walk_Up =>
                  return Walk_Continue;
               when Walk_Continue =>
                  null;
            end case;
            Decl := Get_Chain (Decl);
         end loop;
         return Walk_Continue;
      end Walk_Decl_Chain;

      function Walk_Conc_Chain (Chain : Iir) return Walk_Status;

      function Walk_Generate_Statement_Body (Bod : Iir) return Walk_Status is
      begin
         if Walk_Decl_Chain (Get_Declaration_Chain (Bod)) = Walk_Abort then
            return Walk_Abort;
         end if;
         if Walk_Conc_Chain (Get_Concurrent_Statement_Chain (Bod)) = Walk_Abort
         then
            return Walk_Abort;
         end if;
         return Walk_Continue;
      end Walk_Generate_Statement_Body;

      function Walk_Conc_Chain (Chain : Iir) return Walk_Status
      is
         Stmt : Iir := Chain;
      begin
         while Stmt /= Null_Iir loop
            case Get_Kind (Stmt) is
               when Iir_Kinds_Process_Statement =>
                  if Walk_Decl_Chain (Get_Declaration_Chain (Stmt))
                    = Walk_Abort
                  then
                     return Walk_Abort;
                  end if;
               when Iir_Kind_For_Generate_Statement =>
                  if Walk_Declarations_Cb.all
                    (Get_Parameter_Specification (Stmt)) = Walk_Abort
                    or else Walk_Generate_Statement_Body
                    (Get_Generate_Statement_Body (Stmt)) = Walk_Abort
                  then
                     return Walk_Abort;
                  end if;
               when Iir_Kind_If_Generate_Statement =>
                  declare
                     Stmt1 : Iir;
                  begin
                     Stmt1 := Stmt;
                     while Stmt1 /= Null_Iir loop
                        if Walk_Generate_Statement_Body
                          (Get_Generate_Statement_Body (Stmt)) = Walk_Abort
                        then
                           return Walk_Abort;
                        end if;
                        Stmt1 := Get_Generate_Else_Clause (Stmt1);
                     end loop;
                  end;
               when Iir_Kind_Component_Instantiation_Statement
                 | Iir_Kind_Concurrent_Simple_Signal_Assignment =>
                  null;
               when Iir_Kind_Block_Statement =>
                  --  FIXME: header
                  if (Walk_Decl_Chain
                        (Get_Declaration_Chain (Stmt)) = Walk_Abort)
                    or else
                    (Walk_Conc_Chain
                       (Get_Concurrent_Statement_Chain (Stmt)) = Walk_Abort)
                  then
                     return Walk_Abort;
                  end if;
               when others =>
                  Vhdl.Errors.Error_Kind ("walk_conc_chain", Stmt);
            end case;
            Stmt := Get_Chain (Stmt);
         end loop;
         return Walk_Continue;
      end Walk_Conc_Chain;
   begin
      case Get_Kind (Unit) is
         when Iir_Kind_Entity_Declaration =>
            if Walk_Decl_Chain (Get_Generic_Chain (Unit)) = Walk_Abort
              or else Walk_Decl_Chain (Get_Port_Chain (Unit)) = Walk_Abort
              or else (Walk_Decl_Chain
                         (Get_Declaration_Chain (Unit)) = Walk_Abort)
              or else (Walk_Conc_Chain
                         (Get_Concurrent_Statement_Chain (Unit)) = Walk_Abort)
            then
               return Walk_Abort;
            end if;
         when Iir_Kind_Architecture_Body =>
            if (Walk_Decl_Chain
                  (Get_Declaration_Chain (Unit)) = Walk_Abort)
              or else (Walk_Conc_Chain
                         (Get_Concurrent_Statement_Chain (Unit)) = Walk_Abort)
            then
               return Walk_Abort;
            end if;
         when Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body =>
            if Walk_Decl_Chain (Get_Declaration_Chain (Unit)) = Walk_Abort
            then
               return Walk_Abort;
            end if;
         when Iir_Kind_Configuration_Declaration =>
            if Walk_Decl_Chain (Get_Declaration_Chain (Unit)) = Walk_Abort
            then
               return Walk_Abort;
            end if;
            --  FIXME: block configuration ?
         when Iir_Kind_Context_Declaration =>
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("Cb_Walk_Declarations", Unit);
      end case;
      return Walk_Continue;
   end Cb_Walk_Declarations;

   function Walk_Declarations (Cb : Walk_Cb) return Walk_Status is
   begin
      Walk_Declarations_Cb := Cb;
      return Walk_Units (Cb_Walk_Declarations'Access);
   end Walk_Declarations;

end Elab.Vhdl_Debug;
