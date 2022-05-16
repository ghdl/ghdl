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

with Elab.Debugger;
with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Values.Debug; use Elab.Vhdl_Values.Debug;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors;

package body Elab.Vhdl_Debug is
   procedure Disp_Discrete_Value (Val : Int64; Btype : Node) is
   begin
      case Get_Kind (Btype) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Integer_Subtype_Definition =>
            Put_Int64 (Val);
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
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
            Disp_Memtyp ((El_Typ, Mem.Mem + Size_Type (I - 1) * El_Typ.Sz),
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
            Put (' ');
            Put_Uns32 (Vt.Val.S);
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
      Put_Dir (Bound.Dir);
      Put (' ');
      Put_Int32 (Bound.Right);
   end Disp_Bound_Type;

   procedure Disp_Discrete_Range (Rng : Discrete_Range_Type; Vtype : Node) is
   begin
      Disp_Discrete_Value (Rng.Left, Vtype);
      Put (' ');
      Put_Dir (Rng.Dir);
      Put (' ');
      Disp_Discrete_Value (Rng.Right, Vtype);
   end Disp_Discrete_Range;

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
     (Instance : Synth_Instance_Acc; Decl : Iir; Indent : Natural) is
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
               Put_Indent (Indent);
               Put (Vhdl.Errors.Disp_Node (Decl));
               Put (": ");
               Disp_Type (Val.Typ, Dtype);
               Put (" = ");
               Disp_Value (Val, Dtype);
               New_Line;
            end;
         when Iir_Kinds_Signal_Attribute
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Attribute_Specification =>
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
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Component_Declaration =>
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("disp_declaration_object", Decl);
      end case;
   end Disp_Declaration_Object;

   procedure Disp_Declaration_Objects
     (Instance : Synth_Instance_Acc; Decl_Chain : Iir; Indent : Natural := 0)
   is
      El : Iir;
   begin
      El := Decl_Chain;
      while El /= Null_Iir loop
         Disp_Declaration_Object (Instance, El, Indent);
         El := Get_Chain (El);
      end loop;
   end Disp_Declaration_Objects;

   package Hierarchy_Pkg is
      type Config_Type is record
         With_Objs : Boolean;
         Recurse : Boolean;
         Indent : Natural;
      end record;

      procedure Disp_Hierarchy (Inst : Synth_Instance_Acc; Cfg : Config_Type);

      procedure Disp_Hierarchy_Statements
        (Inst : Synth_Instance_Acc; Stmts : Node; Cfg : Config_Type);
   end Hierarchy_Pkg;

   package body Hierarchy_Pkg is
      function Inc_Indent (Cfg : Config_Type) return Config_Type
      is
         Res : Config_Type;
      begin
         Res := Cfg;
         Res.Indent := Res.Indent + 1;
         return Res;
      end Inc_Indent;

      procedure Disp_Hierarchy_Statement
        (Inst : Synth_Instance_Acc; Stmt : Node; Cfg : Config_Type) is
      begin
         case Get_Kind (Stmt) is
            when Iir_Kind_Component_Instantiation_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
                  Sub_Node : constant Node := Get_Source_Scope (Sub);
                  Comp_Inst : Synth_Instance_Acc;
               begin
                  Put_Indent (Cfg.Indent);
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
                        end if;
                        if Cfg.With_Objs then
                           Disp_Declaration_Objects
                             (Sub, Get_Generic_Chain (Sub_Node),
                              Cfg.Indent);
                           Disp_Declaration_Objects
                             (Sub, Get_Port_Chain (Sub_Node),
                              Cfg.Indent);
                        end if;
                        if Cfg.Recurse and then Comp_Inst /= null then
                           Disp_Hierarchy (Comp_Inst, Inc_Indent (Cfg));
                        end if;
                     when Iir_Kind_Architecture_Body =>
                        Put (": entity ");
                        Put (Image (Get_Identifier
                                      (Get_Entity (Sub_Node))));
                        Put ('(');
                        Put (Image (Get_Identifier (Sub_Node)));
                        Put (')');
                        New_Line;
                        if Cfg.Recurse then
                           Disp_Hierarchy (Sub, Inc_Indent (Cfg));
                        end if;
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
                  Put_Indent (Cfg.Indent);
                  Put (Image (Get_Label (Stmt)));
                  Put (": if-generate");
                  if Sub = null then
                     Put_Line (" [false]");
                  else
                     Put_Line (" [true]");
                     if Cfg.Recurse then
                        Disp_Hierarchy (Sub, Inc_Indent (Cfg));
                     end if;
                  end if;
               end;
            when Iir_Kind_For_Generate_Statement =>
               declare
                  It : constant Node := Get_Parameter_Specification (Stmt);
                  It_Type : constant Node := Get_Type (It);
                  It_Rng : Type_Acc;
                  It_Len : Natural;
                  Gen_Inst : Synth_Instance_Acc;
               begin
                  Put_Indent (Cfg.Indent);
                  Put (Image (Get_Label (Stmt)));
                  Put (": for-generate");
                  Put (" (");
                  It_Rng := Get_Subtype_Object (Inst, It_Type);
                  Disp_Discrete_Range (It_Rng.Drange, It_Type);
                  Put_Line (")");

                  if Cfg.Recurse then
                     It_Len := Natural (Get_Range_Length (It_Rng.Drange));
                     Gen_Inst := Get_Sub_Instance (Inst, Stmt);
                     for I in 1 .. It_Len loop
                        Disp_Hierarchy
                          (Get_Generate_Sub_Instance (Gen_Inst, I),
                           Inc_Indent (Cfg));
                     end loop;
                  end if;
               end;
            when Iir_Kind_Block_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
               begin
                  Put_Indent (Cfg.Indent);
                  Put (Image (Get_Label (Stmt)));
                  Put_Line (": block");
                  if Cfg.Recurse then
                     Disp_Hierarchy_Statements
                       (Sub, Get_Concurrent_Statement_Chain (Stmt),
                        Inc_Indent (Cfg));
                  end if;
               end;
            when Iir_Kinds_Concurrent_Signal_Assignment
              | Iir_Kind_Concurrent_Assertion_Statement
              | Iir_Kind_Concurrent_Procedure_Call_Statement =>
               null;
            when Iir_Kinds_Process_Statement =>
               --  Note: processes are not elaborated.
               if Cfg.With_Objs then
                  Put_Indent (Cfg.Indent);
                  Put (Image (Get_Label (Stmt)));
                  Put_Line (": process");
               end if;
            when others =>
               Vhdl.Errors.Error_Kind ("disp_hierarchy_statement", Stmt);
         end case;
      end Disp_Hierarchy_Statement;

      procedure Disp_Hierarchy_Statements
        (Inst : Synth_Instance_Acc; Stmts : Node; Cfg : Config_Type)
      is
         Stmt : Node;
      begin
         Stmt := Stmts;
         while Stmt /= Null_Node loop
            Disp_Hierarchy_Statement (Inst, Stmt, Cfg);
            Stmt := Get_Chain (Stmt);
         end loop;
      end Disp_Hierarchy_Statements;

      procedure Disp_Hierarchy
        (Inst : Synth_Instance_Acc; Cfg : Config_Type)
      is
         N : constant Node := Get_Source_Scope (Inst);
      begin
         case Get_Kind (N) is
            when Iir_Kind_Architecture_Body =>
               declare
                  Ent : constant Node := Get_Entity (N);
               begin
                  Put_Indent (Cfg.Indent);
                  Put ("architecture ");
                  Put (Image (Get_Identifier (N)));
                  Put (" of ");
                  Put (Image (Get_Identifier (Ent)));
                  New_Line;
                  if Cfg.With_Objs then
                     Put_Indent (Cfg.Indent);
                     Put_Line ("[entity]");
                     Disp_Declaration_Objects
                       (Inst, Get_Generic_Chain (Ent), Cfg.Indent);
                     Disp_Declaration_Objects
                       (Inst, Get_Port_Chain (Ent), Cfg.Indent);
                     Put_Indent (Cfg.Indent);
                     Put_Line ("[architecture]");
                     Disp_Declaration_Objects
                       (Inst, Get_Declaration_Chain (Ent), Cfg.Indent);
                     Disp_Declaration_Objects
                       (Inst, Get_Declaration_Chain (N), Cfg.Indent);
                  end if;
                  Disp_Hierarchy_Statements
                    (Inst, Get_Concurrent_Statement_Chain (N),
                     Inc_Indent (Cfg));
               end;
            when Iir_Kind_Component_Declaration =>
               Put_Indent (Cfg.Indent);
               Put ("component ");
               Put (Image (Get_Identifier (N)));
               New_Line;
               Disp_Hierarchy
                 (Get_Component_Instance (Inst), Inc_Indent (Cfg));
            when Iir_Kind_Generate_Statement_Body =>
               Put_Indent (Cfg.Indent);
               Put ("generate statement body");
               --  TODO: disp label or index ?
               New_Line;
               Disp_Hierarchy_Statements
                 (Inst, Get_Concurrent_Statement_Chain (N), Cfg);
            when Iir_Kind_Block_Statement =>
               Put_Indent (Cfg.Indent);
               Put ("block statement ");
               Put (Image (Get_Identifier (N)));
               New_Line;
               Disp_Hierarchy_Statements
                 (Inst, Get_Concurrent_Statement_Chain (N), Cfg);
            when others =>
               Vhdl.Errors.Error_Kind ("disp_hierarchy", N);
         end case;
      end Disp_Hierarchy;
   end Hierarchy_Pkg;

   procedure Disp_Hierarchy (Inst : Synth_Instance_Acc;
                             Recurse : Boolean;
                             With_Objs : Boolean)
   is
      use Hierarchy_Pkg;
      Cfg : Config_Type;
   begin
      Cfg := (With_Objs => With_Objs,
              Recurse => Recurse,
              Indent => 0);
      Hierarchy_Pkg.Disp_Hierarchy (Inst, Cfg);
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

   function Find_Concurrent_Statement_By_Name (Stmts : Node; Id : Name_Id)
                                              return Node
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Stmt /= Null_Node loop
         if Get_Label (Stmt) = Id then
            return Stmt;
         end if;
         Stmt := Get_Chain (Stmt);
      end loop;
      return Null_Node;
   end Find_Concurrent_Statement_By_Name;

   function Get_Sub_Instance_By_Name (Inst : Synth_Instance_Acc; Name : String)
                                     return Synth_Instance_Acc
   is
      Scope : constant Node := Get_Source_Scope (Inst);
      Has_Index : Boolean;
      End_Id : Natural;
      Index32 : Uns32;
      Index : Int64;
      Valid : Boolean;
      Stmt : Node;
      Id : Name_Id;
   begin
      End_Id := Name'Last;
      Has_Index := Name (End_Id) = ')';
      Index := 0;
      if Has_Index then
         --  There is a loop-generate index.
         --  Search for '('.
         for I in Name'Range loop
            if Name (I) = '(' then
               End_Id := I - 1;
               exit;
            end if;
         end loop;
         if End_Id = Name'Last or End_Id = Name'First then
            return null;
         end if;
         --  Decode index (assume int).
         Elab.Debugger.To_Num
           (Name (End_Id + 2 .. Name'Last - 1), Index32, Valid);
         Index := Int64 (Index32);
         if not Valid then
            return null;
         end if;
      end if;

      Id := Get_Identifier_No_Create (Name (Name'First .. End_Id));
      if Id = Null_Identifier then
         --  All the identifiers are known, so this name cannot exist.
         return null;
      end if;
      case Get_Kind (Scope) is
         when Iir_Kind_Architecture_Body
           | Iir_Kind_Generate_Statement_Body
           | Iir_Kind_Block_Statement =>
            Stmt := Find_Concurrent_Statement_By_Name
              (Get_Concurrent_Statement_Chain (Scope), Id);
         when others =>
            Vhdl.Errors.Error_Kind ("get_sub_instance(1)", Scope);
      end case;

      if Stmt = Null_Node then
         return null;
      end if;

      case Get_Kind (Stmt) is
         when Iir_Kind_Component_Instantiation_Statement =>
            if Has_Index then
               return null;
            end if;
            declare
               Sub_Inst : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Inst, Stmt);
               Sub_Node : constant Node := Get_Source_Scope (Sub_Inst);
            begin
               case Get_Kind (Sub_Node) is
                  when Iir_Kind_Component_Declaration =>
                     return Get_Component_Instance (Sub_Inst);
                  when Iir_Kind_Architecture_Body =>
                     return Sub_Inst;
                  when others =>
                     raise Internal_Error;
               end case;
            end;
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_Block_Statement =>
            if Has_Index then
               return null;
            end if;
            return Get_Sub_Instance (Inst, Stmt);
         when Iir_Kind_For_Generate_Statement =>
            if not Has_Index then
               return null;
            end if;
            declare
               Iterator : constant Node :=
                 Get_Parameter_Specification (Stmt);
               It_Rng : constant Type_Acc :=
                 Get_Subtype_Object (Inst, Get_Type (Iterator));
               Gen_Inst : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Inst, Stmt);
               Off : Int64;
            begin
               case It_Rng.Drange.Dir is
                  when Dir_To =>
                     if Index < It_Rng.Drange.Left
                       or else Index > It_Rng.Drange.Right
                     then
                        return null;
                     end if;
                     Off := Index - It_Rng.Drange.Left + 1;
                  when Dir_Downto =>
                     if Index > It_Rng.Drange.Left
                       or else Index < It_Rng.Drange.Right
                     then
                        return null;
                     end if;
                     Off := Index - It_Rng.Drange.Right + 1;
               end case;
               return Get_Generate_Sub_Instance (Gen_Inst, Positive (Off));
            end;
         when Iir_Kinds_Concurrent_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement =>
            return null;
         when others =>
            Vhdl.Errors.Error_Kind ("get_sub_instance(2)", Stmt);
      end case;
   end Get_Sub_Instance_By_Name;

   function Find_Concurrent_Statement_By_Instance
     (Inst : Synth_Instance_Acc;
      Stmts : Node;
      Sub_Inst : Synth_Instance_Acc) return Node
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Stmt /= Null_Node loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Component_Instantiation_Statement
              | Iir_Kind_If_Generate_Statement
              | Iir_Kind_For_Generate_Statement
              | Iir_Kind_Block_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
               begin
                  if Sub = Sub_Inst then
                     return Stmt;
                  end if;
               end;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
      raise Internal_Error;
   end Find_Concurrent_Statement_By_Instance;

   function Skip_Instance_Parent (Inst : Synth_Instance_Acc)
                                 return Synth_Instance_Acc
   is
      Parent : constant Synth_Instance_Acc := Get_Instance_Parent (Inst);
      Parent_Scope : constant Node := Get_Source_Scope (Parent);
   begin
      if Parent_Scope = Null_Node then
         --  The root.
         return null;
      end if;

      case Get_Kind (Parent_Scope) is
         when Iir_Kind_Architecture_Body
           | Iir_Kind_Block_Statement =>
            return Inst;
         when Iir_Kind_Component_Declaration =>
            return Parent;
         when Iir_Kind_For_Generate_Statement =>
            return Parent;
         when Iir_Kind_Generate_Statement_Body =>
            --  For an if-generate, the parent is really the parent.
            return Inst;
         when others =>
            Vhdl.Errors.Error_Kind ("skip_instance_parent", Parent_Scope);
      end case;
   end Skip_Instance_Parent;

   function Get_Instance_Path_Parent (Inst : Synth_Instance_Acc)
                                     return Synth_Instance_Acc
   is
      Pre_Parent : constant Synth_Instance_Acc := Skip_Instance_Parent (Inst);
   begin
      if Pre_Parent = null then
         --  The root.
         return null;
      end if;
      return Get_Instance_Parent (Pre_Parent);
   end Get_Instance_Path_Parent;

   procedure Disp_Instance_Path (Inst : Synth_Instance_Acc)
   is
      Pre_Parent_Inst : constant Synth_Instance_Acc :=
        Skip_Instance_Parent (Inst);
      Parent_Inst : Synth_Instance_Acc;
      Parent_Scope : Node;
      Scope : Node;
      Stmt : Node;
   begin
      if Pre_Parent_Inst = null then
         --  The top unit
         Put ('/');
         Parent_Scope := Get_Source_Scope (Inst);
         Put (Image (Get_Identifier (Get_Entity (Parent_Scope))));
         return;
      end if;

      Parent_Inst := Get_Instance_Parent (Pre_Parent_Inst);
      Parent_Scope := Get_Source_Scope (Parent_Inst);
      Disp_Instance_Path (Parent_Inst);
      Put ('/');

      Scope := Get_Source_Scope (Inst);
      if Get_Kind (Scope) in Iir_Kinds_Process_Statement then
         Stmt := Scope;
      else
         Stmt := Find_Concurrent_Statement_By_Instance
           (Parent_Inst, Get_Concurrent_Statement_Chain (Parent_Scope),
            Pre_Parent_Inst);
      end if;
      Put (Image (Get_Identifier (Stmt)));
      if Get_Kind (Stmt) = Iir_Kind_For_Generate_Statement then
         declare
            It : constant Node := Get_Parameter_Specification (Stmt);
            It_Type : constant Node := Get_Type (It);
            Val : constant Valtyp := Get_Value (Inst, It);
         begin
            Put ("(");
            Disp_Discrete_Value (Read_Discrete (Val), It_Type);
            Put (")");
         end;
      end if;
   end Disp_Instance_Path;
end Elab.Vhdl_Debug;
