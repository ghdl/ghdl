--  Annotations for interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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

with Ada.Unchecked_Deallocation;

with Tables;
with Simple_IO;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Ieee.Std_Logic_1164;

package body Vhdl.Annotations is
   procedure Annotate_Declaration_List
     (Block_Info: Sim_Info_Acc; Decl_Chain: Iir);
   procedure Annotate_Sequential_Statement_Chain
     (Block_Info: Sim_Info_Acc; Stmt_Chain: Iir);
   procedure Annotate_Concurrent_Statements_Chain
     (Block_Info: Sim_Info_Acc; Stmt_Chain: Iir);
   procedure Annotate_Block_Configuration
     (Block : Iir_Block_Configuration);
   procedure Annotate_Subprogram_Interfaces_Type
     (Block_Info : Sim_Info_Acc; Subprg: Iir);
   procedure Annotate_Subprogram_Specification
     (Block_Info : Sim_Info_Acc; Subprg: Iir);
   procedure Annotate_Interface_List
     (Block_Info: Sim_Info_Acc; Decl_Chain: Iir; With_Types : Boolean);

   procedure Annotate_Type_Definition (Block_Info: Sim_Info_Acc; Def: Iir);

   --  Annotate type definition DEF only if it is anonymous.
   procedure Annotate_Anonymous_Type_Definition
     (Block_Info: Sim_Info_Acc; Def: Iir);

   -- Add an annotation to object OBJ.
   procedure Create_Object_Info (Block_Info : Sim_Info_Acc;
                                 Obj : Iir;
                                 Obj_Kind : Sim_Info_Kind := Kind_Object)
   is
      Info : Sim_Info_Acc;
   begin
      Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;
      case Obj_Kind is
         when Kind_Type =>
            Info := new Sim_Info_Type'(Kind => Kind_Type,
                                       Ref => Obj,
                                       Obj_Scope => Block_Info,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_Object =>
            Info := new Sim_Info_Type'(Kind => Kind_Object,
                                       Ref => Obj,
                                       Obj_Scope => Block_Info,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_File =>
            Info := new Sim_Info_Type'(Kind => Kind_File,
                                       Ref => Obj,
                                       Obj_Scope => Block_Info,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_Signal =>
            Info := new Sim_Info_Type'(Kind => Kind_Signal,
                                       Ref => Obj,
                                       Obj_Scope => Block_Info,
                                       Slot => Block_Info.Nbr_Objects);
            if not Flag_Synthesis then
               --  Reserve one more slot for value, and initial driver value.
               Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 2;
            end if;
         when Kind_Terminal =>
            Info := new Sim_Info_Type'(Kind => Kind_Terminal,
                                       Ref => Obj,
                                       Obj_Scope => Block_Info,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_Quantity =>
            Info := new Sim_Info_Type'(Kind => Kind_Quantity,
                                       Ref => Obj,
                                       Obj_Scope => Block_Info,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_PSL =>
            Info := new Sim_Info_Type'(Kind => Kind_PSL,
                                       Ref => Obj,
                                       Obj_Scope => Block_Info,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_Block
           | Kind_Process
           | Kind_Frame
           | Kind_Protected
           | Kind_Package
           | Kind_Scalar_Types
           | Kind_File_Type
           | Kind_Extra =>
            raise Internal_Error;
      end case;
      Set_Info (Obj, Info);
   end Create_Object_Info;

   -- Add an annotation to SIGNAL.
   procedure Create_Signal_Info (Block_Info: Sim_Info_Acc; Signal: Iir) is
   begin
      Create_Object_Info (Block_Info, Signal, Kind_Signal);
   end Create_Signal_Info;

   procedure Add_Terminal_Info (Block_Info: Sim_Info_Acc; Terminal : Iir) is
   begin
      Create_Object_Info (Block_Info, Terminal, Kind_Terminal);
   end Add_Terminal_Info;

   procedure Add_Quantity_Info (Block_Info: Sim_Info_Acc; Quantity : Iir) is
   begin
      Create_Object_Info (Block_Info, Quantity, Kind_Quantity);
   end Add_Quantity_Info;

   function Create_Block_Info (Block_Info : Sim_Info_Acc; Blk : Iir)
                              return Sim_Info_Acc
   is
      Info : Sim_Info_Acc;
   begin
      Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;

      Info := new Sim_Info_Type'(Kind => Kind_Block,
                                 Ref => Blk,
                                 Inst_Slot => Block_Info.Nbr_Objects,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 0);
      Set_Info (Blk, Info);
      return Info;
   end Create_Block_Info;

   -- If EXPR has not a literal value, create one.
   -- This is necessary for subtype bounds.
   procedure Annotate_Range_Expression
     (Block_Info: Sim_Info_Acc; Expr: Iir_Range_Expression) is
   begin
      if Get_Info (Expr) /= null then
         return;
      end if;
      Create_Object_Info (Block_Info, Expr);
   end Annotate_Range_Expression;

   --  Annotate type definition DEF only if it is anonymous.
   procedure Annotate_Anonymous_Type_Definition
     (Block_Info: Sim_Info_Acc; Def: Iir) is
   begin
      if Is_Anonymous_Type_Definition (Def) then
         Annotate_Type_Definition (Block_Info, Def);
      end if;
   end Annotate_Anonymous_Type_Definition;


   procedure Annotate_Protected_Type_Declaration (Block_Info : Sim_Info_Acc;
                                                  Prot: Iir)
   is
      Decl : Iir;
      Prot_Info: Sim_Info_Acc;
   begin
      --  First the interfaces type (they are elaborated in their context).
      Decl := Get_Declaration_Chain (Prot);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Annotate_Subprogram_Interfaces_Type (Block_Info, Decl);
            when Iir_Kind_Use_Clause =>
               null;
            when others =>
               --  FIXME: attribute
               Error_Kind ("annotate_protected_type_declaration", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;

      --  Note: if this protected type declaration appears in a generic
      --  package declaration that is shared, the instances will always get
      --  Nbr_Objects as 0...
      Prot_Info := new Sim_Info_Type'(Kind => Kind_Protected,
                                      Ref => Prot,
                                      Nbr_Objects => 0);
      Set_Info (Prot, Prot_Info);

      Decl := Get_Declaration_Chain (Prot);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Annotate_Subprogram_Specification (Block_Info, Decl);
            when Iir_Kind_Use_Clause =>
               null;
            when others =>
               Error_Kind ("annotate_protected_type_declaration", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Annotate_Protected_Type_Declaration;

   procedure Annotate_Protected_Type_Body (Block_Info : Sim_Info_Acc;
                                           Prot: Iir)
   is
      pragma Unreferenced (Block_Info);
      Prot_Info : constant Sim_Info_Acc :=
        Get_Info (Get_Protected_Type_Declaration (Prot));
   begin
      Set_Info (Prot, Prot_Info);

      Annotate_Declaration_List (Prot_Info, Get_Declaration_Chain (Prot));
   end Annotate_Protected_Type_Body;

   procedure Annotate_Type_Definition (Block_Info: Sim_Info_Acc; Def: Iir)
   is
      El: Iir;
   begin
      -- Happen only with universal types.
      if Def = Null_Iir then
         return;
      end if;

      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            if Flag_Synthesis then
               Create_Object_Info (Block_Info, Def, Kind_Type);
            else
               declare
                  Info : Sim_Info_Acc;
                  Nbr_Enums : Natural;
               begin
                  if Def = Vhdl.Std_Package.Boolean_Type_Definition
                    or else Def = Vhdl.Std_Package.Bit_Type_Definition
                  then
                     Info := new Sim_Info_Type'(Kind => Kind_Bit_Type,
                                                Ref => Def,
                                                Width => 1);
                  elsif Def = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type
                    or else Def = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Type
                  then
                     Info := new Sim_Info_Type'(Kind => Kind_Log_Type,
                                                Ref => Def,
                                                Width => 1);
                  else
                     Nbr_Enums := Get_Nbr_Elements
                       (Get_Enumeration_Literal_List (Def));
                     if Nbr_Enums <= 256 then
                        Info := new Sim_Info_Type'(Kind => Kind_E8_Type,
                                                   Ref => Def,
                                                   Width => 0);
                     else
                        Info := new Sim_Info_Type'(Kind => Kind_E32_Type,
                                                   Ref => Def,
                                                   Width => 0);
                     end if;
                  end if;
                  Set_Info (Def, Info);
                  Annotate_Range_Expression
                    (Block_Info, Get_Range_Constraint (Def));
               end;
            end if;

         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            Annotate_Anonymous_Type_Definition
              (Block_Info, Get_Base_Type (Def));
            El := Get_Range_Constraint (Def);
            if El /= Null_Iir then
               case Get_Kind (El) is
                  when Iir_Kind_Range_Expression =>
                     if not Flag_Synthesis then
                        Annotate_Range_Expression (Block_Info, El);
                     end if;
                     --  A physical subtype may be defined by an integer range.
                     if Get_Kind (Def) = Iir_Kind_Physical_Subtype_Definition
                     then
                        null;
                        --  FIXME
                        --  Convert_Int_To_Phys (Get_Info (El).Value);
                     end if;
                  when Iir_Kind_Range_Array_Attribute
                    | Iir_Kind_Reverse_Range_Array_Attribute =>
                     null;
                  when others =>
                     Error_Kind ("annotate_type_definition (rc)", El);
               end case;
            end if;
            if Flag_Synthesis then
               Create_Object_Info (Block_Info, Def, Kind_Type);
            end if;

         when Iir_Kind_Integer_Type_Definition =>
            if Flag_Synthesis then
               Create_Object_Info (Block_Info, Def, Kind_Type);
            else
               Set_Info (Def, new Sim_Info_Type'(Kind => Kind_I64_Type,
                                                 Ref => Def,
                                                 Width => 0));
            end if;

         when Iir_Kind_Floating_Type_Definition =>
            if Flag_Synthesis then
               Create_Object_Info (Block_Info, Def, Kind_Type);
            else
               Set_Info (Def, new Sim_Info_Type'(Kind => Kind_F64_Type,
                                                 Ref => Def,
                                                 Width => 0));
            end if;

         when Iir_Kind_Physical_Type_Definition =>
            if Flag_Synthesis then
               Create_Object_Info (Block_Info, Def, Kind_Type);
            else
               Set_Info (Def, new Sim_Info_Type'(Kind => Kind_I64_Type,
                                                 Ref => Def,
                                                 Width => 0));
            end if;

         when Iir_Kind_Array_Type_Definition =>
            if Flag_Synthesis then
               --  Create an annotation for the element type, as it can be
               --  referenced by the implicit concat function definition for
               --  concatenation with element.
               El := Get_Element_Subtype_Indication (Def);
               if Get_Kind (El) in Iir_Kinds_Subtype_Definition then
                  --  But only if it is a proper new subtype definition
                  --  (ie not a denoting name, or attributes like 'subtype).
                  El := Get_Element_Subtype (Def);
                  Annotate_Anonymous_Type_Definition (Block_Info, El);
               end if;

               --  Then for the array.
               Create_Object_Info (Block_Info, Def, Kind_Type);
            end if;

         when Iir_Kind_Array_Subtype_Definition =>
            if Flag_Synthesis then
               --  For the bounds.
               Create_Object_Info (Block_Info, Def, Kind_Type);
            else
               if Get_Array_Element_Constraint (Def) /= Null_Node
                 or else
                 (Get_Resolution_Indication (Def) /= Null_Node
                    and then
                    (Get_Kind (Get_Resolution_Indication (Def))
                       = Iir_Kind_Array_Element_Resolution))
               then
                  --  This subtype has created a new anonymous subtype for the
                  --  element.
                  El := Get_Element_Subtype (Def);
                  Annotate_Type_Definition (Block_Info, El);
               end if;
               declare
                  List : constant Iir_Flist := Get_Index_Subtype_List (Def);
               begin
                  for I in Flist_First .. Flist_Last (List) loop
                     El := Get_Index_Type (List, I);
                     Annotate_Anonymous_Type_Definition (Block_Info, El);
                  end loop;
               end;
            end if;

         when Iir_Kind_Record_Type_Definition =>
            if Flag_Synthesis then
               --  For the offsets.
               Create_Object_Info (Block_Info, Def, Kind_Type);
            else
               declare
                  List : constant Iir_Flist :=
                    Get_Elements_Declaration_List (Def);
               begin
                  for I in Flist_First .. Flist_Last (List) loop
                     El := Get_Nth_Element (List, I);
                     if Get_Subtype_Indication (El) /= Null_Iir then
                        Annotate_Anonymous_Type_Definition
                          (Block_Info, Get_Type (El));
                     end if;
                  end loop;
               end;
            end if;

         when Iir_Kind_Record_Subtype_Definition =>
            if Flag_Synthesis then
               --  For the offsets.
               Create_Object_Info (Block_Info, Def, Kind_Type);
            end if;

         when Iir_Kind_Access_Type_Definition =>
            Annotate_Anonymous_Type_Definition
              (Block_Info, Get_Designated_Type (Def));
            if Flag_Synthesis then
               --  For the designated type.
               Create_Object_Info (Block_Info, Def, Kind_Type);
            end if;

         when Iir_Kind_Access_Subtype_Definition =>
            null;

         when Iir_Kind_File_Type_Definition =>
            if Flag_Synthesis then
               --  For the File type.
               Create_Object_Info (Block_Info, Def, Kind_Type);
            else
               declare
                  Type_Name : constant Iir :=
                    Get_Type (Get_File_Type_Mark (Def));
                  Res : String_Acc;
               begin
                  if Get_Text_File_Flag (Def)
                    or else
                    (Get_Kind (Type_Name)
                       in Iir_Kinds_Scalar_Type_And_Subtype_Definition)
                  then
                     Res := null;
                  else
                     declare
                        Sig : String
                          (1 .. Get_File_Signature_Length (Type_Name) + 2);
                        Off : Natural := Sig'First;
                     begin
                        Get_File_Signature (Type_Name, Sig, Off);
                        Sig (Off + 0) := '.';
                        Sig (Off + 1) := ASCII.NUL;
                        Res := new String'(Sig);
                     end;
                  end if;
                  Set_Info (Def,
                            new Sim_Info_Type'(Kind => Kind_File_Type,
                                               Ref => Def,
                                               File_Signature => Res));
               end;
            end if;

         when Iir_Kind_Protected_Type_Declaration =>
            Annotate_Protected_Type_Declaration (Block_Info, Def);

         when Iir_Kind_Incomplete_Type_Definition
            | Iir_Kind_Subtype_Attribute =>
            null;

         when Iir_Kind_Foreign_Vector_Type_Definition =>
            if Flag_Synthesis then
               --  For the bounds.
               Create_Object_Info (Block_Info, Def, Kind_Type);
            end if;

         when others =>
            Error_Kind ("annotate_type_definition", Def);
      end case;
   end Annotate_Type_Definition;

   procedure Annotate_Interface_List_Subtype
     (Block_Info: Sim_Info_Acc; Decl_Chain: Iir)
   is
      El: Iir;
   begin
      El := Decl_Chain;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Interface_Signal_Declaration
              | Iir_Kind_Interface_Variable_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               if not Get_Is_Ref (El) then
                  Annotate_Anonymous_Type_Definition
                    (Block_Info, Get_Type (El));
               end if;
            when others =>
               Error_Kind ("annotate_interface_list_subtype", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Annotate_Interface_List_Subtype;

   procedure Annotate_Interface_Package_Declaration
     (Block_Info: Sim_Info_Acc; Inter : Iir)
   is
      Package_Info : Sim_Info_Acc;
   begin
      Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;
      Package_Info := new Sim_Info_Type'
        (Kind => Kind_Package,
         Ref => Inter,
         Nbr_Objects => 0,
         Pkg_Slot => Block_Info.Nbr_Objects,
         Pkg_Parent => Block_Info);
      Set_Info (Inter, Package_Info);

      Annotate_Interface_List
        (Package_Info, Get_Generic_Chain (Inter), True);
      Annotate_Declaration_List (Package_Info, Get_Declaration_Chain (Inter));
   end Annotate_Interface_Package_Declaration;

   procedure Annotate_Interface_List
     (Block_Info: Sim_Info_Acc; Decl_Chain: Iir; With_Types : Boolean)
   is
      Decl : Iir;
   begin
      Decl := Decl_Chain;
      while Decl /= Null_Iir loop
         if With_Types
           and then Get_Kind (Decl) in Iir_Kinds_Interface_Object_Declaration
           and then not Get_Is_Ref (Decl)
         then
            Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (Decl));
         end if;
         case Get_Kind (Decl) is
            when Iir_Kind_Interface_Signal_Declaration =>
               Create_Signal_Info (Block_Info, Decl);
            when Iir_Kind_Interface_Variable_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               Create_Object_Info (Block_Info, Decl);
            when Iir_Kind_Interface_Package_Declaration =>
               Annotate_Interface_Package_Declaration (Block_Info, Decl);
            when Iir_Kind_Interface_Type_Declaration =>
               if Flag_Synthesis
                 and then (Get_Kind (Get_Parent (Decl))
                             = Iir_Kind_Entity_Declaration)
               then
                  --  Create an info on the interface_type_definition.
                  --  This is needed for a generic type in an entity, as the
                  --  nodes are not instantiated.
                  Create_Object_Info (Block_Info, Get_Type (Decl));
               end if;
            when Iir_Kinds_Interface_Subprogram_Declaration =>
               --  Macro-expanded
               null;
            when others =>
               Error_Kind ("annotate_interface_list", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Annotate_Interface_List;

   procedure Annotate_Subprogram_Interfaces_Type
     (Block_Info : Sim_Info_Acc; Subprg: Iir)
   is
      Interfaces : constant Iir := Get_Interface_Declaration_Chain (Subprg);
   begin
      --  See LRM93 12.3.1.1 (Subprogram declarations and bodies).  The type
      --  of the interfaces are elaborated in the outer context.
      Annotate_Interface_List_Subtype (Block_Info, Interfaces);

      if Get_Kind (Subprg) = Iir_Kind_Function_Declaration then
         --  FIXME: can this create a new annotation ?
         Annotate_Anonymous_Type_Definition
           (Block_Info, Get_Return_Type (Subprg));
      end if;
   end Annotate_Subprogram_Interfaces_Type;

   procedure Annotate_Subprogram_Specification
     (Block_Info : Sim_Info_Acc; Subprg: Iir)
   is
      pragma Unreferenced (Block_Info);
      Subprg_Info: Sim_Info_Acc;
      Interfaces : constant Iir := Get_Interface_Declaration_Chain (Subprg);
   begin
      Subprg_Info := new Sim_Info_Type'(Kind => Kind_Frame,
                                        Ref => Subprg,
                                        Nbr_Objects => 0);
      Set_Info (Subprg, Subprg_Info);

      Annotate_Interface_List (Subprg_Info, Interfaces, False);
   end Annotate_Subprogram_Specification;

   procedure Annotate_Subprogram_Body
     (Block_Info : Sim_Info_Acc; Subprg: Iir)
   is
      pragma Unreferenced (Block_Info);
      Spec : constant Iir := Get_Subprogram_Specification (Subprg);
      Subprg_Info : constant Sim_Info_Acc := Get_Info (Spec);
   begin
      Set_Info (Subprg, Subprg_Info);

      --  Do not annotate body of foreign subprograms.
      if Get_Foreign_Flag (Spec) then
         return;
      end if;

      Annotate_Declaration_List
        (Subprg_Info, Get_Declaration_Chain (Subprg));

      Annotate_Sequential_Statement_Chain
        (Subprg_Info, Get_Sequential_Statement_Chain (Subprg));
   end Annotate_Subprogram_Body;

   procedure Annotate_Component_Declaration (Comp: Iir_Component_Declaration)
   is
      Info : Sim_Info_Acc;
   begin

      Info := new Sim_Info_Type'(Kind => Kind_Block,
                                 Ref => Comp,
                                 Inst_Slot => Invalid_Object_Slot,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 1); --  For the instance.
      Set_Info (Comp, Info);

      Annotate_Interface_List (Info, Get_Generic_Chain (Comp), True);
      Annotate_Interface_List (Info, Get_Port_Chain (Comp), True);

      --  Last slot is used for the instance.
      Info.Nbr_Objects := Info.Nbr_Objects + 1;
   end Annotate_Component_Declaration;

   --  For package declaration or package instantiation declaration.
   procedure Annotate_Package_Declaration
     (Block_Info : Sim_Info_Acc; Decl: Iir)
   is
      Is_Inst : constant Boolean :=
        Get_Kind (Decl) = Iir_Kind_Package_Instantiation_Declaration;
      Package_Info : Sim_Info_Acc;
      Header : Iir;
   begin
      if not Is_Inst
        and then Is_Uninstantiated_Package (Decl)
        and then Get_Macro_Expanded_Flag (Decl)
      then
         --  Macro-expanded packages are ignored.  Only their instances are
         --  annotated.
         return;
      end if;

      Package_Info := new Sim_Info_Type'
        (Kind => Kind_Package,
         Ref => Decl,
         Nbr_Objects => 0,
         Pkg_Slot => Invalid_Object_Slot,
         Pkg_Parent => null);

      if Is_Inst or else not Is_Uninstantiated_Package (Decl) then
         Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;
         Package_Info.Pkg_Slot := Block_Info.Nbr_Objects;
         Package_Info.Pkg_Parent := Block_Info;
      end if;

      Set_Info (Decl, Package_Info);

      if Is_Inst then
         Annotate_Interface_List
           (Package_Info, Get_Generic_Chain (Decl), True);
      else
         Header := Get_Package_Header (Decl);
         if Header /= Null_Iir then
            Annotate_Interface_List
              (Package_Info, Get_Generic_Chain (Header), True);
         end if;
      end if;

      -- declarations
      Annotate_Declaration_List (Package_Info, Get_Declaration_Chain (Decl));

      if Is_Inst then
         declare
            Bod : constant Iir := Get_Instance_Package_Body (Decl);
         begin
            if Bod /= Null_Iir then
               Set_Info (Bod, Package_Info);
               Annotate_Declaration_List
                 (Package_Info, Get_Declaration_Chain (Bod));
            else
               declare
                  Uninst : constant Iir :=
                    Get_Uninstantiated_Package_Decl (Decl);
                  Uninst_Info : constant Sim_Info_Acc := Get_Info (Uninst);
               begin
                  --  There is not corresponding body for an instantiation, so
                  --  also add objects for the shared body.
                  if not Get_Macro_Expanded_Flag (Uninst) then
                     Package_Info.Nbr_Objects := Uninst_Info.Nbr_Objects;
                  end if;
               end;
            end if;
         end;
      end if;
   end Annotate_Package_Declaration;

   procedure Annotate_Package_Body (Bod: Iir)
   is
      Pkg : constant Node := Get_Package (Bod);
      Package_Info : constant Sim_Info_Acc := Get_Info (Pkg);
   begin
      if Is_Uninstantiated_Package (Pkg)
        and then Get_Macro_Expanded_Flag (Pkg)
      then
         --  The body of a macro-expanded flag.
         return;
      end if;

      --  Set info field of package body declaration.
      Set_Info (Bod, Package_Info);

      -- declarations
      Annotate_Declaration_List (Package_Info, Get_Declaration_Chain (Bod));
   end Annotate_Package_Body;

   procedure Annotate_Declaration_Type (Block_Info: Sim_Info_Acc; Decl: Iir)
   is
      Ind : Iir;
   begin
      if Get_Is_Ref (Decl) then
         return;
      end if;
      Ind := Get_Subtype_Indication (Decl);
      if Get_Kind (Ind) in Iir_Kinds_Denoting_Name then
         return;
      end if;
      Annotate_Type_Definition (Block_Info, Ind);
   end Annotate_Declaration_Type;

   procedure Annotate_Declaration (Block_Info: Sim_Info_Acc; Decl: Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration =>
            Annotate_Package_Declaration (Block_Info, Decl);

         when Iir_Kind_Package_Body =>
            Annotate_Package_Body (Decl);

         when Iir_Kind_Signal_Attribute_Declaration =>
            declare
               Attr : Iir;
            begin
               Attr := Get_Signal_Attribute_Chain (Decl);
               while Is_Valid (Attr) loop
                  Create_Signal_Info (Block_Info, Attr);
                  Attr := Get_Attr_Chain (Attr);
               end loop;
            end;

         when Iir_Kind_Signal_Declaration =>
            Annotate_Declaration_Type (Block_Info, Decl);
            Create_Signal_Info (Block_Info, Decl);

         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Iterator_Declaration =>
            Annotate_Declaration_Type (Block_Info, Decl);
            Create_Object_Info (Block_Info, Decl);

         when Iir_Kind_Constant_Declaration =>
            if Get_Deferred_Declaration (Decl) = Null_Iir
              or else Get_Deferred_Declaration_Flag (Decl)
            then
               --  Create the slot only if the constant is not a full constant
               --  declaration.
               Annotate_Declaration_Type (Block_Info, Decl);
               Create_Object_Info (Block_Info, Decl);
            elsif Flag_Synthesis then
               --  Always create the slot for the subtype.
               Annotate_Declaration_Type (Block_Info, Decl);
            end if;

         when Iir_Kind_File_Declaration =>
            Annotate_Declaration_Type (Block_Info, Decl);
            Create_Object_Info (Block_Info, Decl, Kind_File);

         when Iir_Kind_Terminal_Declaration =>
            Add_Terminal_Info (Block_Info, Decl);
         when Iir_Kinds_Branch_Quantity_Declaration =>
            Annotate_Declaration_Type (Block_Info, Decl);
            Add_Quantity_Info (Block_Info, Decl);

         when Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration =>
            Annotate_Type_Definition (Block_Info, Get_Type_Definition (Decl));
         when Iir_Kind_Subtype_Declaration =>
            Annotate_Type_Definition (Block_Info, Get_Type (Decl));

         when Iir_Kind_Protected_Type_Body =>
            Annotate_Protected_Type_Body (Block_Info, Decl);

         when Iir_Kind_Component_Declaration =>
            Annotate_Component_Declaration (Decl);

         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            if (Get_Implicit_Definition (Decl)
                  not in Iir_Predefined_Operators)
              and then not Is_Second_Subprogram_Specification (Decl)
            then
               Annotate_Subprogram_Interfaces_Type (Block_Info, Decl);
               Annotate_Subprogram_Specification (Block_Info, Decl);
            end if;
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Annotate_Subprogram_Body (Block_Info, Decl);

         when Iir_Kind_Object_Alias_Declaration =>
            if Get_Subtype_Indication (Decl) /= Null_Iir then
               Annotate_Anonymous_Type_Definition
                 (Block_Info, Get_Type (Decl));
            end if;
            Create_Object_Info (Block_Info, Decl);

         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;

         when Iir_Kind_Attribute_Declaration =>
            null;
         when Iir_Kind_Attribute_Specification =>
            declare
               Value : Iir_Attribute_Value;
            begin
               Value := Get_Attribute_Value_Spec_Chain (Decl);
               while Value /= Null_Iir loop
                  if not Flag_Synthesis then
                     Annotate_Anonymous_Type_Definition
                       (Block_Info, Get_Type (Value));
                  end if;
                  Create_Object_Info (Block_Info, Value);
                  Value := Get_Spec_Chain (Value);
               end loop;
            end;
         when Iir_Kind_Disconnection_Specification =>
            null;

         when Iir_Kind_Group_Template_Declaration =>
            null;
         when Iir_Kind_Group_Declaration =>
            null;
         when Iir_Kind_Use_Clause =>
            null;

         when Iir_Kind_Configuration_Specification =>
            null;

--           when Iir_Kind_Implicit_Signal_Declaration =>
--              declare
--                 Nsig : Iir;
--              begin
--                 Nsig := Decl;
--                 loop
--                    Nsig := Get_Implicit_Signal_Chain (Nsig);
--                    exit when Nsig = Null_Iir;
--                    Add_Signal_Info (Block_Info, Nsig);
--                 end loop;
--              end;

         when Iir_Kind_Nature_Declaration =>
            null;

         when Iir_Kind_Psl_Default_Clock =>
            null;

         when Iir_Kind_Suspend_State_Declaration =>
            Create_Object_Info (Block_Info, Decl);

         when others =>
            Error_Kind ("annotate_declaration", Decl);
      end case;
   end Annotate_Declaration;

   procedure Annotate_Declaration_List
     (Block_Info: Sim_Info_Acc; Decl_Chain: Iir)
   is
      El: Iir;
   begin
      El := Decl_Chain;
      while El /= Null_Iir loop
         Annotate_Declaration (Block_Info, El);
         El := Get_Chain (El);
      end loop;
   end Annotate_Declaration_List;

   procedure Annotate_Procedure_Call_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      Call : constant Iir := Get_Procedure_Call (Stmt);
      Imp  : constant Iir := Get_Implementation (Call);
      Assoc_Chain : constant Iir := Get_Parameter_Association_Chain (Call);
      Inter_Chain : constant Iir := Get_Interface_Declaration_Chain (Imp);
      Assoc : Iir;
      Assoc_Inter : Iir;
      Inter : Iir;
   begin
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Assoc /= Null_Iir loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         if Is_Copyback_Parameter (Inter) then
            Create_Object_Info (Block_Info, Assoc, Kind_Object);
         end if;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Annotate_Procedure_Call_Statement;

   procedure Annotate_Sequential_Statement_Chain
     (Block_Info: Sim_Info_Acc; Stmt_Chain: Iir)
   is
      Stmt : Iir;
      Max_Nbr_Objects : Object_Slot_Type;
      Current_Nbr_Objects : Object_Slot_Type;

      procedure Save_Nbr_Objects is
      begin
         --  Objects used by loop statements can be reused later by
         --  other (ie following) loop statements.
         --  Furthermore, this allow to correctly check elaboration
         --  order.
         Max_Nbr_Objects := Object_Slot_Type'Max
           (Block_Info.Nbr_Objects, Max_Nbr_Objects);
         Block_Info.Nbr_Objects := Current_Nbr_Objects;
      end Save_Nbr_Objects;
   begin
      Current_Nbr_Objects := Block_Info.Nbr_Objects;
      Max_Nbr_Objects := Current_Nbr_Objects;

      Stmt := Stmt_Chain;
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Null_Statement =>
               null;
            when Iir_Kind_Assertion_Statement
              | Iir_Kind_Report_Statement =>
               null;
            when Iir_Kind_Return_Statement =>
               null;
            when Iir_Kind_Simple_Signal_Assignment_Statement
              | Iir_Kind_Selected_Waveform_Assignment_Statement
              | Iir_Kind_Conditional_Signal_Assignment_Statement
              | Iir_Kind_Variable_Assignment_Statement
              | Iir_Kind_Conditional_Variable_Assignment_Statement =>
               null;
            when Iir_Kind_Procedure_Call_Statement =>
               Annotate_Procedure_Call_Statement (Block_Info, Stmt);
               Save_Nbr_Objects;
            when Iir_Kind_Exit_Statement
              | Iir_Kind_Next_Statement =>
               null;
            when Iir_Kind_Wait_Statement =>
               null;

            when Iir_Kind_If_Statement =>
               declare
                  Clause: Iir := Stmt;
               begin
                  loop
                     Annotate_Sequential_Statement_Chain
                       (Block_Info, Get_Sequential_Statement_Chain (Clause));
                     Clause := Get_Else_Clause (Clause);
                     exit when Clause = Null_Iir;
                     Save_Nbr_Objects;
                  end loop;
               end;

            when Iir_Kind_Case_Statement =>
               declare
                  Assoc: Iir;
               begin
                  Assoc := Get_Case_Statement_Alternative_Chain (Stmt);
                  loop
                     Annotate_Sequential_Statement_Chain
                       (Block_Info, Get_Associated_Chain (Assoc));
                     Assoc := Get_Chain (Assoc);
                     exit when Assoc = Null_Iir;
                     Save_Nbr_Objects;
                  end loop;
               end;

            when Iir_Kind_For_Loop_Statement =>
               Annotate_Declaration
                 (Block_Info, Get_Parameter_Specification (Stmt));
               Annotate_Sequential_Statement_Chain
                 (Block_Info, Get_Sequential_Statement_Chain (Stmt));

            when Iir_Kind_While_Loop_Statement =>
               Annotate_Sequential_Statement_Chain
                 (Block_Info, Get_Sequential_Statement_Chain (Stmt));

            when Iir_Kind_Suspend_State_Statement =>
               null;

            when others =>
               Error_Kind ("annotate_sequential_statement_chain", Stmt);
         end case;

         Save_Nbr_Objects;

         Stmt := Get_Chain (Stmt);
      end loop;
      Block_Info.Nbr_Objects := Max_Nbr_Objects;
   end Annotate_Sequential_Statement_Chain;

   procedure Annotate_Block_Statement
     (Block_Info : Sim_Info_Acc; Block : Iir_Block_Statement)
   is
      Info : Sim_Info_Acc;
      Header : Iir_Block_Header;
      Guard : Iir;
   begin
      Info := Create_Block_Info (Block_Info, Block);

      Guard := Get_Guard_Decl (Block);
      if Guard /= Null_Iir then
         Create_Signal_Info (Info, Guard);
      end if;
      Header := Get_Block_Header (Block);
      if Header /= Null_Iir then
         Annotate_Interface_List (Info, Get_Generic_Chain (Header), True);
         Annotate_Interface_List (Info, Get_Port_Chain (Header), True);
      end if;
      Annotate_Declaration_List (Info, Get_Declaration_Chain (Block));
      Annotate_Concurrent_Statements_Chain
        (Info, Get_Concurrent_Statement_Chain (Block));
   end Annotate_Block_Statement;

   procedure Annotate_Generate_Statement_Body
     (Block_Info : Sim_Info_Acc; Bod : Iir; It : Iir)
   is
      Info : Sim_Info_Acc;
   begin
      Info := Create_Block_Info (Block_Info, Bod);

      if It /= Null_Iir then
         Create_Object_Info (Info, It);
      end if;
      Annotate_Declaration_List (Info, Get_Declaration_Chain (Bod));
      Annotate_Concurrent_Statements_Chain
        (Info, Get_Concurrent_Statement_Chain (Bod));
   end Annotate_Generate_Statement_Body;

   procedure Annotate_If_Generate_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      Info : Sim_Info_Acc;
      Clause : Iir;
   begin
      Info := Create_Block_Info (Block_Info, Stmt);
      pragma Unreferenced (Info);

      Clause := Stmt;
      while Clause /= Null_Iir loop
         --  Use the same slot as the if-generate statement.
         Block_Info.Nbr_Objects := Block_Info.Nbr_Objects - 1;

         Annotate_Generate_Statement_Body
           (Block_Info, Get_Generate_Statement_Body (Clause), Null_Iir);
         Clause := Get_Generate_Else_Clause (Clause);
      end loop;
   end Annotate_If_Generate_Statement;

   procedure Annotate_For_Generate_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      Param : constant Iir := Get_Parameter_Specification (Stmt);
      Info : Sim_Info_Acc;
   begin
      --  Elaborate the subtype in the current block.
      Annotate_Declaration_Type (Block_Info, Param);

      Info := Create_Block_Info (Block_Info, Stmt);

      Annotate_Generate_Statement_Body
        (Info, Get_Generate_Statement_Body (Stmt), Param);
   end Annotate_For_Generate_Statement;

   procedure Annotate_Case_Generate_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      Assoc : Iir;
   begin
      Assoc := Get_Case_Statement_Alternative_Chain (Stmt);
      while Assoc /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Annotate_Generate_Statement_Body
              (Block_Info, Get_Associated_Block (Assoc), Null_Iir);
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Annotate_Case_Generate_Statement;

   procedure Annotate_Component_Instantiation_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      Info: Sim_Info_Acc;
   begin
      --  Add a slot just to put the instance.
      Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;
      Info := new Sim_Info_Type'(Kind => Kind_Block,
                                 Ref => Stmt,
                                 Inst_Slot => Block_Info.Nbr_Objects,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 1);
      Set_Info (Stmt, Info);
   end Annotate_Component_Instantiation_Statement;

   procedure Annotate_Process_Statement (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      pragma Unreferenced (Block_Info);
      Info : Sim_Info_Acc;
   begin
      Info := new Sim_Info_Type'(Kind => Kind_Process,
                                 Ref => Stmt,
                                 Nbr_Objects => 0);
      Set_Info (Stmt, Info);

      Annotate_Declaration_List
        (Info, Get_Declaration_Chain (Stmt));
      Annotate_Sequential_Statement_Chain
        (Info, Get_Sequential_Statement_Chain (Stmt));
   end Annotate_Process_Statement;

   procedure Annotate_Concurrent_Statement
     (Block_Info: Sim_Info_Acc; Stmt : Iir) is
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            Annotate_Process_Statement (Block_Info, Stmt);

         when Iir_Kind_Component_Instantiation_Statement =>
            Annotate_Component_Instantiation_Statement (Block_Info, Stmt);

         when Iir_Kind_Block_Statement =>
            Annotate_Block_Statement (Block_Info, Stmt);

         when Iir_Kind_If_Generate_Statement =>
            Annotate_If_Generate_Statement (Block_Info, Stmt);
         when Iir_Kind_For_Generate_Statement =>
            Annotate_For_Generate_Statement (Block_Info, Stmt);
         when Iir_Kind_Case_Generate_Statement =>
            Annotate_Case_Generate_Statement (Block_Info, Stmt);

         when Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Declaration =>
            null;

         when Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Restrict_Directive =>
            null;
         when Iir_Kind_Psl_Endpoint_Declaration =>
            Create_Object_Info (Block_Info, Stmt, Kind_PSL);

         when Iir_Kind_Simple_Simultaneous_Statement =>
            null;

         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement =>
            --  In case concurrent signal assignemnts were not
            --  canonicalized (for synthesis).
            null;

         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            declare
               Info : Sim_Info_Acc;
            begin
               Info := new Sim_Info_Type'(Kind => Kind_Process,
                                          Ref => Stmt,
                                          Nbr_Objects => 0);
               Set_Info (Stmt, Info);
               Annotate_Procedure_Call_Statement (Info, Stmt);
            end;

         when others =>
            Error_Kind ("annotate_concurrent_statement", Stmt);
      end case;
   end Annotate_Concurrent_Statement;

   procedure Annotate_Concurrent_Statements_Chain
     (Block_Info: Sim_Info_Acc; Stmt_Chain : Iir)
   is
      Stmt : Iir;
   begin
      Stmt := Stmt_Chain;
      while Stmt /= Null_Iir loop
         Annotate_Concurrent_Statement (Block_Info, Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Annotate_Concurrent_Statements_Chain;

   procedure Annotate_Entity (Decl : Iir_Entity_Declaration)
   is
      Entity_Info: Sim_Info_Acc;
   begin
      Entity_Info := new Sim_Info_Type'(Kind => Kind_Block,
                                        Ref => Decl,
                                        Inst_Slot => Invalid_Object_Slot,
                                        Nbr_Objects => 0,
                                        Nbr_Instances => 0);
      Set_Info (Decl, Entity_Info);

      Annotate_Interface_List (Entity_Info, Get_Generic_Chain (Decl), True);
      Annotate_Interface_List (Entity_Info, Get_Port_Chain (Decl), True);

      Annotate_Declaration_List (Entity_Info, Get_Declaration_Chain (Decl));
      Annotate_Concurrent_Statements_Chain
        (Entity_Info, Get_Concurrent_Statement_Chain (Decl));
   end Annotate_Entity;

   procedure Annotate_Architecture (Decl: Iir_Architecture_Body)
   is
      Entity_Info : constant Sim_Info_Acc := Get_Info (Get_Entity (Decl));
      Saved_Info : constant Sim_Info_Type (Kind_Block) := Entity_Info.all;
      Arch_Info: Sim_Info_Acc;
   begin
      --  No blocks no instantiation in entities.
      pragma Assert (Entity_Info.Nbr_Instances = 0);

      --  Annotate architecture using the entity as the architecture extend
      --  the scope of the entity, and the entity is the reference.

      Annotate_Declaration_List (Entity_Info, Get_Declaration_Chain (Decl));
      Annotate_Concurrent_Statements_Chain
        (Entity_Info, Get_Concurrent_Statement_Chain (Decl));

      Arch_Info := new Sim_Info_Type'(Entity_Info.all);
      Entity_Info.all := Saved_Info;
      Set_Info (Decl, Arch_Info);
   end Annotate_Architecture;

   procedure Annotate_Vunit_Declaration (Decl : Iir)
   is
      Vunit_Info : Sim_Info_Acc;
      Item : Iir;
   begin
      Vunit_Info := new Sim_Info_Type'(Kind => Kind_Block,
                                       Ref => Decl,
                                       Inst_Slot => Invalid_Object_Slot,
                                       Nbr_Objects => 0,
                                       Nbr_Instances => 0);
      Set_Info (Decl, Vunit_Info);

      Item := Get_Vunit_Item_Chain (Decl);
      while Item /= Null_Iir loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Declaration
               | Iir_Kind_PSL_Inherit_Spec =>
               null;
            when Iir_Kind_Psl_Assert_Directive
               | Iir_Kind_Psl_Assume_Directive
               | Iir_Kind_Psl_Cover_Directive
               | Iir_Kind_Psl_Restrict_Directive =>
               null;
            when Iir_Kind_Signal_Declaration
               | Iir_Kind_Constant_Declaration
               | Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration
               | Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body
               | Iir_Kind_Attribute_Declaration
               | Iir_Kind_Attribute_Specification
               | Iir_Kind_Object_Alias_Declaration
               | Iir_Kind_Non_Object_Alias_Declaration
               | Iir_Kind_Subtype_Declaration
               | Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Annotate_Declaration (Vunit_Info, Item);
            when Iir_Kinds_Concurrent_Signal_Assignment
               | Iir_Kinds_Process_Statement
               | Iir_Kinds_Generate_Statement
               | Iir_Kind_Block_Statement
               | Iir_Kind_Concurrent_Procedure_Call_Statement
               | Iir_Kind_Component_Instantiation_Statement =>
               Annotate_Concurrent_Statement (Vunit_Info, Item);
            when others =>
               Error_Kind ("annotate_vunit_declaration", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Annotate_Vunit_Declaration;

   procedure Annotate_Foreign_Module (Decl : Iir)
   is
      Info: Sim_Info_Acc;
   begin
      Info := new Sim_Info_Type'(Kind => Kind_Block,
                                 Ref => Decl,
                                 Inst_Slot => Invalid_Object_Slot,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 0);
      Set_Info (Decl, Info);

      Annotate_Interface_List (Info, Get_Generic_Chain (Decl), True);
      Annotate_Interface_List (Info, Get_Port_Chain (Decl), True);
   end Annotate_Foreign_Module;

   procedure Annotate_Component_Configuration
     (Conf : Iir_Component_Configuration)
   is
      Block : constant Iir := Get_Block_Configuration (Conf);
   begin
      Annotate_Block_Configuration (Block);
   end Annotate_Component_Configuration;

   procedure Annotate_Block_Configuration (Block : Iir_Block_Configuration)
   is
      El : Iir;
   begin
      if Block = Null_Iir then
         return;
      end if;

      --  Declaration are use_clause only.
      El := Get_Configuration_Item_Chain (Block);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Block_Configuration =>
               Annotate_Block_Configuration (El);
            when Iir_Kind_Component_Configuration =>
               Annotate_Component_Configuration (El);
            when others =>
               Error_Kind ("annotate_block_configuration", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Annotate_Block_Configuration;

   procedure Annotate_Configuration_Declaration
     (Block_Info : Sim_Info_Acc; Decl : Iir_Configuration_Declaration)
   is
      Config_Info: Sim_Info_Acc;
   begin
      Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;
      Config_Info := new Sim_Info_Type'
        (Kind => Kind_Package,
         Ref => Decl,
         Nbr_Objects => 0,
         Pkg_Slot => Block_Info.Nbr_Objects,
         Pkg_Parent => Block_Info);
      Set_Info (Decl, Config_Info);

      Annotate_Declaration_List (Config_Info, Get_Declaration_Chain (Decl));
      Annotate_Block_Configuration (Get_Block_Configuration (Decl));
   end Annotate_Configuration_Declaration;

   package Info_Node is new Tables
     (Table_Component_Type => Sim_Info_Acc,
      Table_Index_Type => Iir,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   procedure Annotate_Expand_Table
   is
      El: Iir;
   begin
      Info_Node.Increment_Last;
      El := Info_Node.Last;
      Info_Node.Set_Last (Get_Last_Node);
      for I in El .. Info_Node.Last loop
         Info_Node.Table (I) := null;
      end loop;
   end Annotate_Expand_Table;

   -- Decorate the tree in order to be usable with the internal simulator.
   procedure Annotate (Unit : Iir_Design_Unit)
   is
      El : constant Iir := Get_Library_Unit (Unit);
   begin
      --  Expand info table.
      Annotate_Expand_Table;

      case Get_Kind (El) is
         when Iir_Kind_Entity_Declaration =>
            Annotate_Entity (El);
         when Iir_Kind_Architecture_Body =>
            Annotate_Architecture (El);
         when Iir_Kind_Package_Declaration =>
            declare
               use Vhdl.Std_Package;
            begin
               if El = Standard_Package then
                  pragma Assert (Global_Info = null);
                  Global_Info :=
                    new Sim_Info_Type'(Kind => Kind_Block,
                                       Ref => El,
                                       Nbr_Objects => 0,
                                       Inst_Slot => Invalid_Object_Slot,
                                       Nbr_Instances => 0);
                  Annotate_Package_Declaration (Global_Info, El);
                  --  These types are not in std.standard!
                  Annotate_Type_Definition
                    (Get_Info (El), Convertible_Integer_Type_Definition);
                  Annotate_Type_Definition
                    (Get_Info (El), Convertible_Real_Type_Definition);
               else
                  pragma Assert (Global_Info /= null);
                  Annotate_Package_Declaration (Global_Info, El);
               end if;
            end;
         when Iir_Kind_Package_Body =>
            Annotate_Package_Body (El);
         when Iir_Kind_Configuration_Declaration =>
            Annotate_Configuration_Declaration (Global_Info, El);
         when Iir_Kind_Package_Instantiation_Declaration =>
            Annotate_Package_Declaration (Global_Info, El);
         when Iir_Kind_Context_Declaration =>
            null;
         when Iir_Kind_Vunit_Declaration =>
            Annotate_Vunit_Declaration (El);
         when Iir_Kind_Foreign_Module =>
            Annotate_Foreign_Module (El);
         when others =>
            Error_Kind ("annotate2", El);
      end case;
   end Annotate;

   procedure Initialize_Annotate is
   begin
      Info_Node.Init;
   end Initialize_Annotate;

   procedure Finalize_Annotate
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Sim_Info_Type, Sim_Info_Acc);
   begin
      Free (Global_Info);
      for I in Info_Node.First .. Info_Node.Last loop
         case Get_Kind (I) is
            when Iir_Kind_Package_Body
              | Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body
              | Iir_Kind_Protected_Type_Body =>
               --  Info is shared with the spec.
               null;
            when others =>
               Free (Info_Node.Table (I));
         end case;
      end loop;
      Info_Node.Free;
   end Finalize_Annotate;

   -- Disp annotations for an iir node.
   procedure Disp_Vhdl_Info (Node: Iir)
   is
      use Simple_IO;
      Info : constant Sim_Info_Acc := Get_Info (Node);
   begin
      if Info = null then
         return;
      end if;

      case Info.Kind is
         when Kind_Block =>
            Put_Line
              ("-- nbr objects:" & Object_Slot_Type'Image (Info.Nbr_Objects));

         when Kind_Frame
           | Kind_Protected
           | Kind_Process
           | Kind_Package =>
            Put_Line
              ("-- nbr objects:" & Object_Slot_Type'Image (Info.Nbr_Objects));

         when Kind_Type | Kind_Object | Kind_Signal | Kind_File
           | Kind_Terminal
           | Kind_Quantity
           | Kind_PSL =>
            Put_Line ("-- slot:" & Object_Slot_Type'Image (Info.Slot));
         when Kind_Scalar_Types
           | Kind_File_Type
           | Kind_Extra =>
            null;
      end case;
   end Disp_Vhdl_Info;

   procedure Disp_Info (Info : Sim_Info_Acc)
   is
      use Simple_IO;
   begin
      if Info = null then
         Put_Line ("*null*");
         return;
      end if;
      case Info.Kind is
         when Kind_Block
           | Kind_Frame
           | Kind_Protected
           | Kind_Process
           | Kind_Package =>
            Put_Line ("nbr objects:"
                        & Object_Slot_Type'Image (Info.Nbr_Objects));
            case Info.Kind is
               when Kind_Block =>
                  Put ("inst_slot:"
                         & Object_Slot_Type'Image (Info.Inst_Slot));
                  Put_Line (", nbr instance:"
                              & Instance_Slot_Type'Image (Info.Nbr_Instances));
               when others =>
                  null;
            end case;
         when Kind_Type | Kind_Object | Kind_Signal | Kind_File
           | Kind_Terminal | Kind_Quantity
           | Kind_PSL =>
            Put_Line ("slot:" & Object_Slot_Type'Image (Info.Slot));
         when Kind_Extra =>
            Put_Line ("extra:" & Extra_Slot_Type'Image (Info.Extra_Slot));
         when Kind_Scalar_Types =>
            Put_Line ("scalar type");
         when Kind_File_Type =>
            Put ("file type: ");
            if Info.File_Signature = null then
               Put ("(no sig)");
            else
               Put (Info.File_Signature.all);
            end if;
            New_Line;
      end case;
   end Disp_Info;

   procedure Disp_Tree_Info (Node: Iir) is
   begin
      Disp_Info (Get_Info (Node));
   end Disp_Tree_Info;

   procedure Set_Info (Target: Iir; Info: Sim_Info_Acc) is
   begin
      pragma Assert (Info_Node.Table (Target) = null);
      Info_Node.Table (Target) := Info;
   end Set_Info;

   function Get_Info (Target: Iir) return Sim_Info_Acc is
   begin
      return Info_Node.Table (Target);
   end Get_Info;
end Vhdl.Annotations;
