--  Annotations for interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with GNAT.Table;
with Ada.Text_IO;
with Std_Package;
with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;

package body Annotations is
   --  Current scope.  Used when an object is created to indicate which scope
   --  it belongs to.
   Current_Scope: Scope_Type := (Kind => Scope_Kind_None);

   procedure Annotate_Declaration_List
     (Block_Info: Sim_Info_Acc; Decl_Chain: Iir);
   procedure Annotate_Sequential_Statement_Chain
     (Block_Info: Sim_Info_Acc; Stmt_Chain: Iir);
   procedure Annotate_Concurrent_Statements_List
     (Block_Info: Sim_Info_Acc; Stmt_Chain: Iir);
   procedure Annotate_Block_Configuration
     (Block : Iir_Block_Configuration);
   procedure Annotate_Subprogram_Interfaces_Type
     (Block_Info : Sim_Info_Acc; Subprg: Iir);
   procedure Annotate_Subprogram_Specification
     (Block_Info : Sim_Info_Acc; Subprg: Iir);

   procedure Annotate_Type_Definition (Block_Info: Sim_Info_Acc; Def: Iir);

   --  Annotate type definition DEF only if it is anonymous.
   procedure Annotate_Anonymous_Type_Definition
     (Block_Info: Sim_Info_Acc; Def: Iir);

   procedure Increment_Current_Scope is
   begin
      case Current_Scope.Kind is
         when Scope_Kind_None
           | Scope_Kind_Package
           | Scope_Kind_Pkg_Inst =>
            --  For a subprogram in a package
            Current_Scope := (Scope_Kind_Frame, Scope_Depth_Type'First);
         when Scope_Kind_Frame =>
            Current_Scope := (Scope_Kind_Frame,
                                    Current_Scope.Depth + 1);
         when Scope_Kind_Component =>
            raise Internal_Error;
      end case;
   end Increment_Current_Scope;

   -- Add an annotation to object OBJ.
   procedure Create_Object_Info
     (Block_Info : Sim_Info_Acc;
      Obj : Iir;
      Obj_Kind : Sim_Info_Kind := Kind_Object)
   is
      Info : Sim_Info_Acc;
   begin
      Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;
      case Obj_Kind is
         when Kind_Object =>
            Info := new Sim_Info_Type'(Kind => Kind_Object,
                                       Obj_Scope => Current_Scope,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_File =>
            Info := new Sim_Info_Type'(Kind => Kind_File,
                                       Obj_Scope => Current_Scope,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_Signal =>
            Info := new Sim_Info_Type'(Kind => Kind_Signal,
                                       Obj_Scope => Current_Scope,
                                       Slot => Block_Info.Nbr_Objects);
            --  Reserve one more slot for default value.
            Block_Info.Nbr_Objects := Block_Info.Nbr_Objects + 1;
         when Kind_Terminal =>
            Info := new Sim_Info_Type'(Kind => Kind_Terminal,
                                       Obj_Scope => Current_Scope,
                                       Slot => Block_Info.Nbr_Objects);
         when Kind_Quantity =>
            Info := new Sim_Info_Type'(Kind => Kind_Quantity,
                                       Obj_Scope => Current_Scope,
                                       Slot => Block_Info.Nbr_Objects);
         when others =>
            raise Internal_Error;
      end case;
      Set_Info (Obj, Info);
   end Create_Object_Info;

   -- Add an annotation to SIGNAL.
   procedure Add_Signal_Info (Block_Info: Sim_Info_Acc; Signal: Iir) is
   begin
      Create_Object_Info (Block_Info, Signal, Kind_Signal);
   end Add_Signal_Info;

   procedure Add_Terminal_Info (Block_Info: Sim_Info_Acc; Terminal : Iir) is
   begin
      Create_Object_Info (Block_Info, Terminal, Kind_Terminal);
   end Add_Terminal_Info;

   procedure Add_Quantity_Info (Block_Info: Sim_Info_Acc; Quantity : Iir) is
   begin
      Create_Object_Info (Block_Info, Quantity, Kind_Quantity);
   end Add_Quantity_Info;

   -- If EXPR has not a literal value, create one.
   -- This is necessary for subtype bounds.
   procedure Annotate_Range_Expression
     (Block_Info: Sim_Info_Acc; Expr: Iir_Range_Expression)
   is
   begin
      if Get_Info (Expr) /= null then
         return;
      end if;
--       if Expr = null or else Get_Info (Expr) /= null then
--          return;
--       end if;
      Create_Object_Info (Block_Info, Expr);
   end Annotate_Range_Expression;

   --  Annotate type definition DEF only if it is anonymous.
   procedure Annotate_Anonymous_Type_Definition
     (Block_Info: Sim_Info_Acc; Def: Iir)
   is
   begin
      if Is_Anonymous_Type_Definition (Def) then
         Annotate_Type_Definition (Block_Info, Def);
      end if;
   end Annotate_Anonymous_Type_Definition;

   function Get_File_Signature_Length (Def : Iir) return Natural is
   begin
      case Get_Kind (Def) is
         when Iir_Kinds_Scalar_Type_Definition =>
            return 1;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            return 2
              + Get_File_Signature_Length (Get_Element_Subtype (Def));
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               El : Iir;
               Res : Natural;
               List : Iir_List;
            begin
               Res := 2;
               List := Get_Elements_Declaration_List (Get_Base_Type (Def));
               for I in Natural loop
                  El := Get_Nth_Element (List, I);
                  exit when El = Null_Iir;
                  Res := Res + Get_File_Signature_Length (Get_Type (El));
               end loop;
               return Res;
            end;
         when others =>
            Error_Kind ("get_file_signature_length", Def);
      end case;
   end Get_File_Signature_Length;

   procedure Get_File_Signature (Def : Iir;
                                 Res : in out String;
                                 Off : in out Natural)
   is
      Scalar_Map : constant array (Iir_Value_Scalars) of Character := "bEIF";
   begin
      case Get_Kind (Def) is
         when Iir_Kinds_Scalar_Type_Definition =>
            Res (Off) :=
              Scalar_Map (Get_Info (Get_Base_Type (Def)).Scalar_Mode);
            Off := Off + 1;
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            Res (Off) := '[';
            Off := Off + 1;
            Get_File_Signature (Get_Element_Subtype (Def), Res, Off);
            Res (Off) := ']';
            Off := Off + 1;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               El : Iir;
               List : Iir_List;
            begin
               Res (Off) := '<';
               Off := Off + 1;
               List := Get_Elements_Declaration_List (Get_Base_Type (Def));
               for I in Natural loop
                  El := Get_Nth_Element (List, I);
                  exit when El = Null_Iir;
                  Get_File_Signature (Get_Type (El), Res, Off);
               end loop;
               Res (Off) := '>';
               Off := Off + 1;
            end;
         when others =>
            Error_Kind ("get_file_signature", Def);
      end case;
   end Get_File_Signature;

   procedure Annotate_Protected_Type_Declaration (Block_Info : Sim_Info_Acc;
                                                  Prot: Iir)
   is
      Prev_Scope : constant Scope_Type := Current_Scope;
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

      --  Then the interfaces object.  Increment the scope to reserve a scope
      --  for the protected object.
      Increment_Current_Scope;

      Prot_Info :=
        new Sim_Info_Type'(Kind => Kind_Frame,
                           Inst_Slot => Invalid_Instance_Slot,
                           Frame_Scope => Current_Scope,
                           Nbr_Objects => 0,
                           Nbr_Instances => 0);
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

      Current_Scope := Prev_Scope;
   end Annotate_Protected_Type_Declaration;

   procedure Annotate_Protected_Type_Body (Block_Info : Sim_Info_Acc;
                                           Prot: Iir)
   is
      pragma Unreferenced (Block_Info);
      Prot_Info: Sim_Info_Acc;
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      Prot_Info := Get_Info (Get_Protected_Type_Declaration (Prot));
      Set_Info (Prot, Prot_Info);

      Current_Scope := Prot_Info.Frame_Scope;

      Annotate_Declaration_List
        (Prot_Info, Get_Declaration_Chain (Prot));

      Current_Scope := Prev_Scope;
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
            if Def = Std_Package.Boolean_Type_Definition
              or else Def = Std_Package.Bit_Type_Definition
            then
               Set_Info (Def,
                         new Sim_Info_Type'(Kind => Kind_Scalar_Type,
                                            Scalar_Mode => Iir_Value_B1));
            else
               Set_Info (Def,
                         new Sim_Info_Type'(Kind => Kind_Scalar_Type,
                                            Scalar_Mode => Iir_Value_E32));
            end if;
            Annotate_Range_Expression (Block_Info, Get_Range_Constraint (Def));

         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            El := Get_Range_Constraint (Def);
            if El /= Null_Iir then
               case Get_Kind (El) is
                  when Iir_Kind_Range_Expression =>
                     Annotate_Range_Expression (Block_Info, El);
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
            Annotate_Anonymous_Type_Definition
              (Block_Info, Get_Base_Type (Def));

         when Iir_Kind_Integer_Type_Definition =>
            Set_Info (Def,
                      new Sim_Info_Type'(Kind => Kind_Scalar_Type,
                                         Scalar_Mode => Iir_Value_I64));

         when Iir_Kind_Floating_Type_Definition =>
            Set_Info (Def,
                      new Sim_Info_Type'(Kind => Kind_Scalar_Type,
                                         Scalar_Mode => Iir_Value_F64));

         when Iir_Kind_Physical_Type_Definition =>
            Set_Info (Def,
                      new Sim_Info_Type'(Kind => Kind_Scalar_Type,
                                         Scalar_Mode => Iir_Value_I64));

         when Iir_Kind_Array_Type_Definition =>
            El := Get_Element_Subtype (Def);
            Annotate_Anonymous_Type_Definition (Block_Info, El);

         when Iir_Kind_Array_Subtype_Definition =>
            declare
               List : constant Iir_List := Get_Index_Subtype_List (Def);
            begin
               for I in Natural loop
                  El := Get_Index_Type (List, I);
                  exit when El = Null_Iir;
                  Annotate_Anonymous_Type_Definition (Block_Info, El);
               end loop;
            end;

         when Iir_Kind_Record_Type_Definition =>
            declare
               List : constant Iir_List := Get_Elements_Declaration_List (Def);
            begin
               for I in Natural loop
                  El := Get_Nth_Element (List, I);
                  exit when El = Null_Iir;
                  Annotate_Anonymous_Type_Definition
                    (Block_Info, Get_Type (El));
               end loop;
            end;

         when Iir_Kind_Record_Subtype_Definition =>
            null;

         when Iir_Kind_Access_Type_Definition =>
            Annotate_Anonymous_Type_Definition
              (Block_Info, Get_Designated_Type (Def));

         when Iir_Kind_Access_Subtype_Definition =>
            null;

         when Iir_Kind_File_Type_Definition =>
            declare
               Type_Name : constant Iir := Get_Type (Get_File_Type_Mark (Def));
               Res : String_Acc;
            begin
               if Get_Text_File_Flag (Def)
                 or else
                 Get_Kind (Type_Name) in Iir_Kinds_Scalar_Type_Definition
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
                                            File_Signature => Res));
            end;

         when Iir_Kind_Protected_Type_Declaration =>
            Annotate_Protected_Type_Declaration (Block_Info, Def);

         when Iir_Kind_Incomplete_Type_Definition =>
            null;

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
            when Iir_Kind_Interface_Signal_Declaration =>
               Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (El));
            when Iir_Kind_Interface_Variable_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (El));
            when others =>
               Error_Kind ("annotate_interface_list", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Annotate_Interface_List_Subtype;

   procedure Annotate_Create_Interface_List
     (Block_Info: Sim_Info_Acc; Decl_Chain: Iir; With_Types : Boolean)
   is
      Decl : Iir;
      N : Object_Slot_Type;
   begin
      Decl := Decl_Chain;
      while Decl /= Null_Iir loop
         if With_Types then
            Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (Decl));
         end if;
         case Get_Kind (Decl) is
            when Iir_Kind_Interface_Signal_Declaration =>
               Add_Signal_Info (Block_Info, Decl);
            when Iir_Kind_Interface_Variable_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               Create_Object_Info (Block_Info, Decl);
            when others =>
               Error_Kind ("annotate_create_interface_list", Decl);
         end case;
         N := Block_Info.Nbr_Objects;
         --  Annotation of the default value must not create objects.
         --  FIXME: Is it true ???
         if Block_Info.Nbr_Objects /= N then
            raise Internal_Error;
         end if;
         Decl := Get_Chain (Decl);
      end loop;
   end Annotate_Create_Interface_List;

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
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      Increment_Current_Scope;

      Subprg_Info :=
        new Sim_Info_Type'(Kind => Kind_Frame,
                           Inst_Slot => Invalid_Instance_Slot,
                           Frame_Scope => Current_Scope,
                           Nbr_Objects => 0,
                           Nbr_Instances => 0);
      Set_Info (Subprg, Subprg_Info);

      Annotate_Create_Interface_List (Subprg_Info, Interfaces, False);

      Current_Scope := Prev_Scope;
   end Annotate_Subprogram_Specification;

   procedure Annotate_Subprogram_Body
     (Block_Info : Sim_Info_Acc; Subprg: Iir)
   is
      pragma Unreferenced (Block_Info);
      Spec : constant Iir := Get_Subprogram_Specification (Subprg);
      Subprg_Info : constant Sim_Info_Acc := Get_Info (Spec);
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      --  Do not annotate body of foreign subprograms.
      if Get_Foreign_Flag (Spec) then
         return;
      end if;

      Set_Info (Subprg, Subprg_Info);

      Current_Scope := Subprg_Info.Frame_Scope;

      Annotate_Declaration_List
        (Subprg_Info, Get_Declaration_Chain (Subprg));

      Annotate_Sequential_Statement_Chain
        (Subprg_Info, Get_Sequential_Statement_Chain (Subprg));

      Current_Scope := Prev_Scope;
   end Annotate_Subprogram_Body;

   procedure Annotate_Component_Declaration
     (Comp: Iir_Component_Declaration)
   is
      Info: Sim_Info_Acc;
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      Current_Scope := (Kind => Scope_Kind_Component);

      Info := new Sim_Info_Type'(Kind => Kind_Frame,
                                 Inst_Slot => Invalid_Instance_Slot,
                                 Frame_Scope => Current_Scope,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 1); --  For the instance.
      Set_Info (Comp, Info);

      Annotate_Create_Interface_List (Info, Get_Generic_Chain (Comp), True);
      Annotate_Create_Interface_List (Info, Get_Port_Chain (Comp), True);

      Current_Scope := Prev_Scope;
   end Annotate_Component_Declaration;

   procedure Annotate_Declaration (Block_Info: Sim_Info_Acc; Decl: Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Signal_Declaration =>
            Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (Decl));
            Add_Signal_Info (Block_Info, Decl);

         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Iterator_Declaration =>
            Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (Decl));
            Create_Object_Info (Block_Info, Decl);

         when Iir_Kind_Constant_Declaration =>
            if Get_Deferred_Declaration (Decl) = Null_Iir
              or else Get_Deferred_Declaration_Flag (Decl)
            then
               --  Create the slot only if the constant is not a full constant
               --  declaration.
               Annotate_Anonymous_Type_Definition
                 (Block_Info, Get_Type (Decl));
               Create_Object_Info (Block_Info, Decl);
            else
               Set_Info (Decl, Get_Info (Get_Deferred_Declaration (Decl)));
            end if;

         when Iir_Kind_File_Declaration =>
            Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (Decl));
            Create_Object_Info (Block_Info, Decl, Kind_File);

         when Iir_Kind_Terminal_Declaration =>
            Add_Terminal_Info (Block_Info, Decl);
         when Iir_Kinds_Branch_Quantity_Declaration =>
            Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (Decl));
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
            if Get_Implicit_Definition (Decl) in Iir_Predefined_Explicit
              and then not Is_Second_Subprogram_Specification (Decl)
            then
               Annotate_Subprogram_Interfaces_Type (Block_Info, Decl);
               Annotate_Subprogram_Specification (Block_Info, Decl);
            end if;
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Annotate_Subprogram_Body (Block_Info, Decl);

         when Iir_Kind_Object_Alias_Declaration =>
            Annotate_Anonymous_Type_Definition (Block_Info, Get_Type (Decl));
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

   procedure Annotate_Sequential_Statement_Chain
     (Block_Info: Sim_Info_Acc; Stmt_Chain: Iir)
   is
      El: Iir;
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

      El := Stmt_Chain;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Null_Statement =>
               null;
            when Iir_Kind_Assertion_Statement
              | Iir_Kind_Report_Statement =>
               null;
            when Iir_Kind_Return_Statement =>
               null;
            when Iir_Kind_Signal_Assignment_Statement
              | Iir_Kind_Variable_Assignment_Statement =>
               null;
            when Iir_Kind_Procedure_Call_Statement =>
               null;
            when Iir_Kind_Exit_Statement
              | Iir_Kind_Next_Statement =>
               null;
            when Iir_Kind_Wait_Statement =>
               null;

            when Iir_Kind_If_Statement =>
               declare
                  Clause: Iir := El;
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
                  Assoc := Get_Case_Statement_Alternative_Chain (El);
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
                 (Block_Info, Get_Parameter_Specification (El));
               Annotate_Sequential_Statement_Chain
                 (Block_Info, Get_Sequential_Statement_Chain (El));

            when Iir_Kind_While_Loop_Statement =>
               Annotate_Sequential_Statement_Chain
                 (Block_Info, Get_Sequential_Statement_Chain (El));

            when others =>
               Error_Kind ("annotate_sequential_statement_chain", El);
         end case;

         Save_Nbr_Objects;

         El := Get_Chain (El);
      end loop;
      Block_Info.Nbr_Objects := Max_Nbr_Objects;
   end Annotate_Sequential_Statement_Chain;

   procedure Annotate_Block_Statement
     (Block_Info : Sim_Info_Acc; Block : Iir_Block_Statement)
   is
      Info : Sim_Info_Acc;
      Header : Iir_Block_Header;
      Guard : Iir;
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      Increment_Current_Scope;

      Info := new Sim_Info_Type'(Kind => Kind_Block,
                                 Inst_Slot => Block_Info.Nbr_Instances,
                                 Frame_Scope => Current_Scope,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 0);
      Set_Info (Block, Info);

      Block_Info.Nbr_Instances := Block_Info.Nbr_Instances + 1;

      Guard := Get_Guard_Decl (Block);
      if Guard /= Null_Iir then
         Add_Signal_Info (Info, Guard);
      end if;
      Header := Get_Block_Header (Block);
      if Header /= Null_Iir then
         Annotate_Create_Interface_List
           (Info, Get_Generic_Chain (Header), True);
         Annotate_Create_Interface_List
           (Info, Get_Port_Chain (Header), True);
      end if;
      Annotate_Declaration_List (Info, Get_Declaration_Chain (Block));
      Annotate_Concurrent_Statements_List
        (Info, Get_Concurrent_Statement_Chain (Block));

      Current_Scope := Prev_Scope;
   end Annotate_Block_Statement;

   procedure Annotate_Generate_Statement_Body
     (Block_Info : Sim_Info_Acc; Bod : Iir; It : Iir)
   is
      Info : Sim_Info_Acc;
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      Increment_Current_Scope;

      Info := new Sim_Info_Type'(Kind => Kind_Block,
                                 Inst_Slot => Block_Info.Nbr_Instances,
                                 Frame_Scope => Current_Scope,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 0);
      Set_Info (Bod, Info);

      Block_Info.Nbr_Instances := Block_Info.Nbr_Instances + 1;

      if It /= Null_Iir then
         Annotate_Declaration (Info, It);
      end if;
      Annotate_Declaration_List (Info, Get_Declaration_Chain (Bod));
      Annotate_Concurrent_Statements_List
        (Info, Get_Concurrent_Statement_Chain (Bod));

      Current_Scope := Prev_Scope;
   end Annotate_Generate_Statement_Body;

   procedure Annotate_If_Generate_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      Clause : Iir;
   begin
      Clause := Stmt;
      while Clause /= Null_Iir loop
         Annotate_Generate_Statement_Body
           (Block_Info, Get_Generate_Statement_Body (Clause), Null_Iir);
         Clause := Get_Generate_Else_Clause (Clause);
      end loop;
   end Annotate_If_Generate_Statement;

   procedure Annotate_For_Generate_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir) is
   begin
      Annotate_Generate_Statement_Body
        (Block_Info,
         Get_Generate_Statement_Body (Stmt),
         Get_Parameter_Specification (Stmt));
   end Annotate_For_Generate_Statement;

   procedure Annotate_Component_Instantiation_Statement
     (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      Info: Sim_Info_Acc;
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      Increment_Current_Scope;

      --  Add a slot just to put the instance.
      Info := new Sim_Info_Type'(Kind => Kind_Block,
                                 Inst_Slot => Block_Info.Nbr_Instances,
                                 Frame_Scope => Current_Scope,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 1);
      Set_Info (Stmt, Info);
      Block_Info.Nbr_Instances := Block_Info.Nbr_Instances + 1;

      Current_Scope := Prev_Scope;
   end Annotate_Component_Instantiation_Statement;

   procedure Annotate_Process_Statement (Block_Info : Sim_Info_Acc; Stmt : Iir)
   is
      pragma Unreferenced (Block_Info);
      Info: Sim_Info_Acc;
      Prev_Scope : constant Scope_Type := Current_Scope;
   begin
      Increment_Current_Scope;

      Info := new Sim_Info_Type'(Kind => Kind_Process,
                                 Inst_Slot => Invalid_Instance_Slot,
                                 Frame_Scope => Current_Scope,
                                 Nbr_Objects => 0,
                                 Nbr_Instances => 0);
      Set_Info (Stmt, Info);

      Annotate_Declaration_List
        (Info, Get_Declaration_Chain (Stmt));
      Annotate_Sequential_Statement_Chain
        (Info, Get_Sequential_Statement_Chain (Stmt));

      Current_Scope := Prev_Scope;
   end Annotate_Process_Statement;

   procedure Annotate_Concurrent_Statements_List
     (Block_Info: Sim_Info_Acc; Stmt_Chain : Iir)
   is
      El: Iir;
   begin
      El := Stmt_Chain;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Process_Statement =>
               Annotate_Process_Statement (Block_Info, El);

            when Iir_Kind_Component_Instantiation_Statement =>
               Annotate_Component_Instantiation_Statement (Block_Info, El);

            when Iir_Kind_Block_Statement =>
               Annotate_Block_Statement (Block_Info, El);

            when Iir_Kind_If_Generate_Statement =>
               Annotate_If_Generate_Statement (Block_Info, El);
            when Iir_Kind_For_Generate_Statement =>
               Annotate_For_Generate_Statement (Block_Info, El);

            when Iir_Kind_Simple_Simultaneous_Statement =>
               null;

            when others =>
               Error_Kind ("annotate_concurrent_statements_list", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Annotate_Concurrent_Statements_List;

   procedure Annotate_Entity (Decl: Iir_Entity_Declaration)
   is
      Entity_Info: Sim_Info_Acc;
   begin
      pragma Assert (Current_Scope.Kind = Scope_Kind_None);
      Increment_Current_Scope;

      Entity_Info :=
        new Sim_Info_Type'(Kind => Kind_Block,
                           Inst_Slot => Invalid_Instance_Slot,
                           Frame_Scope => Current_Scope,
                           Nbr_Objects => 0,
                           Nbr_Instances => 0);
      Set_Info (Decl, Entity_Info);

      -- generic list.
      Annotate_Create_Interface_List
        (Entity_Info, Get_Generic_Chain (Decl), True);

      -- Port list.
      Annotate_Create_Interface_List
        (Entity_Info, Get_Port_Chain (Decl), True);

      -- declarations
      Annotate_Declaration_List (Entity_Info, Get_Declaration_Chain (Decl));

      -- processes.
      Annotate_Concurrent_Statements_List
        (Entity_Info, Get_Concurrent_Statement_Chain (Decl));

      Current_Scope := (Kind => Scope_Kind_None);
   end Annotate_Entity;

   procedure Annotate_Architecture (Decl: Iir_Architecture_Body)
   is
      Entity_Info : constant Sim_Info_Acc := Get_Info (Get_Entity (Decl));
      Arch_Info: Sim_Info_Acc;
   begin
      pragma Assert (Current_Scope.Kind = Scope_Kind_None);
      Current_Scope := Entity_Info.Frame_Scope;

      Arch_Info := new Sim_Info_Type'
        (Kind => Kind_Block,
         Inst_Slot => 0, --  Slot for a component
         Frame_Scope => Current_Scope,
         Nbr_Objects => Entity_Info.Nbr_Objects,
         Nbr_Instances => Entity_Info.Nbr_Instances); --  Should be 0.
      Set_Info (Decl, Arch_Info);

      --  FIXME: annotate the default configuration for the arch ?

      -- declarations
      Annotate_Declaration_List (Arch_Info, Get_Declaration_Chain (Decl));

      -- processes.
      Annotate_Concurrent_Statements_List
        (Arch_Info, Get_Concurrent_Statement_Chain (Decl));

      Current_Scope := (Kind => Scope_Kind_None);
   end Annotate_Architecture;

   procedure Annotate_Package (Decl: Iir_Package_Declaration)
   is
      Package_Info: Sim_Info_Acc;
   begin
      pragma Assert (Current_Scope.Kind = Scope_Kind_None);

      Nbr_Packages := Nbr_Packages + 1;
      Current_Scope := (Scope_Kind_Package, Nbr_Packages);

      Package_Info := new Sim_Info_Type'
        (Kind => Kind_Block,
         Inst_Slot => Invalid_Instance_Slot,
         Frame_Scope => Current_Scope,
         Nbr_Objects => 0,
         Nbr_Instances => 0);

      Set_Info (Decl, Package_Info);

      -- declarations
      Annotate_Declaration_List (Package_Info, Get_Declaration_Chain (Decl));

      Current_Scope := (Kind => Scope_Kind_None);
   end Annotate_Package;

   procedure Annotate_Package_Body (Decl: Iir)
   is
      Package_Info: Sim_Info_Acc;
   begin
      pragma Assert (Current_Scope.Kind = Scope_Kind_None);

      -- Set info field of package body declaration.
      Package_Info := Get_Info (Get_Package (Decl));
      Set_Info (Decl, Package_Info);

      Current_Scope := Package_Info.Frame_Scope;

      -- declarations
      Annotate_Declaration_List (Package_Info, Get_Declaration_Chain (Decl));

      Current_Scope := (Kind => Scope_Kind_None);
   end Annotate_Package_Body;

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
     (Decl : Iir_Configuration_Declaration)
   is
      Config_Info: Sim_Info_Acc;
   begin
      pragma Assert (Current_Scope.Kind = Scope_Kind_None);
      Increment_Current_Scope;

      Config_Info := new Sim_Info_Type'
        (Kind => Kind_Block,
         Inst_Slot => Invalid_Instance_Slot,
         Frame_Scope => Current_Scope,
         Nbr_Objects => 0,
         Nbr_Instances => 0);

      Annotate_Declaration_List (Config_Info, Get_Declaration_Chain (Decl));
      Annotate_Block_Configuration (Get_Block_Configuration (Decl));

      Current_Scope := (Kind => Scope_Kind_None);
   end Annotate_Configuration_Declaration;

   package Info_Node is new GNAT.Table
     (Table_Component_Type => Sim_Info_Acc,
      Table_Index_Type => Iir,
      Table_Low_Bound => 2,
      Table_Initial => 1024,
      Table_Increment => 100);

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
   procedure Annotate (Tree: Iir_Design_Unit)
   is
      El: Iir;
   begin
      --  Expand info table.
      Annotate_Expand_Table;

      El := Get_Library_Unit (Tree);
      if Trace_Annotation then
         Ada.Text_IO.Put_Line ("annotating " & Disp_Node (El));
      end if;
      case Get_Kind (El) is
         when Iir_Kind_Entity_Declaration =>
            Annotate_Entity (El);
         when Iir_Kind_Architecture_Body =>
            Annotate_Architecture (El);
         when Iir_Kind_Package_Declaration =>
            Annotate_Package (El);
            declare
               use Std_Package;
            begin
               if El = Standard_Package then
                  --  These types are not in std.standard!
                  Annotate_Type_Definition
                    (Get_Info (El), Convertible_Integer_Type_Definition);
                  Annotate_Type_Definition
                    (Get_Info (El), Convertible_Real_Type_Definition);
               end if;
            end;
         when Iir_Kind_Package_Body =>
            Annotate_Package_Body (El);
         when Iir_Kind_Configuration_Declaration =>
            Annotate_Configuration_Declaration (El);
         when others =>
            Error_Kind ("annotate2", El);
      end case;
   end Annotate;

   function Image (Scope : Scope_Type) return String is
   begin
      case Scope.Kind is
         when Scope_Kind_None =>
            return "none";
         when Scope_Kind_Component =>
            return "component";
         when Scope_Kind_Frame =>
            return "frame" & Scope_Depth_Type'Image (Scope.Depth);
         when Scope_Kind_Package =>
            return "package" & Pkg_Index_Type'Image (Scope.Pkg_Index);
         when Scope_Kind_Pkg_Inst =>
            return "pkg inst" & Parameter_Slot_Type'Image (Scope.Pkg_Inst);
      end case;
   end Image;

   -- Disp annotations for an iir node.
   procedure Disp_Vhdl_Info (Node: Iir) is
      use Ada.Text_IO;
      Indent: Count;
      Info: Sim_Info_Acc;
   begin
      Info := Get_Info (Node);
      Indent := Col;
      case Info.Kind is
         when Kind_Block =>
            Put_Line
              ("-- nbr objects:" & Object_Slot_Type'Image (Info.Nbr_Objects));

         when Kind_Frame | Kind_Process  =>
            Put_Line ("-- scope:" & Image (Info.Frame_Scope));
            Set_Col (Indent);
            Put_Line
              ("-- nbr objects:" & Object_Slot_Type'Image (Info.Nbr_Objects));

         when Kind_Object | Kind_Signal | Kind_File
           | Kind_Terminal | Kind_Quantity =>
            Put_Line ("-- slot:" & Object_Slot_Type'Image (Info.Slot)
                      & ", scope:" & Image (Info.Obj_Scope));
         when Kind_Scalar_Type
           | Kind_File_Type =>
            null;
         when Kind_Range =>
            Put ("${");
            Put (Object_Slot_Type'Image (Info.Slot));
            Put ("}");
      end case;
   end Disp_Vhdl_Info;

   procedure Disp_Info (Info : Sim_Info_Acc)
   is
      use Ada.Text_IO;
      Indent: Count;
   begin
      Indent := Col + 2;
      Set_Col (Indent);
      if Info = null then
         Put_Line ("*null*");
         return;
      end if;
      case Info.Kind is
         when Kind_Block | Kind_Frame | Kind_Process =>
            Put_Line ("scope:" & Image (Info.Frame_Scope));
            Set_Col (Indent);
            Put_Line ("inst_slot:"
                        & Instance_Slot_Type'Image (Info.Inst_Slot));
            Set_Col (Indent);
            Put_Line ("nbr objects:"
                        & Object_Slot_Type'Image (Info.Nbr_Objects));
            Set_Col (Indent);
            Put_Line ("nbr instance:"
                      & Instance_Slot_Type'Image (Info.Nbr_Instances));
         when Kind_Object | Kind_Signal | Kind_File
           | Kind_Terminal | Kind_Quantity =>
            Put_Line ("slot:" & Object_Slot_Type'Image (Info.Slot)
                      & ", scope:" & Image (Info.Obj_Scope));
         when Kind_Range =>
            Put_Line ("range slot:" & Object_Slot_Type'Image (Info.Slot));
         when Kind_Scalar_Type =>
            Put_Line ("scalar type: "
                        & Iir_Value_Kind'Image (Info.Scalar_Mode));
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
end Annotations;
