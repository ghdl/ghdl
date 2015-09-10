--  Package (and subprograms) instantiations

--  When a package is instantiated, we need to 'duplicate' its declaration.
--  This looks useless for analysis but it isn't: a type from a package
--  instantiated twice declares two different types.  Without duplication, we
--  need to attach to each declaration its instance, which looks more expansive
--  that duplicating the declaration.
--
--  Furthermore, for generic type interface, it looks a good idea to duplicate
--  the body (macro expansion).
--
--  Duplicating is not trivial: internal links must be kept and external
--  links preserved.  A table is used to map nodes from the uninstantiated
--  package to its duplicated node.  Links from instantiated declaration to
--  the original declaration are also stored in that table.

with Tables;
with Nodes;
with Nodes_Meta;
with Types; use Types;
with Iirs_Utils; use Iirs_Utils;
with Errorout; use Errorout;

package body Sem_Inst is
   --  Table of origin.  This is an extension of vhdl nodes to track the
   --  origin of a node.  If a node has a non-null origin, then the node was
   --  instantiated for the origin node.
   --
   --  Furthermore, during instantiation, we need to keep track of instantiated
   --  nodes (ie nodes created by instantiation) used by references.  As an
   --  instance cannot be uninstantiated, there is no collisions, as soon as
   --  such entries are cleaned after instantiation.
   --
   --  As an example, here are declarations of an uninstantiated package:
   --    type Nat is range 0 to 1023;
   --    constant N : Nat := 5;
   --  A node Nat1 will be created from node Nat (an integer type definition).
   --  The origin of Nat1 is Nat and this is true forever.  During
   --  instantiation, the instance of Nat is Nat1, so that the type of N will
   --  be set to Nat1.
   package Origin_Table is new Tables
     (Table_Component_Type => Iir,
      Table_Index_Type => Iir,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   procedure Expand_Origin_Table
   is
      use Nodes;
      Last : constant Iir := Iirs.Get_Last_Node;
      El: Iir;
   begin
      El := Origin_Table.Last;
      if El < Last then
         Origin_Table.Set_Last (Last);
         Origin_Table.Table (El + 1 .. Last) := (others => Null_Iir);
      end if;
   end Expand_Origin_Table;

   --  This is the public function; the table may not have been extended.
   function Get_Origin (N : Iir) return Iir
   is
      --  Make the '<=' operator visible.
      use Nodes;
   begin
      if N <= Origin_Table.Last then
         return Origin_Table.Table (N);
      else
         return Null_Iir;
      end if;
   end Get_Origin;

   --  This is the private function: the table *must* have been extended.
   function Get_Instance (N : Iir) return Iir
   is
      --  Make '<=' operator visible for the assert.
      use Nodes;
   begin
      pragma Assert (N <= Origin_Table.Last);
      return Origin_Table.Table (N);
   end Get_Instance;

   procedure Set_Origin (N : Iir; Orig : Iir) is
   begin
      --  As nodes are created, we need to expand origin table.
      Expand_Origin_Table;

      pragma Assert (Orig = Null_Iir
                       or else Origin_Table.Table (N) = Null_Iir);
      Origin_Table.Table (N) := Orig;
   end Set_Origin;

   type Instance_Entry_Type is record
      --  Node
      N : Iir;

      --  Old value in Origin_Table.
      Old_Origin : Iir;
   end record;

   type Instance_Index_Type is new Natural;

   --  Table of previous values in Origin_Table.  The first purpose of this
   --  table is to be able to revert the calls to Set_Instance, so that a unit
   --  can be instantiated several times.  Keep the nodes that have been
   --  instantiated is cheaper than walking the tree a second time.
   --  The second purpose of this table is not yet implemented: being able to
   --  have uninstantiated packages in instantiated packages.  In that case,
   --  the slot in Origin_Table cannot be the origin and the instance at the
   --  same time.
   package Prev_Instance_Table is new Tables
     (Table_Component_Type => Instance_Entry_Type,
      Table_Index_Type => Instance_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 256);

   procedure Set_Instance (Orig : Iir; N : Iir)
   is
      use Nodes;
   begin
      pragma Assert (Orig <= Origin_Table.Last);

      --  Save the old entry
      Prev_Instance_Table.Append
        (Instance_Entry_Type'(N => Orig,
                              Old_Origin => Origin_Table.Table (Orig)));

      --  Set the entry.
      Origin_Table.Table (Orig) := N;
   end Set_Instance;

   procedure Restore_Origin (Mark : Instance_Index_Type) is
   begin
      for I in reverse Mark + 1 .. Prev_Instance_Table.Last loop
         declare
            El : Instance_Entry_Type renames Prev_Instance_Table.Table (I);
         begin
            Origin_Table.Table (El.N) := El.Old_Origin;
         end;
      end loop;
      Prev_Instance_Table.Set_Last (Mark);
   end Restore_Origin;

   --  The location to be used while instantiated nodes.
   Instantiate_Loc : Location_Type;

   function Instantiate_Iir (N : Iir; Is_Ref : Boolean) return Iir;

   --  Instantiate a list.  Simply create a new list and instantiate nodes of
   --  that list.
   function Instantiate_Iir_List (L : Iir_List; Is_Ref : Boolean)
                                 return Iir_List
   is
      Res : Iir_List;
      El : Iir;
   begin
      case L is
         when Null_Iir_List
           | Iir_List_All
           | Iir_List_Others =>
            return L;
         when others =>
            Res := Create_Iir_List;
            for I in Natural loop
               El := Get_Nth_Element (L, I);
               exit when El = Null_Iir;
               Append_Element (Res, Instantiate_Iir (El, Is_Ref));
            end loop;
            return Res;
      end case;
   end Instantiate_Iir_List;

   --  Instantiate a chain.  This is a special case to reduce stack depth.
   function Instantiate_Iir_Chain (N : Iir) return Iir
   is
      First : Iir;
      Last : Iir;
      Next_N : Iir;
      Next_R : Iir;
   begin
      if N = Null_Iir then
         return Null_Iir;
      end if;

      First := Instantiate_Iir (N, False);
      Last := First;
      Next_N := Get_Chain (N);
      while Next_N /= Null_Iir loop
         Next_R := Instantiate_Iir (Next_N, False);
         Set_Chain (Last, Next_R);
         Last := Next_R;
         Next_N := Get_Chain (Next_N);
      end loop;

      return First;
   end Instantiate_Iir_Chain;

   procedure Instantiate_Iir_Field
     (Res : Iir; N : Iir; F : Nodes_Meta.Fields_Enum)
   is
      use Nodes_Meta;
   begin
      case Get_Field_Type (F) is
         when Type_Iir =>
            declare
               S : constant Iir := Get_Iir (N, F);
               R : Iir;
            begin
               case Get_Field_Attribute (F) is
                  when Attr_None =>
                     R := Instantiate_Iir (S, False);
                  when Attr_Ref =>
                     R := Instantiate_Iir (S, True);
                  when Attr_Maybe_Ref =>
                     R := Instantiate_Iir (S, Get_Is_Ref (N));
                  when Attr_Chain =>
                     R := Instantiate_Iir_Chain (S);
                  when Attr_Chain_Next =>
                     R := Null_Iir;
                  when Attr_Of_Ref =>
                     --  Can only appear in list.
                     raise Internal_Error;
               end case;
               Set_Iir (Res, F, R);
            end;
         when Type_Iir_List =>
            declare
               S : constant Iir_List := Get_Iir_List (N, F);
               R : Iir_List;
            begin
               case Get_Field_Attribute (F) is
                  when Attr_None =>
                     R := Instantiate_Iir_List (S, False);
                  when Attr_Of_Ref =>
                     R := Instantiate_Iir_List (S, True);
                  when others =>
                     --  Ref is specially handled in Instantiate_Iir.
                     --  Others cannot appear for lists.
                     raise Internal_Error;
               end case;
               Set_Iir_List (Res, F, R);
            end;
         when Type_PSL_NFA
           | Type_PSL_Node =>
            --  TODO
            raise Internal_Error;
         when Type_String8_Id =>
            Set_String8_Id (Res, F, Get_String8_Id (N, F));
         when Type_Source_Ptr =>
            Set_Source_Ptr (Res, F, Get_Source_Ptr (N, F));
         when Type_Date_Type
           | Type_Date_State_Type
           | Type_Time_Stamp_Id
           | Type_File_Checksum_Id =>
            --  Can this happen ?
            raise Internal_Error;
         when Type_Base_Type =>
            Set_Base_Type (Res, F, Get_Base_Type (N, F));
         when Type_Iir_Constraint =>
            Set_Iir_Constraint (Res, F, Get_Iir_Constraint (N, F));
         when Type_Iir_Mode =>
            Set_Iir_Mode (Res, F, Get_Iir_Mode (N, F));
         when Type_Iir_Index32 =>
            Set_Iir_Index32 (Res, F, Get_Iir_Index32 (N, F));
         when Type_Iir_Int64 =>
            Set_Iir_Int64 (Res, F, Get_Iir_Int64 (N, F));
         when Type_Boolean =>
            Set_Boolean (Res, F, Get_Boolean (N, F));
         when Type_Iir_Staticness =>
            Set_Iir_Staticness (Res, F, Get_Iir_Staticness (N, F));
         when Type_Iir_All_Sensitized =>
            Set_Iir_All_Sensitized (Res, F, Get_Iir_All_Sensitized (N, F));
         when Type_Iir_Signal_Kind =>
            Set_Iir_Signal_Kind (Res, F, Get_Iir_Signal_Kind (N, F));
         when Type_Tri_State_Type =>
            Set_Tri_State_Type (Res, F, Get_Tri_State_Type (N, F));
         when Type_Iir_Pure_State =>
            Set_Iir_Pure_State (Res, F, Get_Iir_Pure_State (N, F));
         when Type_Iir_Delay_Mechanism =>
            Set_Iir_Delay_Mechanism (Res, F, Get_Iir_Delay_Mechanism (N, F));
         when Type_Iir_Predefined_Functions =>
            Set_Iir_Predefined_Functions
              (Res, F, Get_Iir_Predefined_Functions (N, F));
         when Type_Iir_Direction =>
            Set_Iir_Direction (Res, F, Get_Iir_Direction (N, F));
         when Type_Location_Type =>
            Set_Location_Type (Res, F, Instantiate_Loc);
         when Type_Iir_Int32 =>
            Set_Iir_Int32 (Res, F, Get_Iir_Int32 (N, F));
         when Type_Int32 =>
            Set_Int32 (Res, F, Get_Int32 (N, F));
         when Type_Iir_Fp64 =>
            Set_Iir_Fp64 (Res, F, Get_Iir_Fp64 (N, F));
         when Type_Token_Type =>
            Set_Token_Type (Res, F, Get_Token_Type (N, F));
         when Type_Name_Id =>
            Set_Name_Id (Res, F, Get_Name_Id (N, F));
      end case;
   end Instantiate_Iir_Field;

   function Instantiate_Iir (N : Iir; Is_Ref : Boolean) return Iir
   is
      Res : Iir;
   begin
      --  Nothing to do for null node.
      if N = Null_Iir then
         return Null_Iir;
      end if;

      --  For a reference, do not create a new node.
      if Is_Ref then
         Res := Get_Instance (N);
         if Res /= Null_Iir then
            --  There is an instance for N.
            return Res;
         else
            --  Reference outside the instance.
            return N;
         end if;
      end if;

      declare
         use Nodes_Meta;
         Kind : constant Iir_Kind := Get_Kind (N);
         Fields : constant Fields_Array := Get_Fields (Kind);
         F : Fields_Enum;
      begin
         Res := Get_Instance (N);

         if Kind = Iir_Kind_Interface_Constant_Declaration
           and then Get_Identifier (N) = Null_Identifier
           and then Res /= Null_Iir
         then
            --  Anonymous constant interface declarations are the only nodes
            --  that can be shared.  Handle that very special case.
            return Res;
         end if;

         pragma Assert (Res = Null_Iir);

         --  Create a new node.
         Res := Create_Iir (Kind);

         --  The origin of this new node is N.
         Set_Origin (Res, N);

         --  And the instance of N is RES.
         Set_Instance (N, Res);

         Set_Location (Res, Instantiate_Loc);

         for I in Fields'Range loop
            F := Fields (I);

            --  Fields that are handled specially.
            case F is
               when Field_Index_Subtype_List =>
                  --  Index_Subtype_List is always a reference, so retrieve
                  --  the instance of the referenced list.  This is a special
                  --  case because there is no origins for list.
                  declare
                     List : Iir_List;
                  begin
                     case Kind is
                        when Iir_Kind_Array_Type_Definition =>
                           List := Get_Index_Subtype_Definition_List (Res);
                        when Iir_Kind_Array_Subtype_Definition =>
                           List := Get_Index_Constraint_List (Res);
                           if List = Null_Iir_List then
                              List := Get_Index_Subtype_List
                                (Get_Denoted_Type_Mark (Res));
                           end if;
                        when others =>
                           --  All the nodes where Index_Subtype_List appears
                           --  are handled above.
                           raise Internal_Error;
                     end case;
                     Set_Index_Subtype_List (Res, List);
                  end;

               when others =>
                  --  Common case.
                  Instantiate_Iir_Field (Res, N, F);
            end case;
         end loop;

         case Kind is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               --  Subprogram body is a forward declaration.
               Set_Subprogram_Body (Res, Null_Iir);
            when others =>
               --  TODO: other forward references:
               --  incomplete constant
               --  attribute_value
               null;
         end case;

         return Res;
      end;
   end Instantiate_Iir;

   --  As the scope generic interfaces extends beyond the immediate scope (see
   --  LRM08 12.2 Scope of declarations), they must be instantiated.
   function Instantiate_Generic_Chain (Inst : Iir; Inters : Iir) return Iir
   is
      Inter : Iir;
      First : Iir;
      Last : Iir;
      Res : Iir;
   begin
      First := Null_Iir;
      Last := Null_Iir;

      Inter := Inters;
      while Inter /= Null_Iir loop
         --  Create a copy of the interface.  FIXME: is it really needed ?
         Res := Create_Iir (Get_Kind (Inter));
         Set_Location (Res, Instantiate_Loc);
         Set_Parent (Res, Inst);
         Set_Identifier (Res, Get_Identifier (Inter));
         Set_Visible_Flag (Res, Get_Visible_Flag (Inter));

         Set_Origin (Res, Inter);
         Set_Instance (Inter, Res);

         case Get_Kind (Res) is
            when Iir_Kind_Interface_Constant_Declaration =>
               Set_Type (Res, Get_Type (Inter));
               Set_Subtype_Indication (Res, Get_Subtype_Indication (Inter));
               Set_Mode (Res, Get_Mode (Inter));
               Set_Has_Mode (Res, Get_Has_Mode (Inter));
               Set_Has_Class (Res, Get_Has_Class (Inter));
               Set_Has_Identifier_List (Res, Get_Has_Identifier_List (Inter));
               Set_Expr_Staticness (Res, Get_Expr_Staticness (Inter));
               Set_Name_Staticness (Res, Get_Name_Staticness (Inter));
            when Iir_Kind_Interface_Package_Declaration =>
               Set_Uninstantiated_Package_Name
                 (Res, Get_Uninstantiated_Package_Name (Inter));
            when others =>
               Error_Kind ("instantiate_generic_chain", Res);
         end case;

         --  Append
         if First = Null_Iir then
            First := Res;
         else
            Set_Chain (Last, Res);
         end if;
         Last := Res;

         Inter := Get_Chain (Inter);
      end loop;

      return First;
   end Instantiate_Generic_Chain;

   procedure Set_Instance_On_Chain (Chain : Iir; Inst_Chain : Iir);
   procedure Set_Instance_On_Iir_List (N : Iir_List; Inst : Iir_List);

   procedure Set_Instance_On_Iir (N : Iir; Inst : Iir) is
   begin
      if N = Null_Iir then
         pragma Assert (Inst = Null_Iir);
         return;
      end if;
      pragma Assert (Inst /= Null_Iir);

      declare
         use Nodes_Meta;
         Kind : constant Iir_Kind := Get_Kind (N);
         Fields : constant Fields_Array := Get_Fields (Kind);
         F : Fields_Enum;
      begin
         pragma Assert (Get_Kind (Inst) = Kind);

         if Kind = Iir_Kind_Interface_Constant_Declaration
           and then Get_Identifier (N) = Null_Identifier
         then
            --  Anonymous constant interface declarations are the only nodes
            --  that can be shared.  Handle that very special case.
            return;
         end if;

         --  pragma Assert (Get_Instance (N) = Null_Iir);
         Set_Instance (N, Inst);

         for I in Fields'Range loop
            F := Fields (I);

            case Get_Field_Type (F) is
               when Type_Iir =>
                  declare
                     S : constant Iir := Get_Iir (N, F);
                     S_Inst : constant Iir := Get_Iir (Inst, F);
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Set_Instance_On_Iir (S, S_Inst);
                        when Attr_Ref =>
                           null;
                        when Attr_Maybe_Ref =>
                           if not Get_Is_Ref (N) then
                              Set_Instance_On_Iir (S, S_Inst);
                           end if;
                        when Attr_Chain =>
                           Set_Instance_On_Chain (S, S_Inst);
                        when Attr_Chain_Next =>
                           null;
                        when Attr_Of_Ref =>
                           --  Can only appear in list.
                           raise Internal_Error;
                     end case;
                  end;
               when Type_Iir_List =>
                  declare
                     S : constant Iir_List := Get_Iir_List (N, F);
                     S_Inst : constant Iir_List := Get_Iir_List (Inst, F);
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Set_Instance_On_Iir_List (S, S_Inst);
                        when Attr_Of_Ref
                          | Attr_Ref =>
                           null;
                        when others =>
                           --  Ref is specially handled in Instantiate_Iir.
                           --  Others cannot appear for lists.
                           raise Internal_Error;
                     end case;
                  end;
               when others =>
                  null;
            end case;
         end loop;
      end;
   end Set_Instance_On_Iir;

   procedure Set_Instance_On_Iir_List (N : Iir_List; Inst : Iir_List)
   is
      El : Iir;
      El_Inst : Iir;
   begin
      case N is
         when Null_Iir_List
           | Iir_List_All
           | Iir_List_Others =>
            pragma Assert (Inst = N);
            return;
         when others =>
            for I in Natural loop
               El := Get_Nth_Element (N, I);
               El_Inst := Get_Nth_Element (Inst, I);
               exit when El = Null_Iir;
               pragma Assert (El_Inst /= Null_Iir);

               Set_Instance_On_Iir (El, El_Inst);
            end loop;
            pragma Assert (El_Inst = Null_Iir);
      end case;
   end Set_Instance_On_Iir_List;

   procedure Set_Instance_On_Chain (Chain : Iir; Inst_Chain : Iir)
   is
      El : Iir;
      Inst_El : Iir;
   begin
      El := Chain;
      Inst_El := Inst_Chain;
      while El /= Null_Iir loop
         pragma Assert (Inst_El /= Null_Iir);
         Set_Instance_On_Iir (El, Inst_El);
         El := Get_Chain (El);
         Inst_El := Get_Chain (Inst_El);
      end loop;
      pragma Assert (Inst_El = Null_Iir);
   end Set_Instance_On_Chain;

   --  In the instance, replace references (and inner references) to interface
   --  package declaration to the associated package.
   procedure Instantiate_Generic_Map_Chain (Inst : Iir; Pkg : Iir)
   is
      pragma Unreferenced (Pkg);
      Assoc : Iir;
   begin
      Assoc := Get_Generic_Map_Aspect_Chain (Inst);
      while Assoc /= Null_Iir loop
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Expression
              | Iir_Kind_Association_Element_By_Individual
              | Iir_Kind_Association_Element_Open =>
               null;
            when Iir_Kind_Association_Element_Package =>
               declare
                  Sub_Inst : constant Iir :=
                    Get_Named_Entity (Get_Actual (Assoc));
                  Sub_Pkg : constant Iir := Get_Associated_Interface (Assoc);
               begin
                  Set_Instance (Sub_Pkg, Sub_Inst);
                  Set_Instance_On_Chain (Get_Generic_Chain (Sub_Pkg),
                                         Get_Generic_Chain (Sub_Inst));
                  Set_Instance_On_Chain (Get_Declaration_Chain (Sub_Pkg),
                                        Get_Declaration_Chain (Sub_Inst));
               end;
            when others =>
               Error_Kind ("instantiate_generic_map_chain", Assoc);
         end case;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Instantiate_Generic_Map_Chain;

   procedure Instantiate_Package_Declaration (Inst : Iir; Pkg : Iir)
   is
      Header : constant Iir := Get_Package_Header (Pkg);
      Prev_Loc : constant Location_Type := Instantiate_Loc;
      Mark : constant Instance_Index_Type := Prev_Instance_Table.Last;
   begin
      Instantiate_Loc := Get_Location (Inst);

      --  Be sure Get_Origin_Priv can be called on existing nodes.
      Expand_Origin_Table;

      --  For Parent: the instance of PKG is INST.
      Set_Origin (Pkg, Inst);

      Set_Generic_Chain
        (Inst, Instantiate_Generic_Chain (Inst, Get_Generic_Chain (Header)));
      Instantiate_Generic_Map_Chain (Inst, Pkg);
      Set_Declaration_Chain
        (Inst, Instantiate_Iir_Chain (Get_Declaration_Chain (Pkg)));

      Set_Origin (Pkg, Null_Iir);

      Instantiate_Loc := Prev_Loc;
      Restore_Origin (Mark);
   end Instantiate_Package_Declaration;
end Sem_Inst;
