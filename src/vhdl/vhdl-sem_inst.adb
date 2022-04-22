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
with Vhdl.Nodes_Priv;
with Vhdl.Nodes_Meta;
with Types; use Types;
with Files_Map;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Sem_Utils;

package body Vhdl.Sem_Inst is
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
      use Vhdl.Nodes_Priv;
      Last : constant Iir := Nodes.Get_Last_Node;
      El : constant Iir := Origin_Table.Last;
   begin
      if El < Last then
         Origin_Table.Set_Last (Last);
         Origin_Table.Table (El + 1 .. Last) := (others => Null_Iir);
      end if;
   end Expand_Origin_Table;

   --  This is the public function; the table may not have been extended.
   function Get_Origin (N : Iir) return Iir
   is
      --  Make the '<=' operator visible.
      use Vhdl.Nodes_Priv;
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
      use Vhdl.Nodes_Priv;
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
   --  can be instantiated several times.  Keeping the nodes that have been
   --  instantiated is cheaper than walking the tree a second time.
   --  The second purpose of this table is to be able to have uninstantiated
   --  packages in instantiated packages.  In that case, the slot in
   --  Origin_Table cannot be the origin and the instance at the same time and
   --  has to be saved.
   package Prev_Instance_Table is new Tables
     (Table_Component_Type => Instance_Entry_Type,
      Table_Index_Type => Instance_Index_Type,
      Table_Low_Bound => 1,
      Table_Initial => 256);

   --  The instance of ORIG is now N.  So during instantiation, a reference
   --  to ORIG will be replaced by a reference to N.  The previous instance
   --  of ORIG is saved.
   procedure Set_Instance (Orig : Iir; N : Iir)
   is
      use Vhdl.Nodes_Priv;
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

   --  The virtual file for the instance.
   Instance_File : Source_File_Entry;

   --  True if currently instantiated a shared generic.
   Is_Within_Shared_Instance : Boolean := False;

   --  Get the new location.
   function Relocate (Loc : Location_Type) return Location_Type is
   begin
      if Instance_File /= No_Source_File_Entry then
         --  For Instantiate.
         return Files_Map.Instance_Relocate (Instance_File, Loc);
      else
         --  For Copy_Tree.
         return Loc;
      end if;
   end Relocate;

   procedure Create_Relocation (Inst : Iir; Orig : Iir)
   is
      use Files_Map;
      Orig_File : Source_File_Entry;
      Pos : Source_Ptr;
   begin
      Location_To_File_Pos (Get_Location (Orig), Orig_File, Pos);
      Instance_File := Create_Instance_Source_File
        (Orig_File, Get_Location (Inst), Inst);
   end Create_Relocation;

   function Instantiate_Iir (N : Iir; Is_Ref : Boolean) return Iir;

   --  Instantiate a list.  Simply create a new list and instantiate nodes of
   --  that list.
   function Instantiate_Iir_List (L : Iir_List; Is_Ref : Boolean)
                                 return Iir_List
   is
      Res : Iir_List;
      It : List_Iterator;
      El : Iir;
   begin
      case L is
         when Null_Iir_List
           | Iir_List_All =>
            return L;
         when others =>
            Res := Create_Iir_List;
            It := List_Iterate (L);
            while Is_Valid (It) loop
               El := Get_Element (It);
               Append_Element (Res, Instantiate_Iir (El, Is_Ref));
               Next (It);
            end loop;
            return Res;
      end case;
   end Instantiate_Iir_List;

   function Instantiate_Iir_Flist (L : Iir_Flist; Is_Ref : Boolean)
                                  return Iir_Flist
   is
      Res : Iir_Flist;
      El : Iir;
   begin
      case L is
         when Null_Iir_Flist
           | Iir_Flist_All
           | Iir_Flist_Others =>
            return L;
         when others =>
            Res := Create_Iir_Flist (Get_Nbr_Elements (L));
            for I in Flist_First .. Flist_Last (L) loop
               El := Get_Nth_Element (L, I);
               Set_Nth_Element (Res, I, Instantiate_Iir (El, Is_Ref));
            end loop;
            return Res;
      end case;
   end Instantiate_Iir_Flist;

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
                  when Attr_Forward_Ref =>
                     --  Must be explicitly handled in Instantiate_Iir, as it
                     --  requires special handling.
                     raise Internal_Error;
                  when Attr_Maybe_Forward_Ref =>
                     if Get_Is_Forward_Ref (N) then
                        --  Likewise: must be explicitly handled.
                        raise Internal_Error;
                     else
                        R := Instantiate_Iir (S, True);
                     end if;
                  when Attr_Chain =>
                     R := Instantiate_Iir_Chain (S);
                  when Attr_Chain_Next =>
                     R := Null_Iir;
                  when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                     --  Can only appear in list.
                     raise Internal_Error;
               end case;
               Set_Iir (Res, F, R);
            end;
         when Type_Iir_List =>
            declare
               S : constant Iir_List := Get_Iir_List (N, F);
               R : Iir_List;
               Ref : Boolean;
            begin
               case Get_Field_Attribute (F) is
                  when Attr_None =>
                     Ref := False;
                  when Attr_Of_Ref =>
                     Ref := True;
                  when Attr_Of_Maybe_Ref =>
                     Ref := Get_Is_Ref (N);
                  when others =>
                     --  Ref is specially handled in Instantiate_Iir.
                     --  Others cannot appear for lists.
                     raise Internal_Error;
               end case;
               R := Instantiate_Iir_List (S, Ref);
               Set_Iir_List (Res, F, R);
            end;
         when Type_Iir_Flist =>
            declare
               S : constant Iir_Flist := Get_Iir_Flist (N, F);
               R : Iir_Flist;
               Ref : Boolean;
            begin
               case Get_Field_Attribute (F) is
                  when Attr_None =>
                     Ref := False;
                  when Attr_Of_Ref =>
                     Ref := True;
                  when Attr_Of_Maybe_Ref =>
                     Ref := Get_Is_Ref (N);
                  when others =>
                     --  Ref is specially handled in Instantiate_Iir.
                     --  Others cannot appear for lists.
                     raise Internal_Error;
               end case;
               R := Instantiate_Iir_Flist (S, Ref);
               Set_Iir_Flist (Res, F, R);
            end;
         when Type_PSL_NFA
           | Type_PSL_Node =>
            --  TODO
            raise Internal_Error;
         when Type_String8_Id =>
            Set_String8_Id (Res, F, Get_String8_Id (N, F));
         when Type_Source_Ptr =>
            Set_Source_Ptr (Res, F, Get_Source_Ptr (N, F));
         when Type_Source_File_Entry =>
            Set_Source_File_Entry (Res, F, Get_Source_File_Entry (N, F));
         when Type_Date_Type
           | Type_Date_State_Type
           | Type_Time_Stamp_Id
           | Type_File_Checksum_Id =>
            --  Can this happen ?
            raise Internal_Error;
         when Type_Number_Base_Type =>
            Set_Number_Base_Type (Res, F, Get_Number_Base_Type (N, F));
         when Type_Iir_Constraint =>
            Set_Iir_Constraint (Res, F, Get_Iir_Constraint (N, F));
         when Type_Iir_Mode =>
            Set_Iir_Mode (Res, F, Get_Iir_Mode (N, F));
         when Type_Iir_Index32 =>
            Set_Iir_Index32 (Res, F, Get_Iir_Index32 (N, F));
         when Type_Int64 =>
            Set_Int64 (Res, F, Get_Int64 (N, F));
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
         when Type_Iir_Force_Mode =>
            Set_Iir_Force_Mode (Res, F, Get_Iir_Force_Mode (N, F));
         when Type_Iir_Predefined_Functions =>
            Set_Iir_Predefined_Functions
              (Res, F, Get_Iir_Predefined_Functions (N, F));
         when Type_Direction_Type =>
            Set_Direction_Type (Res, F, Get_Direction_Type (N, F));
         when Type_Iir_Int32 =>
            Set_Iir_Int32 (Res, F, Get_Iir_Int32 (N, F));
         when Type_Int32 =>
            Set_Int32 (Res, F, Get_Int32 (N, F));
         when Type_Fp64 =>
            Set_Fp64 (Res, F, Get_Fp64 (N, F));
         when Type_Token_Type =>
            Set_Token_Type (Res, F, Get_Token_Type (N, F));
         when Type_Scalar_Size =>
            Set_Scalar_Size (Res, F, Get_Scalar_Size (N, F));
         when Type_Name_Id =>
            Set_Name_Id (Res, F, Get_Name_Id (N, F));
      end case;
   end Instantiate_Iir_Field;

   --  Set designated_entity of attribute_value from attribute_value_chain
   --  of RES.
   procedure Instantiate_Attribute_Value_Chain (Res : Iir)
   is
      Val : Iir;
      Ref_Ent : Iir;
   begin
      Val := Get_Attribute_Value_Chain (Res);
      while Val /= Null_Iir loop
         pragma Assert (Get_Designated_Entity (Val) = Null_Iir);
         Ref_Ent := Get_Designated_Entity (Get_Origin (Val));
         Ref_Ent := Instantiate_Iir (Ref_Ent, True);
         Set_Designated_Entity (Val, Ref_Ent);
         Val := Get_Value_Chain (Val);
      end loop;
   end Instantiate_Attribute_Value_Chain;

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
         --  In general, Get_Instance (N) is Null_Iir.  There are two
         --  exceptions:
         --  - N is also an instance (instance within an uninstantiated
         --    package).  As instances and origin share the same table,
         --    Get_Instance returns the origin.  During instantiation, the old
         --    value of Origin is saved so this case is correctly handled.
         --  - N is shared, so it was already instantiated.  This happends only
         --    for interface_constant of implicit operators.  In that case,
         --    multiple instances are created for the same node, which is not
         --    ideal.  That's still ok (if no infos are attached to the
         --    interface) and is the price to pay for this optimization.

         --  Create a new node.
         Res := Create_Iir (Kind);

         --  The origin of this new node is N.
         Set_Origin (Res, N);

         --  And the instance of N is RES.
         Set_Instance (N, Res);

         Set_Location (Res, Relocate (Get_Location (N)));

         for I in Fields'Range loop
            F := Fields (I);

            --  Fields that are handled specially.
            case F is
               when Field_Index_Subtype_List =>
                  --  Index_Subtype_List is always a reference, so retrieve
                  --  the instance of the referenced list.  This is a special
                  --  case because there is no origins for list.
                  declare
                     List : Iir_Flist;
                  begin
                     case Kind is
                        when Iir_Kind_Array_Type_Definition =>
                           List := Get_Index_Subtype_Definition_List (Res);
                        when Iir_Kind_Array_Subtype_Definition =>
                           List := Get_Index_Constraint_List (Res);
                           if List = Null_Iir_Flist then
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

               when Field_Simple_Aggregate_List =>
                  Set_Simple_Aggregate_List
                    (Res, Get_Simple_Aggregate_List (N));

               when Field_Subprogram_Body =>
                  --  This is a forward reference.  Not yet solved.
                  Set_Subprogram_Body (Res, Null_Iir);

               when Field_Subprogram_Specification =>
                  --  Resolve it.
                  Instantiate_Iir_Field (Res, N, F);

                  --  Set body.
                  pragma Assert (Kind_In (Res, Iir_Kind_Procedure_Body,
                                          Iir_Kind_Function_Body));
                  declare
                     Spec : constant Iir := Get_Subprogram_Specification (Res);
                  begin
                     pragma Assert (Get_Subprogram_Body (Spec) = Null_Iir);
                     Set_Subprogram_Body (Spec, Res);
                  end;

               when Field_Incomplete_Type_Ref_Chain =>
                  if Get_Kind (Res) = Iir_Kind_Access_Type_Definition then
                     --  Link
                     declare
                        Def : constant Iir := Get_Named_Entity
                          (Get_Designated_Subtype_Indication (Res));
                     begin
                        if Get_Kind (Def) = Iir_Kind_Incomplete_Type_Definition
                        then
                           Set_Incomplete_Type_Ref_Chain
                             (Res, Get_Incomplete_Type_Ref_Chain (Def));
                           Set_Incomplete_Type_Ref_Chain (Def, Res);
                        end if;
                     end;
                  end if;

               when Field_Designated_Type =>
                  null;
               when Field_Designated_Subtype_Indication =>
                  Instantiate_Iir_Field (Res, N, F);
                  --  The designated type will be patched later if it is an
                  --  incomplete type definition
                  Set_Designated_Type
                    (Res, Get_Type (Get_Designated_Subtype_Indication (Res)));

               when Field_Complete_Type_Definition =>
                  --  Will be set by the declaration of the complete type
                  null;
               when Field_Incomplete_Type_Declaration =>
                  Instantiate_Iir_Field (Res, N, F);
                  declare
                     Res_Decl : constant Iir :=
                       Get_Incomplete_Type_Declaration (Res);
                     N_Decl : constant Iir :=
                       Get_Incomplete_Type_Declaration (N);
                     Res_Complete : Iir;
                     N_Def, Res_Def : Iir;
                     N_El, Next_N_El : Iir;
                     Res_El, Next_Res_El : Iir;
                  begin
                     if Is_Valid (N_Decl) then
                        --  N/RES completes a type declaration.
                        N_Def := Get_Type_Definition (N_Decl);
                        Res_Def := Get_Type_Definition (Res_Decl);
                        --  Set Complete_Type_Definition
                        Res_Complete := Get_Type (Res);
                        Set_Complete_Type_Definition (Res_Def, Res_Complete);
                        --  Rebuild the list and patch designated types
                        N_El := N_Def;
                        Res_El := Res_Def;
                        loop
                           Next_N_El := Get_Incomplete_Type_Ref_Chain (N_El);
                           exit when Is_Null (Next_N_El);
                           Next_Res_El := Get_Instance (Next_N_El);
                           Set_Designated_Type (Next_Res_El, Res_Complete);
                           Set_Incomplete_Type_Ref_Chain (Res_El, Next_Res_El);
                           N_El := Next_N_El;
                        end loop;
                     end if;
                  end;

               when Field_Deferred_Declaration =>
                  if not Get_Deferred_Declaration_Flag (N)
                    and then Is_Valid (Get_Deferred_Declaration (N))
                  then
                     --  This is the completion.
                     declare
                        Incomplete_Decl_N : constant Iir :=
                          Get_Deferred_Declaration (N);
                        Incomplete_Decl_Res : constant Iir :=
                          Get_Instance (Incomplete_Decl_N);
                     begin
                        pragma Assert (Is_Valid (Incomplete_Decl_Res));
                        Set_Deferred_Declaration (Res, Incomplete_Decl_Res);
                        Set_Deferred_Declaration (Incomplete_Decl_Res, Res);
                     end;
                  end if;

               when Field_Protected_Type_Body =>
                  null;
               when Field_Protected_Type_Declaration =>
                  Instantiate_Iir_Field (Res, N, F);
                  Set_Protected_Type_Body
                    (Get_Protected_Type_Declaration (Res), Res);

               when Field_Package_Body =>
                  null;
               when Field_Package =>
                  Instantiate_Iir_Field (Res, N, F);
                  declare
                     Pkg : constant Iir := Get_Package (Res);
                  begin
                     --  The current node can be the body of a package; in that
                     --  case set the forward link.
                     --  Or it can be the body of an instantiated package; in
                     --  that case there is no forward link.
                     if Get_Kind (Pkg) = Iir_Kind_Package_Declaration then
                        Set_Package_Body (Get_Package (Res), Res);
                     end if;
                  end;

               when Field_Instance_Package_Body =>
                  --  Do not instantiate the body of a package while
                  --  instantiating a shared package.
                  if not Is_Within_Shared_Instance then
                     Instantiate_Iir_Field (Res, N, F);
                  end if;

               when Field_Subtype_Definition =>
                  --  TODO
                  null;

               when Field_Instance_Source_File =>
                  Set_Instance_Source_File
                    (Res, Files_Map.Create_Instance_Source_File
                       (Get_Instance_Source_File (N),
                        Get_Location (Res), Res));

               when Field_Generic_Chain
                 | Field_Declaration_Chain =>
                  if Kind = Iir_Kind_Package_Instantiation_Declaration then
                     declare
                        Prev_Instance_File : constant Source_File_Entry :=
                          Instance_File;
                     begin
                        --  Also relocate the instantiated declarations.
                        Instance_File := Get_Instance_Source_File (Res);
                        pragma Assert (Instance_File /= No_Source_File_Entry);
                        Instantiate_Iir_Field (Res, N, F);
                        Instance_File := Prev_Instance_File;
                     end;
                  else
                     Instantiate_Iir_Field (Res, N, F);
                  end if;

               when Field_Designated_Entity =>
                  --  This is a field of attribute_value.  It is a
                  --  forward_ref because it may reference a statement.
                  --  Handle it later.
                  null;

               when Field_Attribute_Value_Chain =>
                  --  Chain of attribute_value for a scope parent.  This is
                  --  a ref.  As the field is declared after the declarations
                  --  and statements of the scope, the attribute_value have
                  --  been instantiated.  So the reference can be resolved.
                  Instantiate_Iir_Field (Res, N, F);

                  --  However, the designated_entity of attribute_value have
                  --  not been resolved.  As they are now instantiated, the
                  --  forward_ref links can be fixed.
                  Instantiate_Attribute_Value_Chain (Res);

               when others =>
                  --  Common case.
                  Instantiate_Iir_Field (Res, N, F);
            end case;
         end loop;

         --  TODO: other forward references:
         --  incomplete constant
         --  incomplete type

         if Get_Kind (Res) in Iir_Kinds_Subprogram_Declaration then
            --  Recompute the hash as the interface may have
            --  changed due to instantiation.
            Sem_Utils.Compute_Subprogram_Hash (Res);
         end if;

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
         Set_Location (Res, Relocate (Get_Location (Inter)));

         Set_Parent (Res, Inst);
         Set_Identifier (Res, Get_Identifier (Inter));
         Set_Visible_Flag (Res, Get_Visible_Flag (Inter));

         Set_Origin (Res, Inter);
         Set_Instance (Inter, Res);

         case Get_Kind (Res) is
            when Iir_Kind_Interface_Constant_Declaration =>
               Set_Type (Res, Get_Type (Inter));
               Set_Subtype_Indication (Res, Null_Iir); --  Not owner
               Set_Mode (Res, Get_Mode (Inter));
               Set_Has_Mode (Res, Get_Has_Mode (Inter));
               Set_Has_Class (Res, Get_Has_Class (Inter));
               Set_Has_Identifier_List (Res, Get_Has_Identifier_List (Inter));
               Set_Expr_Staticness (Res, Get_Expr_Staticness (Inter));
               Set_Name_Staticness (Res, Get_Name_Staticness (Inter));
               Set_Default_Value (Res, Get_Default_Value (Inter));
               Set_Is_Ref (Res, True);
            when Iir_Kind_Interface_Package_Declaration =>
               Set_Uninstantiated_Package_Decl
                 (Res, Get_Uninstantiated_Package_Decl (Inter));
               Set_Generic_Chain
                 (Res,
                  Instantiate_Generic_Chain (Res, Get_Generic_Chain (Inter)));
               Set_Declaration_Chain
                 (Res, Instantiate_Iir_Chain (Get_Declaration_Chain (Inter)));
            when Iir_Kind_Interface_Type_Declaration =>
               Set_Type (Res, Get_Type (Inter));
            when Iir_Kinds_Interface_Subprogram_Declaration =>
               Sem_Utils.Compute_Subprogram_Hash (Res);
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
   procedure Set_Instance_On_Iir_Flist (N : Iir_Flist; Inst : Iir_Flist);

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
                        when Attr_Ref
                          | Attr_Forward_Ref
                          | Attr_Maybe_Forward_Ref =>
                           null;
                        when Attr_Maybe_Ref =>
                           if not Get_Is_Ref (N) then
                              Set_Instance_On_Iir (S, S_Inst);
                           end if;
                        when Attr_Chain =>
                           Set_Instance_On_Chain (S, S_Inst);
                        when Attr_Chain_Next =>
                           null;
                        when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
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
                        when Attr_Of_Maybe_Ref =>
                           if not Get_Is_Ref (N) then
                              Set_Instance_On_Iir_List (S, S_Inst);
                           end if;
                        when Attr_Of_Ref
                          | Attr_Ref
                          | Attr_Forward_Ref =>
                           null;
                        when others =>
                           --  Ref is specially handled in Instantiate_Iir.
                           --  Others cannot appear for lists.
                           raise Internal_Error;
                     end case;
                  end;
               when Type_Iir_Flist =>
                  declare
                     S : constant Iir_Flist := Get_Iir_Flist (N, F);
                     S_Inst : constant Iir_Flist := Get_Iir_Flist (Inst, F);
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Set_Instance_On_Iir_Flist (S, S_Inst);
                        when Attr_Of_Maybe_Ref =>
                           if not Get_Is_Ref (N) then
                              Set_Instance_On_Iir_Flist (S, S_Inst);
                           end if;
                        when Attr_Of_Ref
                          | Attr_Ref
                          | Attr_Forward_Ref =>
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
      It, It_Inst : List_Iterator;
   begin
      case N is
         when Null_Iir_List
           | Iir_List_All =>
            pragma Assert (Inst = N);
            return;
         when others =>
            It := List_Iterate (N);
            It_Inst := List_Iterate (Inst);
            while Is_Valid (It) loop
               pragma Assert (Is_Valid (It_Inst));
               El := Get_Element (It);
               El_Inst := Get_Element (It_Inst);

               Set_Instance_On_Iir (El, El_Inst);

               Next (It);
               Next (It_Inst);
            end loop;
            pragma Assert (not Is_Valid (It_Inst));
      end case;
   end Set_Instance_On_Iir_List;

   procedure Set_Instance_On_Iir_Flist (N : Iir_Flist; Inst : Iir_Flist)
   is
      El : Iir;
      El_Inst : Iir;
   begin
      case N is
         when Null_Iir_Flist
           | Iir_Flist_All
           | Iir_Flist_Others =>
            pragma Assert (Inst = N);
            return;
         when others =>
            pragma Assert (Get_Nbr_Elements (N) = Get_Nbr_Elements (Inst));
            for I in Flist_First .. Flist_Last (N) loop
               El := Get_Nth_Element (N, I);
               El_Inst := Get_Nth_Element (Inst, I);

               Set_Instance_On_Iir (El, El_Inst);
            end loop;
      end case;
   end Set_Instance_On_Iir_Flist;

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
   procedure Instantiate_Generic_Map (Assoc : Iir; Inter: Iir) is
   begin
      --  Replace formal reference to the instance.
      --  Cf Get_association_Interface
      declare
         Formal : Iir;
      begin
         Formal := Get_Formal (Assoc);
         if Is_Valid (Formal) then
            loop
               case Get_Kind (Formal) is
                  when Iir_Kind_Simple_Name
                    | Iir_Kind_Operator_Symbol =>
                     Set_Named_Entity
                       (Formal, Get_Instance (Get_Named_Entity (Formal)));
                     exit;
                  when Iir_Kind_Slice_Name
                    | Iir_Kind_Indexed_Name
                    | Iir_Kind_Selected_Element =>
                     Formal := Get_Prefix (Formal);
                  when others =>
                     Error_Kind ("instantiate_generic_map_chain", Formal);
               end case;
            end loop;
         end if;
      end;

      case Get_Kind (Assoc) is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open =>
            --  If the type of the formal is an interface type also
            --  associated by this map, change the type of the formal
            --  to the associated type.
            declare
               Assoc_Formal : constant Iir :=
                 Get_Association_Interface (Assoc, Inter);
               Formal_Type : Iir;
               Formal_Orig : Iir;
            begin
               if Assoc_Formal = Null_Iir then
                  return;
               end if;
               Formal_Type := Get_Type (Assoc_Formal);
               if Get_Kind (Formal_Type)
                 = Iir_Kind_Interface_Type_Definition
               then
                  --  Type of the formal is an interface type.
                  --  Check if the interface type was declared in the same
                  --  interface list: must have the same parent.
                  Formal_Orig := Get_Origin (Assoc_Formal);
                  if Get_Parent (Get_Type_Declarator (Formal_Type))
                    = Get_Parent (Formal_Orig)
                  then
                     Set_Type (Assoc_Formal, Get_Instance (Formal_Type));
                  end if;
               end if;
            end;
         when Iir_Kind_Association_Element_Package =>
            declare
               Sub_Inst : constant Iir :=
                 Get_Named_Entity (Get_Actual (Assoc));
               Sub_Pkg_Inter : constant Iir :=
                 Get_Association_Interface (Assoc, Inter);
               Sub_Pkg : constant Iir := Get_Origin (Sub_Pkg_Inter);
            begin
               --  Replace references of interface package to references
               --  to the actual package.
               Set_Instance (Sub_Pkg, Sub_Inst);
               Set_Instance_On_Chain (Get_Generic_Chain (Sub_Pkg),
                                      Get_Generic_Chain (Sub_Inst));
               Set_Instance_On_Chain (Get_Declaration_Chain (Sub_Pkg),
                                      Get_Declaration_Chain (Sub_Inst));
            end;
         when Iir_Kind_Association_Element_Type =>
            --  Replace the incomplete interface type by the actual subtype
            --  indication.
            declare
               Assoc_Inter : constant Iir :=
                 Get_Association_Interface (Assoc, Inter);
               Inter_Type_Def : constant Iir := Get_Type (Assoc_Inter);
               Actual_Type : constant Iir := Get_Actual_Type (Assoc);
            begin
               Set_Instance (Inter_Type_Def, Actual_Type);
            end;
         when Iir_Kind_Association_Element_Subprogram =>
            --  Replace the interface subprogram by the subprogram.
            declare
               Inter_Subprg : constant Iir :=
                 Get_Association_Interface (Assoc, Inter);
               Actual_Subprg : constant Iir :=
                 Get_Named_Entity (Get_Actual (Assoc));
            begin
               Set_Instance (Get_Origin (Inter_Subprg), Actual_Subprg);
            end;
         when others =>
            Error_Kind ("instantiate_generic_map", Assoc);
      end case;
   end Instantiate_Generic_Map;

   procedure Instantiate_Generic_Map_Chain (Inst : Iir; Pkg : Iir)
   is
      pragma Unreferenced (Pkg);
      Assoc : Iir;
      Inter : Iir;
   begin
      Assoc := Get_Generic_Map_Aspect_Chain (Inst);
      Inter := Get_Generic_Chain (Inst);
      while Is_Valid (Assoc) loop
         Instantiate_Generic_Map (Assoc, Inter);
         Next_Association_Interface (Assoc, Inter);
      end loop;
   end Instantiate_Generic_Map_Chain;

   function Copy_Tree (Orig : Iir) return Iir
   is
      Prev_Instance_File : constant Source_File_Entry := Instance_File;
      Mark : constant Instance_Index_Type := Prev_Instance_Table.Last;
      Res : Iir;
   begin
      Instance_File := No_Source_File_Entry;

      --  Be sure Get_Origin_Priv can be called on existing nodes.
      Expand_Origin_Table;

      Res := Instantiate_Iir (Orig, False);

      Instance_File := Prev_Instance_File;
      Restore_Origin (Mark);

      return Res;
   end Copy_Tree;

   procedure Instantiate_Subprogram_Declaration (Inst : Iir; Subprg : Iir)
   is
      Prev_Instance_File : constant Source_File_Entry := Instance_File;
      Mark : constant Instance_Index_Type := Prev_Instance_Table.Last;
   begin
      Create_Relocation (Inst, Subprg);
      Set_Instance_Source_File (Inst, Instance_File);

      --  Be sure Get_Origin_Priv can be called on existing nodes.
      Expand_Origin_Table;

      --  For Parent: the instance of PKG is INST.
      Set_Origin (Subprg, Inst);

      --  Manually instantiate the package declaration.
      Set_Generic_Chain
        (Inst, Instantiate_Generic_Chain (Inst, Get_Generic_Chain (Subprg)));
      Instantiate_Generic_Map_Chain (Inst, Subprg);
      if Get_Kind (Subprg) = Iir_Kind_Function_Instantiation_Declaration then
         Set_Return_Type (Inst, Instantiate_Iir (Subprg, True));
      end if;
      Set_Interface_Declaration_Chain
        (Inst,
         Instantiate_Iir_Chain (Get_Interface_Declaration_Chain (Subprg)));

      Set_Origin (Subprg, Null_Iir);

      Instance_File := Prev_Instance_File;
      Restore_Origin (Mark);
   end Instantiate_Subprogram_Declaration;

   procedure Instantiate_Package_Declaration (Inst : Iir; Pkg : Iir)
   is
      Header : constant Iir := Get_Package_Header (Pkg);
      Prev_Instance_File : constant Source_File_Entry := Instance_File;
      Mark : constant Instance_Index_Type := Prev_Instance_Table.Last;
      Prev_Within_Shared_Instance : constant Boolean :=
        Is_Within_Shared_Instance;
   begin
      Create_Relocation (Inst, Pkg);
      Set_Instance_Source_File (Inst, Instance_File);

      --  Be sure Get_Origin_Priv can be called on existing nodes.
      Expand_Origin_Table;

      --  For Parent: the instance of PKG is INST.
      Set_Origin (Pkg, Inst);

      Is_Within_Shared_Instance := not Get_Macro_Expanded_Flag (Pkg);

      --  Manually instantiate the package declaration.
      Set_Generic_Chain
        (Inst, Instantiate_Generic_Chain (Inst, Get_Generic_Chain (Header)));
      Instantiate_Generic_Map_Chain (Inst, Pkg);
      Set_Declaration_Chain
        (Inst, Instantiate_Iir_Chain (Get_Declaration_Chain (Pkg)));
      Set_Attribute_Value_Chain
        (Inst, Instantiate_Iir (Get_Attribute_Value_Chain (Pkg), True));
      Instantiate_Attribute_Value_Chain (Inst);

      Set_Origin (Pkg, Null_Iir);

      Instance_File := Prev_Instance_File;
      Restore_Origin (Mark);

      Is_Within_Shared_Instance := Prev_Within_Shared_Instance;
   end Instantiate_Package_Declaration;

   function Instantiate_Package_Body (Inst : Iir) return Iir
   is
      Pkg : constant Iir := Get_Uninstantiated_Package_Decl (Inst);
      Prev_Instance_File : constant Source_File_Entry := Instance_File;
      Mark : constant Instance_Index_Type := Prev_Instance_Table.Last;
      Res : Iir;
   begin
      Create_Relocation (Inst, Pkg);

      --  Be sure Get_Origin_Priv can be called on existing nodes.
      Expand_Origin_Table;

      --  References to package specification (and its declarations) will
      --  be redirected to the package instantiation.
      Set_Instance (Pkg, Inst);
      declare
         Pkg_Hdr : constant Iir := Get_Package_Header (Pkg);
         Pkg_El : Iir;
         Inst_El : Iir;
         Inter_El : Iir;
         Inter : Iir;
      begin
         --  In the body, references to interface object are redirected to the
         --  instantiated interface objects.
         Pkg_El := Get_Generic_Chain (Pkg_Hdr);
         Inst_El := Get_Generic_Chain (Inst);
         while Is_Valid (Pkg_El) loop
            if Get_Kind (Pkg_El) in Iir_Kinds_Interface_Object_Declaration then
               Set_Instance (Pkg_El, Inst_El);
            end if;
            Pkg_El := Get_Chain (Pkg_El);
            Inst_El := Get_Chain (Inst_El);
         end loop;

         --  In the body, references to interface type are substitued to the
         --  mapped type.
         Inst_El := Get_Generic_Map_Aspect_Chain (Inst);
         Inter_El := Get_Generic_Chain (Inst);
         while Is_Valid (Inst_El) loop
            case Get_Kind (Inst_El) is
               when Iir_Kind_Association_Element_Type =>
                  Inter := Get_Association_Interface (Inst_El, Inter_El);
                  Set_Instance (Get_Type (Get_Origin (Inter)),
                                Get_Actual_Type (Inst_El));
                  --  Implicit operators.
                  declare
                     Imp_Inter : Iir;
                     Imp_Assoc : Iir;
                  begin
                     Imp_Assoc := Get_Subprogram_Association_Chain (Inst_El);
                     Imp_Inter := Get_Interface_Type_Subprograms
                       (Get_Origin (Inter));
                     while Is_Valid (Imp_Inter) and Is_Valid (Imp_Assoc) loop
                        Set_Instance
                          (Imp_Inter,
                           Get_Named_Entity (Get_Actual (Imp_Assoc)));
                        Imp_Inter := Get_Chain (Imp_Inter);
                        Imp_Assoc := Get_Chain (Imp_Assoc);
                     end loop;
                  end;

               when Iir_Kind_Association_Element_Subprogram =>
                  Inter := Get_Association_Interface (Inst_El, Inter_El);
                  Set_Instance (Get_Origin (Inter),
                                Get_Named_Entity (Get_Actual (Inst_El)));

               when Iir_Kind_Association_Element_By_Expression
                 | Iir_Kind_Association_Element_By_Individual
                 | Iir_Kind_Association_Element_Open =>
                  null;
               when others =>
                  --  TODO.
                  raise Internal_Error;
            end case;
            Next_Association_Interface (Inst_El, Inter_El);
         end loop;
      end;
      Set_Instance_On_Chain
        (Get_Declaration_Chain (Pkg), Get_Declaration_Chain (Inst));

      --  Instantiate the body.
      Res := Instantiate_Iir (Get_Package_Body (Pkg), False);
      Set_Identifier (Res, Get_Identifier (Inst));

      --  Restore.
      Instance_File := Prev_Instance_File;
      Restore_Origin (Mark);

      return Res;
   end Instantiate_Package_Body;

   procedure Substitute_On_Iir_List (L : Iir_List; E : Iir; Rep : Iir);

   procedure Substitute_On_Iir (N : Iir; E : Iir; Rep : Iir) is
   begin
      if N = Null_Iir then
         return;
      end if;

      pragma Assert (N /= E);

      declare
         use Nodes_Meta;
         Kind : constant Iir_Kind := Get_Kind (N);
         Fields : constant Fields_Array := Get_Fields (Kind);
         F : Fields_Enum;
      begin
         for I in Fields'Range loop
            F := Fields (I);

            case Get_Field_Type (F) is
               when Type_Iir =>
                  declare
                     S : constant Iir := Get_Iir (N, F);
                  begin
                     if S = E then
                        --  Substitute
                        Set_Iir (N, F, Rep);
                        pragma Assert (Get_Field_Attribute (F) = Attr_Ref);
                     else
                        case Get_Field_Attribute (F) is
                           when Attr_None =>
                              Substitute_On_Iir (S, E, Rep);
                           when Attr_Ref
                             | Attr_Forward_Ref
                             | Attr_Maybe_Forward_Ref =>
                              null;
                           when Attr_Maybe_Ref =>
                              if not Get_Is_Ref (N) then
                                 Substitute_On_Iir (S, E, Rep);
                              end if;
                           when Attr_Chain =>
                              Substitute_On_Chain (S, E, Rep);
                           when Attr_Chain_Next =>
                              null;
                           when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                              --  Can only appear in list.
                              raise Internal_Error;
                        end case;
                     end if;
                  end;
               when Type_Iir_List =>
                  declare
                     S : constant Iir_List := Get_Iir_List (N, F);
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Substitute_On_Iir_List (S, E, Rep);
                        when Attr_Of_Maybe_Ref =>
                           if not Get_Is_Ref (N) then
                              Substitute_On_Iir_List (S, E, Rep);
                           end if;
                        when Attr_Of_Ref
                          | Attr_Ref
                          | Attr_Forward_Ref =>
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
   end Substitute_On_Iir;

   procedure Substitute_On_Iir_List (L : Iir_List; E : Iir; Rep : Iir)
   is
      It : List_Iterator;
   begin
      case L is
         when Null_Iir_List
           | Iir_List_All =>
            return;
         when others =>
            It := List_Iterate (L);
            while Is_Valid (It) loop
               Substitute_On_Iir (Get_Element (It), E, Rep);
               Next (It);
            end loop;
      end case;
   end Substitute_On_Iir_List;

   procedure Substitute_On_Chain (Chain : Iir; E : Iir; Rep : Iir)
   is
      El : Iir;
   begin
      El := Chain;
      while Is_Valid (El) loop
         Substitute_On_Iir (El, E, Rep);
         El := Get_Chain (El);
      end loop;
   end Substitute_On_Chain;

   function Get_Subprogram_Body_Origin (Spec : Iir) return Iir
   is
      Res : constant Iir := Get_Subprogram_Body (Spec);
      Orig : Iir;
   begin
      if Res /= Null_Iir then
         return Res;
      else
         Orig := Get_Origin (Spec);
         pragma Assert (Orig /= Null_Iir);
         return Get_Subprogram_Body_Origin (Orig);
      end if;
   end Get_Subprogram_Body_Origin;
end Vhdl.Sem_Inst;
