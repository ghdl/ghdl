--  Semantic analysis.
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Ada.Text_IO;
with GNAT.Table;
with Name_Table; -- use Name_Table;
with Errorout; use Errorout;
with Iirs_Utils;

package body Sem_Scopes is
   -- FIXME: names:
   -- scopes => regions ?

   --  Debugging subprograms.
   procedure Disp_All_Names;
   pragma Unreferenced (Disp_All_Names);

   procedure Disp_Scopes;
   pragma Unreferenced (Disp_Scopes);

   procedure Disp_Visible_Types;
   pragma Unreferenced (Disp_Visible_Types);

   procedure Disp_Detailed_Interpretations (Ident : Name_Id);
   pragma Unreferenced (Disp_Detailed_Interpretations);

   -- An interpretation cell is the element of the simply linked list
   -- of interpratation for an identifier.
   -- DECL is visible declaration;
   -- NEXT is the next element of the list.
   -- Interpretation cells are stored in a stack, Interpretations.
   type Interpretation_Cell is record
      Decl: Iir;
      Is_Potential : Boolean;
      Pad_0 : Boolean;
      Next: Name_Interpretation_Type;
   end record;
   pragma Pack (Interpretation_Cell);

   -- To manage the list of interpretation and to add informations to this
   -- list, a stack is used.
   -- Elements of stack can be of kind:
   -- Save_Cell:
   --   the element contains the interpretation INTER for the indentifier ID
   --   for the outer declarative region.
   --   A save cell is always each time a declaration is added to save the
   --   previous interpretation.
   -- Region_Start:
   --   A new declarative region start at interpretation INTER.  Here, INTER
   --   is used as an index in the interpretations stack (table).
   --   ID is used as an index into the unidim_array stack.
   -- Barrier_start, Barrier_end:
   --   All currents interpretations are saved between both INTER, and
   --   are cleared.  This is used to call semantic during another semantic.

   type Scope_Cell_Kind_Type is
     (Save_Cell, Hide_Cell, Region_Start, Barrier_Start, Barrier_End);

   type Scope_Cell is record
      Kind: Scope_Cell_Kind_Type;

      --  Usage of Inter:
      --  Save_Cell: previous value of name_table (id).info
      --  Hide_Cell: interpretation hidden.
      --  Region_Start: previous value of Current_Scope_Start.
      --  Barrier_Start: previous value of current_scope_start.
      --  Barrier_End: last index of interpretations table.
      Inter: Name_Interpretation_Type;

      --  Usage of Id:
      --  Save_Cell: ID whose interpretations are saved.
      --  Hide_Cell: not used.
      --  Region_Start: previous value of the last index of visible_types.
      --  Barrier_Start: previous value of CURRENT_BARRIER.
      --  Barrier_End: previous value of Current_composite_types_start.
      Id: Name_Id;
   end record;

   type Visible_Type_Cell is record
      Id: Name_Id;
      Decl: Iir;
   end record;

   package Visible_Types is new GNAT.Table
     (Table_Component_Type => Visible_Type_Cell,
      Table_Index_Type => Visible_Type_Index_Type,
      Table_Low_Bound => No_Visible_Type_Index + 1,
      Table_Initial => 32,
      Table_Increment => 10);

   package Interpretations is new GNAT.Table
     (Table_Component_Type => Interpretation_Cell,
      Table_Index_Type => Name_Interpretation_Type,
      Table_Low_Bound => First_Valid_Interpretation,
      Table_Initial => 128,
      Table_Increment => 50);

   package Scopes is new GNAT.Table
     (Table_Component_Type => Scope_Cell,
      Table_Index_Type => Natural,
      Table_Low_Bound => 0,
      Table_Initial => 128,
      Table_Increment => 50);

   -- Index into Interpretations marking the last interpretation of
   -- the previous (immediate) declarative region.
   Current_Scope_Start: Name_Interpretation_Type := No_Name_Interpretation;
   Current_Composite_Types_Start : Visible_Type_Index_Type :=
     No_Visible_Type_Index;

   function Valid_Interpretation (Inter : Name_Interpretation_Type)
                                 return Boolean is
   begin
      return Inter >= First_Valid_Interpretation;
   end Valid_Interpretation;

   -- Get and Set the info field of the table table for a
   -- name_interpretation.
   function Get_Interpretation (Id: Name_Id) return Name_Interpretation_Type is
   begin
      return Name_Interpretation_Type (Name_Table.Get_Info (Id));
   end Get_Interpretation;

   procedure Set_Interpretation (Id: Name_Id; Inter: Name_Interpretation_Type)
   is
   begin
      Name_Table.Set_Info (Id, Int32 (Inter));
   end Set_Interpretation;

   function Get_Under_Interpretation (Id : Name_Id)
     return Name_Interpretation_Type
   is
      Inter : Name_Interpretation_Type;
   begin
      Inter := Name_Interpretation_Type (Name_Table.Get_Info (Id));

      --  ID has no interpretation.
      --  So, there is no 'under' interpretation (FIXME: prove it).
      if not Valid_Interpretation (Inter) then
         return No_Name_Interpretation;
      end if;
      for I in reverse Scopes.First .. Scopes.Last loop
         declare
            S : Scope_Cell renames Scopes.Table (I);
         begin
            case S.Kind is
               when Save_Cell =>
                  if S.Id = Id then
                     --  This is the previous one, return it.
                     return S.Inter;
                  end if;
               when Region_Start
                 | Hide_Cell =>
                  null;
               when Barrier_Start
                 | Barrier_End =>
                  return No_Name_Interpretation;
            end case;
         end;
      end loop;
      return No_Name_Interpretation;
   end Get_Under_Interpretation;

   procedure Check_Interpretations;
   pragma Unreferenced (Check_Interpretations);

   procedure Check_Interpretations
   is
      Inter: Name_Interpretation_Type;
      Last : Name_Interpretation_Type;
      Err : Boolean;
   begin
      Last := Interpretations.Last;
      Err := False;
      for I in 0 .. Name_Table.Last_Name_Id loop
         Inter := Get_Interpretation (I);
         if Inter > Last then
            Ada.Text_IO.Put_Line
              ("bad interpretation for " & Name_Table.Image (I));
            Err := True;
         end if;
      end loop;
      if Err then
         raise Internal_Error;
      end if;
   end Check_Interpretations;

   -- Create a new declarative region.
   -- Simply push a region_start cell and update current_scope_start.
   procedure Open_Declarative_Region is
   begin
      Scopes.Increment_Last;
      Scopes.Table (Scopes.Last) := (Kind => Region_Start,
                                     Inter => Current_Scope_Start,
                                     Id => Name_Id (Visible_Types.Last));
      Current_Scope_Start := Interpretations.Last;
   end Open_Declarative_Region;

   -- Close a declarative region.
   -- Update interpretation of identifiers.
   procedure Close_Declarative_Region is
   begin
      loop
         case Scopes.Table (Scopes.Last).Kind is
            when Region_Start =>
               --  Discard interpretations cells added in this scopes.
               Interpretations.Set_Last (Current_Scope_Start);
               --  Restore Current_Scope_Start.
               Current_Scope_Start := Scopes.Table (Scopes.Last).Inter;
               Visible_Types.Set_Last
                 (Visible_Type_Index_Type (Scopes.Table (Scopes.Last).Id));
               Scopes.Decrement_Last;
               return;
            when Save_Cell =>
               --  Restore a previous interpretation.
               Set_Interpretation (Scopes.Table (Scopes.Last).Id,
                                   Scopes.Table (Scopes.Last).Inter);
            when Hide_Cell =>
               --  Unhide previous interpretation.
               declare
                  H, S : Name_Interpretation_Type;
               begin
                  H := Scopes.Table (Scopes.Last).Inter;
                  S := Interpretations.Table (H).Next;
                  Interpretations.Table (H).Next :=
                    Interpretations.Table (S).Next;
                  Interpretations.Table (S).Next := H;
               end;
            when Barrier_Start
              | Barrier_End =>
               --  Barrier cannot exist inside a declarative region.
               raise Internal_Error;
         end case;
         Scopes.Decrement_Last;
      end loop;
   end Close_Declarative_Region;

   procedure Open_Scope_Extension renames Open_Declarative_Region;
   procedure Close_Scope_Extension renames Close_Declarative_Region;

   function Get_Next_Interpretation (Ni: Name_Interpretation_Type)
                                     return Name_Interpretation_Type is
   begin
      if not Valid_Interpretation (Ni) then
         raise Internal_Error;
      end if;
      return Interpretations.Table (Ni).Next;
   end Get_Next_Interpretation;

   function Get_Declaration (Ni: Name_Interpretation_Type)
                             return Iir is
   begin
      if not Valid_Interpretation (Ni) then
         raise Internal_Error;
      end if;
      return Interpretations.Table (Ni).Decl;
   end Get_Declaration;

   function Get_Non_Alias_Declaration (Ni: Name_Interpretation_Type)
                                      return Iir
   is
      Res : Iir;
   begin
      Res := Get_Declaration (Ni);
      if Get_Kind (Res) = Iir_Kind_Non_Object_Alias_Declaration then
         Res := Get_Name (Res);
      end if;
      return Res;
   end Get_Non_Alias_Declaration;

   -- Pointer just past the last barrier_end in the scopes stack.
   Current_Barrier : Integer := 0;

   procedure Push_Interpretations is
   begin
      -- Add a barrier_start.
      -- Save current_scope_start and current_barrier.
      Scopes.Increment_Last;
      Scopes.Table (Scopes.Last) := (Kind => Barrier_Start,
                                     Inter => Current_Scope_Start,
                                     Id => Name_Id (Current_Barrier));

      -- Save all the current name interpretations.
      --  (For each name that have interpretations, there is a save_cell
      --   containing the interpretations for the outer scope).
      -- FIXME: maybe we should only save the name_table info.
      for I in Current_Barrier .. Scopes.Last - 1 loop
         if Scopes.Table (I).Kind = Save_Cell then
            Scopes.Increment_Last;
            Scopes.Table (Scopes.Last) :=
              (Kind => Save_Cell,
               Inter => Get_Interpretation (Scopes.Table (I).Id),
               Id => Scopes.Table (I).Id);
            Set_Interpretation (Scopes.Table (I).Id, No_Name_Interpretation);
         end if;
      end loop;

      -- Add a barrier_end.
      -- Save interpretations.last.
      Scopes.Increment_Last;
      Scopes.Table (Scopes.Last) :=
        (Kind => Barrier_End,
         Inter => Interpretations.Last,
         Id => Name_Id (Current_Composite_Types_Start));

      -- Start a completly new scope.
      Current_Scope_Start := Interpretations.Last + 1;

      -- Keep the last barrier.
      Current_Barrier := Scopes.Last + 1;
      Current_Composite_Types_Start := Visible_Types.Last;

      pragma Debug (Name_Table.Assert_No_Infos);
   end Push_Interpretations;

   procedure Pop_Interpretations is
   begin
      -- clear all name interpretations set by the current barrier.
      for I in Current_Barrier .. Scopes.Last loop
         if Scopes.Table (I).Kind = Save_Cell then
            Set_Interpretation (Scopes.Table (I).Id, No_Name_Interpretation);
         end if;
      end loop;
      Scopes.Set_Last (Current_Barrier - 1);
      if Scopes.Table (Scopes.Last).Kind /= Barrier_End then
         raise Internal_Error;
      end if;

      pragma Debug (Name_Table.Assert_No_Infos);

      -- Restore the stack pointer of interpretations.
      Interpretations.Set_Last (Scopes.Table (Scopes.Last).Inter);
      Current_Composite_Types_Start :=
        Visible_Type_Index_Type (Scopes.Table (Scopes.Last).Id);
      Scopes.Decrement_Last;

      -- Restore all name interpretations.
      while Scopes.Table (Scopes.Last).Kind /= Barrier_Start loop
         Set_Interpretation (Scopes.Table (Scopes.Last).Id,
                             Scopes.Table (Scopes.Last).Inter);
         Scopes.Decrement_Last;
      end loop;

      -- Restore current_scope_start and current_barrier.
      Current_Scope_Start := Scopes.Table (Scopes.Last).Inter;
      Current_Barrier := Natural (Scopes.Table (Scopes.Last).Id);

      Scopes.Decrement_Last;
   end Pop_Interpretations;

   -- Return TRUE if INTER was made directly visible via a use clause.
   function Is_Potentially_Visible (Inter: Name_Interpretation_Type)
     return Boolean
   is
   begin
      return Interpretations.Table (Inter).Is_Potential;
   end Is_Potentially_Visible;

   -- Return TRUE iif DECL can be overloaded.
   function Is_Overloadable (Decl: Iir) return Boolean is
   begin
      -- LRM93 §10.3:
      -- The overloaded declarations considered in this chapter are those for
      -- subprograms and enumeration literals.
      case Get_Kind (Decl) is
         when Iir_Kind_Enumeration_Literal
           | Iir_Kinds_Function_Declaration
           | Iir_Kinds_Procedure_Declaration =>
            return True;
         when Iir_Kind_Non_Object_Alias_Declaration =>
            case Get_Kind (Get_Name (Decl)) is
               when Iir_Kind_Enumeration_Literal
                 | Iir_Kinds_Function_Declaration
                 | Iir_Kinds_Procedure_Declaration =>
                  return True;
               when Iir_Kind_Non_Object_Alias_Declaration =>
                  raise Internal_Error;
               when others =>
                  return False;
            end case;
         when others =>
            return False;
      end case;
   end Is_Overloadable;

   -- Return true if DECL declare a type that is visible.
   --  This is used to build the list of visible types, ie types that must
   --  be considered for certains expression: access for NULL literals,
   --  arrays and records for aggregates, arrays for string literals.
--    function Is_Visible_Type (Decl: Iir) return Boolean
--    is
--       Def: Iir;
--    begin
--       case Get_Kind (Decl) is
--          when Iir_Kind_Array_Type_Definition
--            | Iir_Kind_Array_Subtype_Definition =>
--             raise Internal_Error;
--          when Iir_Kind_Type_Declaration =>
--             Def := Get_Type (Decl);
--          when others =>
--             return False;
--       end case;
--       case Get_Kind (Def) is
--          when Iir_Kind_Array_Type_Definition
--            | Iir_Kind_Array_Subtype_Definition =>
--             return True;
--          when Iir_Kind_Record_Type_Definition =>
--             return True;
--          when Iir_Kind_Access_Type_Definition
--            | Iir_Kind_Access_Subtype_Definition =>
--             return True;
--          when others =>
--             return False;
--       end case;
--    end Is_Visible_Type;

   function Get_Visible_Type (Vt: Visible_Type_Index_Type)
     return Visible_Type_Index_Type
   is
      Pt: Visible_Type_Index_Type := Vt;
   begin
      if True then
         return Pt;
      else
         while Pt > Current_Composite_Types_Start loop
            if Get_Declaration
              (Get_Interpretation (Visible_Types.Table (Pt).Id))
              = Visible_Types.Table (Pt).Decl
            then
               return Pt;
            end if;
            Pt := Pt - 1;
         end loop;
         return No_Visible_Type_Index;
      end if;
   end Get_Visible_Type;

   -- Get the first visible declaration of unidim array.
   function Get_First_Visible_Type return Visible_Type_Index_Type is
   begin
      return Get_Visible_Type (Visible_Types.Last);
   end Get_First_Visible_Type;

   -- Get the next visible declaration of unidim array in the list.
   function Get_Next_Visible_Type (Index: Visible_Type_Index_Type)
     return Visible_Type_Index_Type is
   begin
      return Get_Visible_Type (Index - 1);
   end Get_Next_Visible_Type;

   -- Get the declaration corresponding to an uni_array_visible_type.
   function Get_Visible_Type_Decl (Index : Visible_Type_Index_Type)
     return Iir is
   begin
      return Visible_Types.Table (Index).Decl;
   end Get_Visible_Type_Decl;

   -- Return TRUE if INTER was made direclty visible in the current
   -- declarative region.
   function Is_In_Current_Declarative_Region (Inter: Name_Interpretation_Type)
                                             return Boolean is
   begin
      return Inter > Current_Scope_Start;
   end Is_In_Current_Declarative_Region;

   --  Called when CURR is being declared in the same declarative region as
   --  PREV, using the same identifier.
   --  The function assumes CURR and PREV are both overloadable.
   --  Return TRUE if this redeclaration is allowed.
--    function Redeclaration_Allowed (Prev, Curr : Iir) return Boolean is
--    begin
--       case Get_Kind (Curr) is
--          when Iir_Kinds_Function_Specification
--            | Iir_Kinds_Procedure_Specification =>
--             if ((Get_Kind (Prev) in Iir_Kinds_User_Function_Specification
--               and then
--               Get_Kind (Curr) in Iir_Kinds_User_Function_Specification)
--               or else
--               (Get_Kind (Prev) in Iir_Kinds_User_Procedure_Specification
--                and then
--               Get_Kind (Curr) in Iir_Kinds_User_Procedure_Specification))
--             then
--                return not Iirs_Utils.Is_Same_Profile (Prev, Curr);
--             else
--                return True;
--             end if;
--          when Iir_Kind_Enumeration_Literal =>
--             if Get_Kind (Prev) /= Get_Kind (Curr) then
--                --  FIXME: PREV may be a function returning the type of the
--                --  literal.
--                return True;
--             end if;
--             return Get_Type (Prev) /= Get_Type (Curr);
--          when others =>
--             return False;
--       end case;
--    end Redeclaration_Allowed;

   procedure Add_Visible_Type (Decl : Iir) is
   begin
      Visible_Types.Append ((Id => Get_Identifier (Decl), Decl => Decl));
   end Add_Visible_Type;

   -- Add interpretation DECL to the identifier of DECL.
   -- POTENTIALLY is true if the identifier comes from a use clause.
   procedure Add_Name (Decl: Iir; Ident: Name_Id; Potentially: Boolean)
   is
      -- Current interpretation of ID.  This is the one before DECL is
      -- added (if so).
      Current_Inter: Name_Interpretation_Type;
      Current_Decl : Iir;

      procedure Save_Current_Interpretation is
      begin
         Scopes.Increment_Last;
         Scopes.Table (Scopes.Last) :=
           (Kind => Save_Cell, Id => Ident, Inter => Current_Inter);
      end Save_Current_Interpretation;

      procedure Add_New_Interpretation is
      begin
         Interpretations.Increment_Last;
         Interpretations.Table (Interpretations.Last) :=
           (Decl => Decl, Next => Current_Inter,
            Is_Potential => Potentially, Pad_0 => False);
         Set_Interpretation (Ident, Interpretations.Last);
      end Add_New_Interpretation;
   begin
      Current_Inter := Get_Interpretation (Ident);

      if Current_Inter = No_Name_Interpretation
        or else (Current_Inter = Conflict_Interpretation and not Potentially)
      then
         -- Very simple: no hide, no overloading.
         Save_Current_Interpretation;
         Add_New_Interpretation;
         return;
      end if;

      if Potentially then
         if Current_Inter = Conflict_Interpretation then
            --  Yet another conflicting interpretation.
            return;
         end if;
         -- Do not re-add a potential decl
         declare
            Inter: Name_Interpretation_Type := Current_Inter;
         begin
            while Valid_Interpretation (Inter) loop
               if Get_Declaration (Inter) = Decl then
                  return;
               end if;
               Inter := Get_Next_Interpretation (Inter);
            end loop;
         end;
      end if;

      --  LRM §10.3:
      --  Each of two declarations is said to be a homograph of the other if
      --  both declarations have the same identifier, operator symbol, or
      --  character literal, and overloading is allowed for at most one
      --  of the two.
      --
      --  GHDL: the condition 'overloading is allowed for at most one of the
      --  two' is false iff overloading is allowed for both; this is a nand.

      --  Note: at this stage, current_inter is valid.
      Current_Decl := Get_Declaration (Current_Inter);

      if Is_Overloadable (Current_Decl) and then Is_Overloadable (Decl) then
         --  Current_Inter and Decl overloads.

         --  LRM 10.3
         --  If overloading is allowed for both declarations, then each of the
         --  two is a homograph of the other if they have the same identifier,
         --  operator symbol or character literal, as well as the same
         --  parameter and result profile.

         declare
            Homograph : Name_Interpretation_Type;
            Prev_Homograph : Name_Interpretation_Type;

            procedure Maybe_Save_And_Add_New_Interpretation is
            begin
               if not Is_In_Current_Declarative_Region (Current_Inter) then
                  Save_Current_Interpretation;
               end if;
               Add_New_Interpretation;
            end Maybe_Save_And_Add_New_Interpretation;

            procedure Hide_Homograph
            is
               S : Name_Interpretation_Type;
            begin
               if Prev_Homograph = No_Name_Interpretation then
                  Prev_Homograph := Interpretations.Last;
               end if;
               if Interpretations.Table (Prev_Homograph).Next /= Homograph
               then
                  --  PREV_HOMOGRAPH must be the interpretation just before
                  --  HOMOGRAPH.
                  raise Internal_Error;
               end if;

               --  Hide previous interpretation.
               S := Interpretations.Table (Homograph).Next;
               Interpretations.Table (Homograph).Next := Prev_Homograph;
               Interpretations.Table (Prev_Homograph).Next := S;
               Scopes.Increment_Last;
               Scopes.Table (Scopes.Last) :=
                 (Kind => Hide_Cell,
                  Id => Null_Identifier, Inter => Homograph);
            end Hide_Homograph;

            function Get_Hash_Non_Alias (D : Iir) return Iir_Int32 is
            begin
               if Get_Kind (D) = Iir_Kind_Non_Object_Alias_Declaration then
                  return Get_Subprogram_Hash (Get_Name (D));
               else
                  return Get_Subprogram_Hash (D);
               end if;
            end Get_Hash_Non_Alias;

            Decl_Hash : Iir_Int32;
            Hash : Iir_Int32;
         begin
            Decl_Hash := Get_Hash_Non_Alias (Decl);
            if Decl_Hash = 0 then
               raise Internal_Error;
            end if;

            --  Find an homograph of this declaration.
            Homograph := Current_Inter;
            Prev_Homograph := No_Name_Interpretation;
            while Homograph /= No_Name_Interpretation loop
               Current_Decl := Get_Declaration (Homograph);
               Hash := Get_Hash_Non_Alias (Current_Decl);
               exit when Decl_Hash = Hash
                 and then Iirs_Utils.Is_Same_Profile (Decl, Current_Decl);
               Prev_Homograph := Homograph;
               Homograph := Get_Next_Interpretation (Homograph);
            end loop;

            if Homograph = No_Name_Interpretation then
               --  Simple case: no homograph.
               Maybe_Save_And_Add_New_Interpretation;
            else
               --  There is an homograph.
               if Potentially then
                  --  LRM 10.4 Use Clauses
                  --  1. A potentially visible declaration is not made
                  --  directly visible if the place considered is within the
                  --  immediate scope of a homograph of the declaration.
                  if Is_In_Current_Declarative_Region (Homograph) then
                     if not Is_Potentially_Visible (Homograph) then
                        return;
                     end if;

                     --  GHDL: if the homograph is in the same declarative
                     --  region than DECL, it must be an implicit declaration
                     --  to be hidden.
                     --  FIXME: this rule is not in the LRM.
                     if Get_Parent (Decl) = Get_Parent (Current_Decl) then
                        --  Note: no need to save previous interpretation!
                        Add_New_Interpretation;
                        Hide_Homograph;
                        return;
                     end if;

                     --  The homograph is potentially visible and was declared
                     --  in a scope different from the DECL scope.
                     --  (ie, it was certainly made visible by another use
                     --   clause).
                     Add_New_Interpretation;
                     return;
                  else
                     --  The homograph was made visible in an outer declarative
                     --  region.  Therefore, it must not be hidden.
                     Maybe_Save_And_Add_New_Interpretation;
                  end if;
               else
                  if not Is_Potentially_Visible (Homograph) then
                     if Is_In_Current_Declarative_Region (Homograph) then
                        if Get_Kind (Current_Decl)
                          /= Iir_Kind_Implicit_Function_Declaration
                          and then
                          Get_Kind (Current_Decl)
                          /= Iir_Kind_Implicit_Procedure_Declaration
                        then
                           Error_Msg_Sem
                             ("redeclaration of " & Disp_Node (Current_Decl)
                              & " defined at " & Disp_Location (Current_Decl),
                              Decl);
                           return;
                        end if;
                     else
                        --  Overload.
                        null;
                     end if;
                  else
                     --  LRM 10.4 Use Clauses
                     --  1. A potentially visible declaration is not made
                     --  directly visible if the place considered is within the
                     --  immediate scope of a homograph of the declaration.
                     null;
                  end if;
                  Maybe_Save_And_Add_New_Interpretation;

                  Hide_Homograph;
                  return;
               end if;
            end if;
         end;
         return;
      end if;

      -- The current interpretation and the new one are homograph.
      if Is_In_Current_Declarative_Region (Current_Inter) then
         -- They are perhaps visible in the same declarative region.
         if Is_Potentially_Visible (Current_Inter) then
            if Potentially then
               -- LRM93 §10.4, item #2
               -- Potentially visible declarations that have the same
               -- designator are not made directly visible unless each of
               -- them is either an enumeration literal specification or
               -- the declaration of a subprogram.
               if Decl = Get_Declaration (Current_Inter) then
                  -- The rule applies only for distinct declaration.
                  -- This handles 'use p.all; use P.all;'.
                  -- FIXME: this should have been handled at the start of
                  -- this subprogram.
                  raise Internal_Error;
                  return;
               end if;
               Save_Current_Interpretation;
               Set_Interpretation (Ident, Conflict_Interpretation);
               return;
            else
               -- LRM93 §10.4 item #1
               --  A potentially visible declaration is not made directly
               --  visible if the place considered is within the immediate
               --  scope of a homograph of the declaration.
               -- GHDL: Discard the current potentially visible declaration,
               --  only if it is not an entity declaration, since it is used
               --  to find default binding.
               if Get_Kind (Current_Decl) = Iir_Kind_Design_Unit
                 and then Get_Kind (Get_Library_Unit (Current_Decl))
                 = Iir_Kind_Entity_Declaration
               then
                  Save_Current_Interpretation;
               end if;
               Current_Inter := No_Name_Interpretation;
               Add_New_Interpretation;
               return;
            end if;
         else
            --  There is already a declaration in the current scope.
            if Potentially then
               -- LRM93 §10.4 item #1
               -- Discard the new and potentially visible declaration.
               -- However, add the type.
               -- FIXME: Add_In_Visible_List (Ident, Decl);
               return;
            else
               --  LRM93 11.2
               --  If two or more logical names having the same
               --  identifier appear in library clauses in the same
               --  context, the second and subsequent occurences of the
               --  logical name have no effect.  The same is true of
               --  logical names appearing both in the context clause
               --  of a primary unit and in the context clause of a
               --  corresponding secondary unit.
               --  GHDL: we apply this rule with VHDL-87, because of implicits
               --  library clauses STD and WORK.
               if Get_Kind (Decl) = Iir_Kind_Library_Declaration
                 and then
                 Get_Kind (Current_Decl) = Iir_Kind_Library_Declaration
               then
                  return;
               end if;

               -- None of the two declarations are potentially visible, ie
               -- both are visible.
               -- LRM §10.3:
               --  Two declarations that occur immediately within the same
               --  declarative region must not be homographs,
               -- FIXME: unless one of them is the implicit declaration of a
               --  predefined operation.
               Error_Msg_Sem ("identifier '" & Name_Table.Image (Ident)
                              & "' already used for a declaration",
                              Decl);
               Error_Msg_Sem
                 ("previous declaration: " & Disp_Node (Current_Decl),
                  Current_Decl);
               return;
            end if;
         end if;
      end if;

      -- Homograph, not in the same scope.
      -- LRM §10.3:
      -- A declaration is said to be hidden within (part of) an inner
      -- declarative region if the inner region contains an homograph
      -- of this declaration; the outer declaration is the hidden
      -- within the immediate scope of the inner homograph.
      Save_Current_Interpretation;
      Current_Inter := No_Name_Interpretation;  -- Hid.
      Add_New_Interpretation;
   end Add_Name;

   procedure Add_Name (Decl: Iir) is
   begin
      Add_Name (Decl, Get_Identifier (Decl), False);
   end Add_Name;

   procedure Replace_Name (Id: Name_Id; Old : Iir; Decl: Iir)
   is
      Inter : Name_Interpretation_Type;
   begin
      Inter := Get_Interpretation (Id);
      if Get_Declaration (Inter) /= Old then
         raise Internal_Error;
      end if;
      if Get_Next_Interpretation (Inter) /= No_Name_Interpretation then
         raise Internal_Error;
      end if;
      Interpretations.Table (Inter).Decl := Decl;
   end Replace_Name;

   procedure Name_Visible (Ident : Name_Id; Decl : Iir)
   is
      pragma Unreferenced (Ident);
   begin
      if Get_Visible_Flag (Decl) then
         --  A name can be made visible only once.
         raise Internal_Error;
      end if;
      Set_Visible_Flag (Decl, True);
   end Name_Visible;

   procedure Name_Visible (Decl : Iir) is
   begin
      Name_Visible (Get_Identifier (Decl), Decl);
   end Name_Visible;

   procedure Iterator_Decl (Decl : Iir; Arg : Arg_Type)
   is
   begin
      case Get_Kind (Decl) is
         when Iir_Kinds_Procedure_Declaration
           | Iir_Kinds_Function_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement =>
            Handle_Decl (Decl, Arg);
         when Iir_Kind_Type_Declaration =>
            declare
               Def : Iir;
               List : Iir_List;
               El : Iir;
            begin
               Def := Get_Type (Decl);

               -- Handle incomplete type declaration.
               if Get_Kind (Def) = Iir_Kind_Incomplete_Type_Definition then
                  return;
               end if;

               Handle_Decl (Decl, Arg);

               if Get_Kind (Def) = Iir_Kind_Enumeration_Type_Definition then
                  List := Get_Enumeration_Literal_List (Def);
                  for I in Natural loop
                     El := Get_Nth_Element (List, I);
                     exit when El = Null_Iir;
                     Handle_Decl (El, Arg);
                  end loop;
               end if;
            end;
         when Iir_Kind_Anonymous_Type_Declaration =>
            Handle_Decl (Decl, Arg);

            declare
               Def : Iir;
               El : Iir;
            begin
               Def := Get_Type (Decl);

               if Get_Kind (Def) = Iir_Kind_Physical_Type_Definition then
                  El := Get_Unit_Chain (Def);
                  while El /= Null_Iir loop
                     Handle_Decl (El, Arg);
                     El := Get_Chain (El);
                  end loop;
               end if;
            end;
         when Iir_Kind_Use_Clause =>
            Handle_Decl (Decl, Arg);
         when Iir_Kind_Library_Clause =>
            Handle_Decl (Decl, Arg);
--             El := Get_Library_Declaration (Decl);
--             if El /= Null_Iir then
--                --  May be empty.
--                Handle_Decl (El, Arg);
--             end if;
         when Iir_Kind_Design_Unit =>
            Handle_Decl (Decl, Arg);

         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            null;

         when Iir_Kind_Attribute_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Disconnection_Specification =>
            null;
         when Iir_Kinds_Signal_Attribute =>
            null;

         when others =>
            Error_Kind ("iterator_decl", Decl);
      end case;
   end Iterator_Decl;

   --  Make POTENTIALLY (or not) visible DECL.
   procedure Add_Name_Decl (Decl : Iir; Potentially : Boolean) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Use_Clause =>
            if not Potentially then
               Add_Use_Clause (Decl);
            end if;
         when Iir_Kind_Library_Clause =>
            Add_Name (Get_Library_Declaration (Decl),
                      Get_Identifier (Decl), Potentially);
         when Iir_Kind_Type_Declaration =>
            Add_Name (Decl, Get_Identifier (Decl), Potentially);
            Add_Visible_Type (Decl);
         when Iir_Kind_Anonymous_Type_Declaration =>
            Add_Visible_Type (Decl);
         when others =>
            Add_Name (Decl, Get_Identifier (Decl), Potentially);
      end case;
   end Add_Name_Decl;

   procedure Add_Declaration is
      new Iterator_Decl (Arg_Type => Boolean, Handle_Decl => Add_Name_Decl);

   procedure Iterator_Decl_List (Decl_List : Iir_List; Arg : Arg_Type)
   is
      Decl: Iir;
   begin
      if Decl_List = Null_Iir_List then
         return;
      end if;
      for I in Natural loop
         Decl := Get_Nth_Element (Decl_List, I);
         exit when Decl = Null_Iir;
         Handle_Decl (Decl, Arg);
      end loop;
   end Iterator_Decl_List;

   procedure Iterator_Decl_Chain (Chain_First : Iir; Arg : Arg_Type)
   is
      Decl: Iir;
   begin
      Decl := Chain_First;
      while Decl /= Null_Iir loop
         Handle_Decl (Decl, Arg);
         Decl := Get_Chain (Decl);
      end loop;
   end Iterator_Decl_Chain;

   procedure Add_Declarations is new Iterator_Decl_Chain
     (Arg_Type => Boolean, Handle_Decl => Add_Declaration);

   procedure Add_Declarations_List is new Iterator_Decl_List
     (Arg_Type => Boolean, Handle_Decl => Add_Declaration);

   procedure Use_Library_All (Library : Iir_Library_Declaration)
   is
      Design_File : Iir_Design_File;
      Design_Unit : Iir_Design_Unit;
      Library_Unit : Iir;
   begin
      Design_File := Get_Design_File_Chain (Library);
      while Design_File /= Null_Iir loop
         Design_Unit := Get_First_Design_Unit (Design_File);
         while Design_Unit /= Null_Iir loop
            Library_Unit := Get_Library_Unit (Design_Unit);
            if Get_Kind (Library_Unit) /= Iir_Kind_Package_Body then
               Add_Name (Design_Unit, Get_Identifier (Design_Unit), True);
            end if;
            Design_Unit := Get_Chain (Design_Unit);
         end loop;
         Design_File := Get_Chain (Design_File);
      end loop;
   end Use_Library_All;

   procedure Use_Selected_Name (Name : Iir) is
   begin
      if Get_Kind (Name) = Iir_Kind_Overload_List then
         Add_Declarations_List (Get_Overload_List (Name), True);
      else
         Add_Declaration (Name, True);
      end if;
   end Use_Selected_Name;

   procedure Use_All_Names (Name: Iir) is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Library_Declaration =>
            Use_Library_All (Name);
         when Iir_Kind_Design_Unit =>
            --  The design unit is a package.
            Add_Declarations
              (Get_Declaration_Chain (Get_Library_Unit (Name)), True);
         when others =>
            raise Internal_Error;
      end case;
   end Use_All_Names;

   procedure Add_Use_Clause (Clause : Iir_Use_Clause)
   is
      Name : Iir;
      Cl : Iir_Use_Clause;
   begin
      Cl := Clause;
      loop
         Name := Get_Selected_Name (Cl);
         if Get_Kind (Name) = Iir_Kind_Selected_By_All_Name then
            Use_All_Names (Get_Named_Entity (Get_Prefix (Name)));
         else
            Use_Selected_Name (Get_Named_Entity (Name));
         end if;
         Cl := Get_Use_Clause_Chain (Cl);
         exit when Cl = Null_Iir;
      end loop;
   end Add_Use_Clause;

   procedure Add_Declarations_From_Interface_Chain (Chain : Iir)
   is
      El: Iir;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Add_Name (El, Get_Identifier (El), False);
         El := Get_Chain (El);
      end loop;
   end Add_Declarations_From_Interface_Chain;

   procedure Add_Declarations_Of_Concurrent_Statement (Parent : Iir)
   is
      El: Iir;
      Label: Name_Id;
   begin
      El := Get_Concurrent_Statement_Chain (Parent);
      while El /= Null_Iir loop
         Label := Get_Label (El);
         if Label /= Null_Identifier then
            Add_Name (El, Get_Identifier (El), False);
         end if;
         El := Get_Chain (El);
      end loop;
   end Add_Declarations_Of_Concurrent_Statement;

   procedure Add_Context_Clauses (Unit : Iir_Design_Unit) is
   begin
      Add_Declarations (Get_Context_Items (Unit), False);
   end Add_Context_Clauses;

   -- Add declarations from an entity into the current declarative region.
   -- This is needed when an architecture is analysed.
   procedure Add_Entity_Declarations (Entity : Iir_Entity_Declaration)
   is
   begin
      Add_Declarations_From_Interface_Chain (Get_Generic_Chain (Entity));
      Add_Declarations_From_Interface_Chain (Get_Port_Chain (Entity));
      Add_Declarations (Get_Declaration_Chain (Entity), False);
      Add_Declarations_Of_Concurrent_Statement (Entity);
   end Add_Entity_Declarations;

   -- Add declarations from a package into the current declarative region.
   -- This is needed when a package body is analysed.
   procedure Add_Package_Declarations (Decl: Iir_Package_Declaration) is
   begin
      Add_Declarations (Get_Declaration_Chain (Decl), False);
   end Add_Package_Declarations;

   procedure Add_Component_Declarations (Component: Iir_Component_Declaration)
   is
   begin
      Add_Declarations_From_Interface_Chain (Get_Generic_Chain (Component));
      Add_Declarations_From_Interface_Chain (Get_Port_Chain (Component));
   end Add_Component_Declarations;

   procedure Add_Protected_Type_Declarations
     (Decl : Iir_Protected_Type_Declaration) is
   begin
      Add_Declarations (Get_Declaration_Chain (Decl), False);
   end Add_Protected_Type_Declarations;

   procedure Extend_Scope_Of_Block_Declarations (Decl : Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Architecture_Declaration =>
            Add_Context_Clauses (Get_Design_Unit (Decl));
         when Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement =>
            --  FIXME: formal, iterator ?
            null;
         when others =>
            Error_Kind ("extend_scope_of_block_declarations", Decl);
      end case;
      Add_Declarations (Get_Declaration_Chain (Decl), False);
      Add_Declarations_Of_Concurrent_Statement (Decl);
   end Extend_Scope_Of_Block_Declarations;

   -- Debugging
   procedure Disp_Visible_Types
   is
      use Ada.Text_IO;
      Index: Visible_Type_Index_Type;
   begin
      Index := Get_First_Visible_Type;
      while Index /= No_Visible_Type_Index loop
         Put_Line (Disp_Node (Get_Visible_Type_Decl (Index)));
         Index := Get_Next_Visible_Type (Index);
      end loop;
   end Disp_Visible_Types;

   procedure Disp_Detailed_Interpretations (Ident : Name_Id)
   is
      use Ada.Text_IO;
      use Name_Table;
      procedure Disp_Type (Str : String; Node : Iir) is
      begin
         Put (Str);
         Put_Line
           (Image (Get_Identifier (Get_Type_Declarator (Node))));
      end Disp_Type;

      Inter: Name_Interpretation_Type;
      Decl : Iir;
   begin
      Put (Name_Table.Image (Ident));
      Put_Line (":");

      Inter := Get_Interpretation (Ident);
      while Valid_Interpretation (Inter) loop
         Decl := Get_Declaration (Inter);
         Put (' ');
         Put (Iir_Kind'Image (Get_Kind (Decl)));
         Put_Line (", loc: " & Get_Location_Str (Get_Location (Decl)));
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Implicit_Function_Declaration =>
               Disp_Type ("  return type: ", Get_Return_Type (Decl));
               null;
            when others =>
               null;
         end case;
         Inter := Get_Next_Interpretation (Inter);
      end loop;
   end Disp_Detailed_Interpretations;

   procedure Disp_All_Interpretations
     (Interpretation: Name_Interpretation_Type)
   is
      use Ada.Text_IO;
      Inter: Name_Interpretation_Type;
   begin
      Inter := Interpretation;
      while Valid_Interpretation (Inter) loop
         Put (Name_Interpretation_Type'Image (Inter));
         Put ('.');
         Put (Iir_Kind'Image (Get_Kind (Get_Declaration (Inter))));
         Inter := Get_Next_Interpretation (Inter);
      end loop;
      New_Line;
   end Disp_All_Interpretations;

   procedure Disp_All_Names
   is
      use Ada.Text_IO;
      Inter: Name_Interpretation_Type;
   begin
      for I in 0 .. Name_Table.Last_Name_Id loop
         Inter := Get_Interpretation (I);
         if Valid_Interpretation (Inter) then
            Put (Name_Table.Image (I));
            Put (Name_Id'Image (I));
            Put (':');
            Disp_All_Interpretations (Inter);
         end if;
      end loop;
      Put_Line ("interprations.last = "
                & Name_Interpretation_Type'Image (Interpretations.Last));
      Put_Line ("current_scope_start ="
                & Name_Interpretation_Type'Image (Current_Scope_Start));
   end Disp_All_Names;

   procedure Disp_Scopes
   is
      use Ada.Text_IO;
   begin
      for I in reverse Scopes.First .. Scopes.Last loop
         declare
            S : Scope_Cell renames Scopes.Table (I);
         begin
            case S.Kind is
               when Save_Cell =>
                  Put ("save_cell: '");
                  Put (Name_Table.Image (S.Id));
                  Put ("', old inter:");
               when Hide_Cell =>
                  Put ("hide_cell: to be inserted after ");
               when Region_Start =>
                  Put ("region_start at");
               when Barrier_Start =>
                  Put ("barrier_start at");
               when Barrier_End =>
                  Put ("barrier_end at");
            end case;
            Put_Line (Name_Interpretation_Type'Image (S.Inter));
         end;
      end loop;
   end Disp_Scopes;
end Sem_Scopes;
