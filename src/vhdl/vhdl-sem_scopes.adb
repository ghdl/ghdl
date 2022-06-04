--  Semantic analysis.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Logging; use Logging;
with Tables;
with Flags; use Flags;
with Name_Table; -- use Name_Table;
with Files_Map; use Files_Map;
with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;

package body Vhdl.Sem_Scopes is
   --  An interpretation cell is the element of the simply linked list
   --  of interpretation for an identifier.
   --  Interpretation cells are stored in table Interpretations.
   type Interpretation_Cell is record
      --  The declaration for this interpretation.
      Decl: Iir;

      --  If True, the declaration is potentially visible (ie visible via a
      --  use clause).
      Is_Potential : Boolean;

      --  If True, previous declarations in PREV chain are hidden and shouldn't
      --  be considered.
      Prev_Hidden : Boolean;

      --  Previous interpretation for this identifier.
      --  If No_Name_Interpretation, this (not PREV) interpretation is the last
      --  one. If Prev_Hidden is True, PREV must be ignored.  If Prev_Hidden is
      --  false, the identifier is overloaded.
      Prev: Name_Interpretation_Type;

      --  Previous added identifier in the declarative region.  This forms a
      --  linked list used to remove interpretations when a declarative
      --  region is closed.
      Prev_In_Region : Name_Id;
   end record;
   pragma Pack (Interpretation_Cell);

   package Interpretations is new Tables
     (Table_Component_Type => Interpretation_Cell,
      Table_Index_Type => Name_Interpretation_Type,
      Table_Low_Bound => First_Valid_Interpretation,
      Table_Initial => 1024);

   --  Cached value of Prev_In_Region of current region.
   Last_In_Region : Name_Id := Null_Identifier;

   --  First interpretation in the current declarative region.
   Current_Region_Start : Name_Interpretation_Type :=
     First_Valid_Interpretation;

   --  First valid interpretation.  All interpretations smaller than this
   --  value are part of a previous (and nested) analysis and must not be
   --  considered.
   First_Interpretation : Name_Interpretation_Type :=
     First_Valid_Interpretation;

   --  List of non-local hidden declarations.
   type Hide_Index is new Nat32;
   No_Hide_Index : constant Hide_Index := 0;

   package Hidden_Decls is new Tables
     (Table_Component_Type => Name_Interpretation_Type,
      Table_Index_Type => Hide_Index,
      Table_Low_Bound => No_Hide_Index + 1,
      Table_Initial => 32);

   --  First non-local hidden declarations.  In VHDL, it is possible to hide
   --  an overloaded declaration (by declaring a subprogram with the same
   --  profile).   If the overloaded declaration is local, the interpretation
   --  can simply be modified.  But if it is not local, the interpretation is
   --  removed from the chain and saved in the Hidden_Decls table.
   First_Hide_Index : Hide_Index := No_Hide_Index;

   -- To manage the list of interpretation and to add informations to this
   -- list, a stack is used.
   -- Elements of stack can be of kind:
   -- Save_Cell:
   --   the element contains the interpretation INTER for the indentifier ID
   --   for the outer declarative region.
   --   A save cell is always created each time a declaration is added to save
   --   the previous interpretation.
   -- Region_Start:
   --   A new declarative region start at interpretation INTER.  Here, INTER
   --   is used as an index in the interpretations stack (table).
   --   ID is used as an index into the unidim_array stack.
   -- Barrier_start, Barrier_end:
   --   All currents interpretations are saved between both INTER, and
   --   are cleared.  This is used to call semantic during another semantic.

   type Scope_Cell_Kind_Type is (Scope_Start, Scope_Region);

   type Scope_Cell is record
      Kind: Scope_Cell_Kind_Type;

      --  Values for the previous scope.
      Saved_Last_In_Region : Name_Id;
      Saved_Region_Start : Name_Interpretation_Type;
      Saved_First_Hide_Index : Hide_Index;
      Saved_First_Interpretation : Name_Interpretation_Type;
   end record;

   package Scopes is new Tables
     (Table_Component_Type => Scope_Cell,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 64);

   function Valid_Interpretation (Inter : Name_Interpretation_Type)
                                 return Boolean is
   begin
      return Inter >= First_Interpretation;
   end Valid_Interpretation;

   --  Return True iff NI means there is a conflict for the identifier: no
   --  valid interpretation due to potentially visible homoraph.
   function Is_Conflict_Declaration (Ni : Name_Interpretation_Type)
                                    return Boolean is
   begin
      pragma Assert (Valid_Interpretation (Ni));
      return Interpretations.Table (Ni).Decl = Null_Iir;
   end Is_Conflict_Declaration;

   --  Get the current interpretation for ID.  The result is raw: it may not
   --  be valid.
   function Get_Interpretation_Raw (Id : Name_Id)
                                   return Name_Interpretation_Type is
   begin
      return Name_Interpretation_Type (Name_Table.Get_Name_Info (Id));
   end Get_Interpretation_Raw;

   procedure Set_Interpretation
     (Id : Name_Id; Inter : Name_Interpretation_Type) is
   begin
      Name_Table.Set_Name_Info (Id, Int32 (Inter));
   end Set_Interpretation;

   function Get_Interpretation_From_Raw (Inter : Name_Interpretation_Type)
                                        return Name_Interpretation_Type is
   begin
      if Valid_Interpretation (Inter)
        and then not Is_Conflict_Declaration (Inter)
      then
         --  In the current scopes set and not a conflict.
         return Inter;
      else
         return No_Name_Interpretation;
      end if;
   end Get_Interpretation_From_Raw;

   function Get_Interpretation (Id : Name_Id)
                               return Name_Interpretation_Type is
   begin
      return Get_Interpretation_From_Raw (Get_Interpretation_Raw (Id));
   end Get_Interpretation;

   procedure Check_Interpretations;
   pragma Unreferenced (Check_Interpretations);

   procedure Check_Interpretations
   is
      Inter: Name_Interpretation_Type;
      Last : constant Name_Interpretation_Type := Interpretations.Last;
      Err : Boolean;
   begin
      Err := False;
      for I in 0 .. Name_Table.Last_Name_Id loop
         Inter := Get_Interpretation (I);
         if Inter > Last then
            Log_Line ("bad interpretation for " & Name_Table.Image (I));
            Err := True;
         end if;
      end loop;
      if Err then
         raise Internal_Error;
      end if;
   end Check_Interpretations;

   procedure Push_Interpretations is
   begin
      Scopes.Append ((Kind => Scope_Start,
                      Saved_Last_In_Region => Last_In_Region,
                      Saved_Region_Start => Current_Region_Start,
                      Saved_First_Hide_Index => First_Hide_Index,
                      Saved_First_Interpretation => First_Interpretation));
      Last_In_Region := Null_Identifier;
      Current_Region_Start := Interpretations.Last + 1;
      First_Hide_Index := Hidden_Decls.Last + 1;
      First_Interpretation := Interpretations.Last + 1;
   end Push_Interpretations;

   procedure Pop_Interpretations
   is
      Cell : Scope_Cell renames Scopes.Table (Scopes.Last);
   begin
      pragma Assert (Scopes.Table (Scopes.Last).Kind = Scope_Start);

      --  All the declarative regions must have been removed.
      pragma Assert (Last_In_Region = Null_Identifier);
      pragma Assert (Current_Region_Start = Interpretations.Last + 1);
      pragma Assert (First_Hide_Index = Hidden_Decls.Last + 1);
      pragma Assert (First_Interpretation = Interpretations.Last + 1);

      Last_In_Region := Cell.Saved_Last_In_Region;
      Current_Region_Start := Cell.Saved_Region_Start;
      First_Hide_Index := Cell.Saved_First_Hide_Index;
      First_Interpretation := Cell.Saved_First_Interpretation;

      Scopes.Decrement_Last;
   end Pop_Interpretations;

   --  Create a new declarative region.
   --  Simply push a region_start cell and update current_scope_start.
   procedure Open_Declarative_Region is
   begin
      Scopes.Append ((Kind => Scope_Region,
                      Saved_Last_In_Region => Last_In_Region,
                      Saved_Region_Start => Current_Region_Start,
                      Saved_First_Hide_Index => First_Hide_Index,
                      Saved_First_Interpretation => No_Name_Interpretation));
      Last_In_Region := Null_Identifier;
      Current_Region_Start := Interpretations.Last + 1;
      First_Hide_Index := Hidden_Decls.Last + 1;
   end Open_Declarative_Region;

   --  Close a declarative region.
   --  Update interpretation of identifiers.
   procedure Close_Declarative_Region
   is
      Cell : Scope_Cell renames Scopes.Table (Scopes.Last);
      Id : Name_Id;
   begin
      pragma Assert (Cell.Kind = Scope_Region);

      --  Restore hidden declarations.
      for I in reverse First_Hide_Index .. Hidden_Decls.Last loop
         declare
            Inter : constant Name_Interpretation_Type :=
              Hidden_Decls.Table (I);
            Prev_Inter, Next_Inter : Name_Interpretation_Type;
         begin
            Prev_Inter := Interpretations.Table (Inter).Prev;
            Next_Inter := Interpretations.Table (Prev_Inter).Prev;
            Interpretations.Table (Inter).Prev := Next_Inter;
            Interpretations.Table (Prev_Inter).Prev := Inter;
         end;
      end loop;
      Hidden_Decls.Set_Last (First_Hide_Index - 1);

      --  Remove interpretations of that region.
      Id := Last_In_Region;
      if Id /= Null_Identifier then
         declare
            Inter : Name_Interpretation_Type;
         begin
            loop
               Inter := Get_Interpretation_Raw (Id);
               pragma Assert (Inter >= Current_Region_Start);
               Set_Interpretation (Id, Interpretations.Table (Inter).Prev);
               Id := Interpretations.Table (Inter).Prev_In_Region;
               exit when Id = Null_Identifier;
            end loop;
            pragma Assert (Inter = Current_Region_Start);
         end;
         Interpretations.Set_Last (Current_Region_Start - 1);
      end if;

      Last_In_Region := Cell.Saved_Last_In_Region;
      Current_Region_Start := Cell.Saved_Region_Start;
      First_Hide_Index := Cell.Saved_First_Hide_Index;

      Scopes.Decrement_Last;
   end Close_Declarative_Region;

   procedure Open_Scope_Extension renames Open_Declarative_Region;
   procedure Close_Scope_Extension renames Close_Declarative_Region;

   function Get_Next_Interpretation (Ni : Name_Interpretation_Type)
                                    return Name_Interpretation_Type
   is
      pragma Assert (Valid_Interpretation (Ni));
      Cell : Interpretation_Cell renames Interpretations.Table (Ni);
   begin
      if Cell.Prev_Hidden
        or else not Valid_Interpretation (Cell.Prev)
      then
         return No_Name_Interpretation;
      else
         return Cell.Prev;
      end if;
   end Get_Next_Interpretation;

   function Get_Declaration (Ni : Name_Interpretation_Type) return Iir is
   begin
      pragma Assert (Valid_Interpretation (Ni));
      return Interpretations.Table (Ni).Decl;
   end Get_Declaration;

   function Get_Under_Interpretation (Id : Name_Id)
                                     return Name_Interpretation_Type
   is
      Inter : constant Name_Interpretation_Type := Get_Interpretation (Id);
   begin
      --  ID has no interpretation.
      --  So, there is no 'under' interpretation (FIXME: prove it).
      pragma Assert (Valid_Interpretation (Inter));

      declare
         Cell : Interpretation_Cell renames Interpretations.Table (Inter);
         Prev : constant Name_Interpretation_Type := Cell.Prev;
      begin
         --  Get_Under_Interpretation can be used only to get a hidden
         --  interpretation.
         pragma Assert (Cell.Prev_Hidden);

         if Valid_Interpretation (Prev)
           --  Not a conflict one (use clauses).
           and then Get_Declaration (Prev) /= Null_Iir
         then
            return Prev;
         else
            return No_Name_Interpretation;
         end if;
      end;
   end Get_Under_Interpretation;

   function Strip_Non_Object_Alias (Decl : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Decl;
      if Get_Kind (Res) = Iir_Kind_Non_Object_Alias_Declaration then
         Res := Get_Named_Entity (Get_Name (Res));
      end if;
      return Res;
   end Strip_Non_Object_Alias;

   function Get_Non_Alias_Declaration (Ni : Name_Interpretation_Type)
                                      return Iir is
   begin
      return Strip_Non_Object_Alias (Get_Declaration (Ni));
   end Get_Non_Alias_Declaration;

   --  Return TRUE if INTER was made directly visible via a use clause.
   function Is_Potentially_Visible (Inter : Name_Interpretation_Type)
                                   return Boolean is
   begin
      return Interpretations.Table (Inter).Is_Potential;
   end Is_Potentially_Visible;

   --  Return TRUE iif DECL can be overloaded.
   function Is_Overloadable (Decl : Iir) return Boolean is
   begin
      --  LRM93 10.3:
      --  The overloaded declarations considered in this chapter are those for
      --  subprograms and enumeration literals.
      case Get_Kind (Decl) is
         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Function_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            return True;
         when Iir_Kind_Non_Object_Alias_Declaration =>
            case Get_Kind (Get_Named_Entity (Get_Name (Decl))) is
               when Iir_Kind_Enumeration_Literal
                 | Iir_Kind_Function_Declaration
                 | Iir_Kind_Procedure_Declaration
                 | Iir_Kind_Interface_Function_Declaration
                 | Iir_Kind_Interface_Procedure_Declaration =>
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

   --  Return TRUE if INTER was made direclty visible in the current
   --  declarative region.
   function Is_In_Current_Declarative_Region (Inter : Name_Interpretation_Type)
                                             return Boolean is
   begin
      return Inter >= Current_Region_Start;
   end Is_In_Current_Declarative_Region;

   --  Emit a warning when DECL hides PREV_DECL.
   procedure Warning_Hide (Decl : Iir; Prev_Decl : Iir)
   is
   begin
      if Get_Kind (Decl) in Iir_Kinds_Interface_Declaration
        and then Get_Kind (Get_Parent (Decl)) = Iir_Kind_Component_Declaration
      then
         --  Do not warn when an interface in a component hides a declaration.
         --  This is a common case (eg: in testbenches), and there is no real
         --  hiding.
         return;
      end if;

      if Get_Kind (Decl) = Iir_Kind_Element_Declaration then
         --  Do not warn for record elements.  They are used by selection.
         return;
      end if;

      if Decl = Prev_Decl then
         --  Can happen in configuration.  No real hidding.
         return;
      end if;

      if Name_Table.Get_Name_Ptr (Get_Identifier (Decl))(1) = 'P' then
         --  Do not warn for labels starting with 'P'.  These are canonicalized
         --  process labels which are scoped.
         --  This can happen as an architecture is canonicalized during
         --  analysis and then its declarations are 'imported' by a
         --  configuration.
         return;
      end if;

      Warning_Msg_Sem (Warnid_Hide, +Decl,
                       "declaration of %i hides %n", (+Decl, +Prev_Decl));
   end Warning_Hide;

   --  Add interpretation DECL to the identifier of DECL.
   --  POTENTIALLY is true if the identifier comes from a use clause.
   procedure Add_Name (Decl : Iir; Ident : Name_Id; Potentially : Boolean)
   is
      -- Current interpretation of ID.  This is the one before DECL is
      -- added (if so).
      Raw_Inter : constant Name_Interpretation_Type :=
        Get_Interpretation_Raw (Ident);
      Current_Inter : constant Name_Interpretation_Type :=
        Get_Interpretation_From_Raw (Raw_Inter);
      Current_Decl : Iir;

      --  Add DECL in the chain of interpretation for the identifier.
      procedure Add_New_Interpretation (Hid_Prev : Boolean; D : Iir := Decl) is
      begin
         Interpretations.Append ((Decl => D,
                                  Prev => Raw_Inter,
                                  Is_Potential => Potentially,
                                  Prev_Hidden => Hid_Prev,
                                  Prev_In_Region => Last_In_Region));
         Set_Interpretation (Ident, Interpretations.Last);
         Last_In_Region := Ident;
      end Add_New_Interpretation;
   begin
      if Ident = Null_Identifier then
         --  Missing identifier can happen only in case of parse error.
         pragma Assert (Flags.Flag_Force_Analysis);
         return;
      end if;

      if not Valid_Interpretation (Raw_Inter) then
         --  Very simple: no hidding, no overloading.
         Add_New_Interpretation (True);
         return;
      end if;

      if Is_Conflict_Declaration (Raw_Inter) then
         --  The current declaration for RAW_INTER is a conflict: there are
         --  multiple *potentially* visible declarations for the identifier.
         if Potentially then
            --  Yet another conflicting interpretation.
            return;
         else
            --  Very simple: no hidding, no overloading.
            --  (current interpretation is Conflict_Interpretation if there is
            --   only potentially visible declarations that are not made
            --   directly visible).
            --  Note: in case of conflict interpretation, it may be unnecessary
            --  to keep the current interpretation (but it is simpler as is).
            Add_New_Interpretation (True);
            return;
         end if;
      end if;

      if Potentially then
         --  Do not re-add a potential decl.  This handles cases like:
         --  'use p.all; use p.all;'.
         --  FIXME: add a flag (or reuse Visible_Flag) to avoid walking all
         --  the interpretations.
         declare
            Inter : Name_Interpretation_Type := Current_Inter;
         begin
            while Valid_Interpretation (Inter) loop
               if Get_Declaration (Inter) = Decl then
                  return;
               end if;
               Inter := Get_Next_Interpretation (Inter);
            end loop;
         end;
      end if;

      --  LRM 10.3 Visibility
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
         --  Current_Inter and Decl overloads (well, they have the same
         --  designator).

         --  LRM 10.3 Visibility
         --  If overloading is allowed for both declarations, then each of the
         --  two is a homograph of the other if they have the same identifier,
         --  operator symbol or character literal, as well as the same
         --  parameter and result profile.

         declare
            Homograph : Name_Interpretation_Type;
            Prev_Homograph : Name_Interpretation_Type;

            --  Hide HOMOGRAPH (ie unlink it from the chain of interpretation).
            procedure Hide_Homograph
            is
               S : Name_Interpretation_Type;
            begin
               if Prev_Homograph = No_Name_Interpretation then
                  Prev_Homograph := Interpretations.Last;
               end if;

               --  PREV_HOMOGRAPH must be the interpretation just before
               --  HOMOGRAPH.
               pragma Assert
                 (Interpretations.Table (Prev_Homograph).Prev = Homograph);

               --  Hide previous interpretation.
               Hidden_Decls.Append (Homograph);

               S := Interpretations.Table (Homograph).Prev;
               Interpretations.Table (Homograph).Prev := Prev_Homograph;
               Interpretations.Table (Prev_Homograph).Prev := S;
            end Hide_Homograph;

            function Get_Hash_Non_Alias (D : Iir) return Iir_Int32 is
            begin
               return Get_Subprogram_Hash (Strip_Non_Object_Alias (D));
            end Get_Hash_Non_Alias;

            --  Return True iff D is an implicit declaration (either a
            --  subprogram or an implicit alias).
            function Is_Implicit_Declaration (D : Iir) return Boolean is
            begin
               case Get_Kind (D) is
                  when Iir_Kind_Non_Object_Alias_Declaration =>
                     return Get_Implicit_Alias_Flag (D);
                  when Iir_Kind_Enumeration_Literal =>
                     return False;
                  when Iir_Kind_Procedure_Declaration
                    | Iir_Kind_Function_Declaration =>
                     return Is_Implicit_Subprogram (D);
                  when others =>
                     Error_Kind ("is_implicit_declaration", D);
               end case;
            end Is_Implicit_Declaration;

            --  Return TRUE iff D is an implicit alias of an implicit
            --  subprogram.
            function Is_Implicit_Alias (D : Iir) return Boolean is
            begin
               --  FIXME: Is it possible to have an implicit alias of an
               --  explicit subprogram ? Yes for enumeration literal and
               --  physical units.
               return Get_Kind (D) = Iir_Kind_Non_Object_Alias_Declaration
                 and then Get_Implicit_Alias_Flag (D)
                 and then Is_Implicit_Subprogram (Get_Named_Entity
                                                    (Get_Name (D)));
            end Is_Implicit_Alias;

            --  Replace the homograph of DECL by DECL.
            procedure Replace_Homograph is
            begin
               Interpretations.Table (Homograph).Decl := Decl;
            end Replace_Homograph;

            Decl_Hash : Iir_Int32;
            Hash : Iir_Int32;
         begin
            Decl_Hash := Get_Hash_Non_Alias (Decl);
            --  The hash must have been computed.
            pragma Assert (Decl_Hash /= 0);

            --  LRM02 10.3 Visibility
            --  Each of two declarations is said to be a /homograph/ of the
            --  other if both declarations have the same identifier, operator
            --  symbol, or character literal, and if overloading is allowed for
            --  at most one of the two.
            --
            --  LRM08 12.3 Visibility
            --  Each of two declarations is said to be a /homograph/ of the
            --  other if and only if both declarations have the same
            --  designator, and they denote different named entities, and
            --  either overloading is allows for at most one of the two, or
            --  overloading is allowed for both declarations and they have the
            --  same parameter and result type profile.

            --  GHDL: here we are in the case when both declarations are
            --  overloadable.  Also, always follow the LRM08 rules as they fix
            --  issues.
            --  GHDL: Special case for a second declaration with the same
            --  designator and that denotes the same named entity than a
            --  previous one (that would be an alias): according to the LRM,
            --  they are both visible and there are no ambiguity as they
            --  denotes the same named entity.  In GHDL, the new one hides the
            --  previous one.  The behaviour should be the same.

            --  Find an homograph of this declaration (and also keep the
            --  interpretation just before it in the chain).
            Homograph := Current_Inter;
            Prev_Homograph := No_Name_Interpretation;
            while Homograph /= No_Name_Interpretation loop
               Current_Decl := Get_Declaration (Homograph);
               Hash := Get_Hash_Non_Alias (Current_Decl);
               exit when Decl_Hash = Hash
                 and then Is_Same_Profile (Decl, Current_Decl);
               Prev_Homograph := Homograph;
               Homograph := Get_Next_Interpretation (Homograph);
            end loop;

            if Homograph = No_Name_Interpretation then
               --  Simple case: no homograph.
               Add_New_Interpretation (False);
               return;
            end if;

            --  There is an homograph (or the named entity is the same).
            if Potentially then
               --  Added DECL would be made potentially visible.

               --  LRM93 10.4 1) / LRM08 12.4 a) Use Clauses
               --  1. A potentially visible declaration is not made
               --     directly visible if the place considered is within the
               --     immediate scope of a homograph of the declaration.
               if not Is_Potentially_Visible (Homograph) then
                  return;
               end if;

               --  LRM08 12.4 Use Clauses
               --  b) If two potentially visible declarations are homograph
               --     and one is explicitly declared and the other is
               --     implicitly declared, then the implicit declaration is
               --     not made directly visible.
               if (Flags.Flag_Explicit or else Flags.Vhdl_Std >= Vhdl_08)
                 and then Is_Potentially_Visible (Homograph)
               then
                  declare
                     Implicit_Current_Decl : constant Boolean :=
                       Is_Implicit_Declaration (Current_Decl);
                     Implicit_Decl : constant Boolean :=
                       Is_Implicit_Declaration (Decl);
                  begin
                     if Implicit_Current_Decl and then not Implicit_Decl then
                        if Is_In_Current_Declarative_Region (Homograph) then
                           Replace_Homograph;
                        else
                           --  Insert DECL and hide homograph.
                           Add_New_Interpretation (False);
                           Hide_Homograph;
                        end if;
                        return;
                     elsif not Implicit_Current_Decl and then Implicit_Decl
                     then
                        --  Discard decl.
                        return;
                     elsif Strip_Non_Object_Alias (Decl)
                       = Strip_Non_Object_Alias (Current_Decl)
                     then
                        --  This rule is not written clearly in the LRM, but
                        --  if two designators denote the same named entity,
                        --  no need to make both visible.
                        return;
                     end if;
                  end;
               end if;

               --  GHDL: if the homograph is in the same declarative
               --  region than DECL, it must be an implicit declaration
               --  to be hidden.
               --  FIXME: this rule is not in the LRM93, but it is necessary
               --  so that explicit declaration hides the implicit one.
               if Flags.Vhdl_Std < Vhdl_08
                 and then not Flags.Flag_Explicit
                 and then Get_Parent (Decl) = Get_Parent (Current_Decl)
               then
                  declare
                     Implicit_Current_Decl : constant Boolean :=
                       Is_Implicit_Subprogram (Current_Decl);
                     Implicit_Decl : constant Boolean :=
                       Is_Implicit_Subprogram (Decl);
                  begin
                     if Implicit_Current_Decl and not Implicit_Decl then
                        --  Note: no need to save previous interpretation, as
                        --  it is in the same declarative region.
                        --  Replace the previous homograph with DECL.
                        Replace_Homograph;
                        return;
                     elsif not Implicit_Current_Decl and Implicit_Decl then
                        --  As we have replaced the homograph, it is possible
                        --  than the implicit declaration is re-added (by
                        --  a new use clause).  Discard it.
                        return;
                     end if;
                  end;
               end if;

               --  The homograph was made visible in an outer declarative
               --  region.  Therefore, it must not be hidden.
               Add_New_Interpretation (False);

               return;
            else
               --  Added DECL would be made directly visible.

               if not Is_Potentially_Visible (Homograph) then
                  --  The homograph was also declared in that declarative
                  --  region or in an inner one.
                  if Is_In_Current_Declarative_Region (Homograph) then
                     --  ... and was declared in the same region

                     --  To sum up: at this point both DECL and CURRENT_DECL
                     --  are overloadable, have the same profile (but may be
                     --  aliases) and are declared in the same declarative
                     --  region.

                     --  LRM08 12.3 Visibility
                     --  LRM93 10.3 Visibility
                     --  Two declarations that occur immediately within
                     --  the same declarative regions [...] shall not be
                     --  homograph, unless exactely one of them is the
                     --  implicit declaration of a predefined operation,

                     --  LRM08 12.3 Visibility
                     --  or is an implicit alias of such implicit declaration.
                     --
                     --  GHDL: FIXME: 'implicit alias'

                     --  LRM08 12.3 Visibility
                     --  LRM93 10.3 Visibility
                     --  Each of two declarations is said to be a
                     --  homograph of the other if and only if both
                     --  declarations have the same designator, [...]
                     --
                     --  LRM08 12.3 Visibility
                     --  [...] and they denote different named entities,
                     --  and [...]
                     declare
                        Is_Decl_Implicit : Boolean;
                        Is_Current_Decl_Implicit : Boolean;
                     begin
                        if Flags.Vhdl_Std >= Vhdl_08 then
                           Is_Current_Decl_Implicit :=
                             Is_Implicit_Subprogram (Current_Decl)
                             or else Is_Implicit_Alias (Current_Decl);
                           Is_Decl_Implicit := Is_Implicit_Subprogram (Decl)
                             or else Is_Implicit_Alias (Decl);

                           --  If they denote the same entity, they aren't
                           --  homograph.
                           if Strip_Non_Object_Alias (Decl)
                             = Strip_Non_Object_Alias (Current_Decl)
                           then
                              if Is_Current_Decl_Implicit
                                and then not Is_Decl_Implicit
                              then
                                 --  They aren't homograph but DECL is stronger
                                 --  (at it is not an implicit declaration)
                                 --  than CURRENT_DECL
                                 Replace_Homograph;
                              end if;

                              return;
                           end if;

                           if Is_Decl_Implicit
                             and then not Is_Current_Decl_Implicit
                           then
                              --  Re-declaration of an implicit subprogram via
                              --  an implicit alias is simply discarded.
                              return;
                           end if;
                        else
                           --  Can an implicit subprogram declaration appears
                           --  after an explicit one in vhdl 93?  I don't
                           --  think so.
                           Is_Decl_Implicit := Is_Implicit_Subprogram (Decl);
                           Is_Current_Decl_Implicit :=
                             Is_Implicit_Subprogram (Current_Decl);
                        end if;

                        if not (Is_Decl_Implicit xor Is_Current_Decl_Implicit)
                        then
                           Error_Msg_Sem
                             (+Decl, "redeclaration of %n defined at %l",
                              (+Current_Decl, +Current_Decl));
                           return;
                        end if;

                        if not Is_Decl_Implicit and Is_Current_Decl_Implicit
                        then
                           --  DECL 'overrides' the predefined current
                           --  declaration.

                           --  LRM93 10.3 Visibility
                           --  In such cases, a predefined operation is always
                           --  hidden by the other homograph.  Where hidden in
                           --  this manner, an implicit declaration is hidden
                           --  within the entire scope of the other declaration
                           --  (regardless of which declaration occurs first);
                           --  the implicit declaration is visible neither by
                           --  selection nor directly.
                           Set_Visible_Flag (Current_Decl, False);
                           if Get_Kind (Decl)
                             in Iir_Kinds_Subprogram_Declaration
                           then
                              Set_Hide_Implicit_Flag (Decl, True);
                           end if;
                        end if;
                     end;
                  else
                     --  GHDL: hide directly visible declaration declared in
                     --  an outer region.
                     null;
                  end if;
               else
                  --  LRM 10.4 Use Clauses
                  --  1. A potentially visible declaration is not made
                  --  directly visible if the place considered is within the
                  --  immediate scope of a homograph of the declaration.

                  --  GHDL: hide the potentially visible declaration.
                  null;
               end if;
               Add_New_Interpretation (False);

               Hide_Homograph;
               return;
            end if;
         end;
      end if;

      --  The current interpretation and the new one aren't overloadable, ie
      --  they are homograph (well almost).

      if Is_Potentially_Visible (Current_Inter) then
         if Potentially then
            --  LRM93 10.4 2) / LRM08 12.4 c) Use clauses
            --  Potentially visible declarations that have the same
            --  designator are not made directly visible unless each of
            --  them is either an enumeration literal specification or
            --  the declaration of a subprogram.
            if Decl = Get_Declaration (Current_Inter) then
               -- The rule applies only for distinct declaration.
               -- This handles 'use p.all; use P.all;'.
               -- FIXME: this should have been handled at the start of
               -- this subprogram.
               raise Internal_Error;
               return;
            end if;

            --  LRM08 12.3 Visibility
            --  Each of two declarations is said to be a homograph of the
            --  other if and only if both declarations have the same
            --  designator; and they denote different named entities, [...]
            if Flags.Vhdl_Std >= Vhdl_08 then
               if Strip_Non_Object_Alias (Decl)
                 = Strip_Non_Object_Alias (Current_Decl)
               then
                  return;
               end if;
            end if;

            --  Conflict.
            Add_New_Interpretation (True, Null_Iir);
            return;
         else
            --  LRM93 10.4 item #1
            --  A potentially visible declaration is not made directly
            --  visible if the place considered is within the immediate
            --  scope of a homograph of the declaration.
            --  GHDL: Could directly replace the previous interpretation
            --  (added in same scope), but don't do that for entity
            --  declarations, since it is used to find default binding.
            Add_New_Interpretation (True);
            return;
         end if;
      else
         --  There is already a declaration in the current scope.
         if Potentially then
            -- LRM93 10.4 item #1
            -- Discard the new and potentially visible declaration.
            -- However, add the type.
            -- FIXME: Add_In_Visible_List (Ident, Decl);
            return;
         else
            if Is_In_Current_Declarative_Region (Current_Inter) then
               --  They are perhaps visible in the same declarative region.

               if Get_Kind (Current_Decl) = Iir_Kind_Library_Declaration then
                  --  LRM93 11.2
                  --  If two or more logical names having the same
                  --  identifier appear in library clauses in the same
                  --  context, the second and subsequent occurences of the
                  --  logical name have no effect.  The same is true of
                  --  logical names appearing both in the context clause
                  --  of a primary unit and in the context clause of a
                  --  corresponding secondary unit.
                  --  GHDL: we apply this rule with VHDL-87, because of
                  --  implicit library clauses STD and WORK.
                  if Get_Kind (Decl) = Iir_Kind_Library_Declaration then
                     return;
                  end if;

                  if Flag_Relaxed_Rules
                    and then Get_Kind (Decl) in Iir_Kinds_Library_Unit
                  then
                     Warning_Msg_Sem
                       (Warnid_Hide, +Decl,
                        "unit %i hides library %i", (+Decl, +Decl));
                     Interpretations.Table (Current_Inter).Decl := Decl;
                     return;
                  end if;
               end if;

               -- None of the two declarations are potentially visible, ie
               -- both are visible.
               -- LRM 10.3:
               --  Two declarations that occur immediately within the same
               --  declarative region must not be homographs,
               -- FIXME: unless one of them is the implicit declaration of a
               --  predefined operation.
               Report_Start_Group;
               Error_Msg_Sem
                 (+Decl, "identifier %i already used for a declaration",
                  +Ident);
               Error_Msg_Sem
                 (+Current_Decl, "previous declaration: %n", +Current_Decl);
               Report_End_Group;
               return;
            else
               --  Homograph, not in the same scope.
               --  LRM93 10.3:
               --  A declaration is said to be hidden within (part of) an inner
               --  declarative region if the inner region contains an homograph
               --  of this declaration; the outer declaration is the hidden
               --  within the immediate scope of the inner homograph.
               if Is_Warning_Enabled (Warnid_Hide)
                 and then not Is_Potentially_Visible (Current_Inter)
               then
                  Warning_Hide (Decl, Current_Decl);
               end if;

               Add_New_Interpretation (True);
               return;
            end if;
         end if;
      end if;
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
      loop
         exit when Get_Declaration (Inter) = Old;
         Inter := Get_Next_Interpretation (Inter);
         pragma Assert (Valid_Interpretation (Inter));
      end loop;
      Interpretations.Table (Inter).Decl := Decl;
      pragma Assert (Get_Next_Interpretation (Inter) = No_Name_Interpretation);
   end Replace_Name;

   procedure Name_Visible (Decl : Iir) is
   begin
      --  A name can be made visible only once.
      pragma Assert (not Get_Visible_Flag (Decl));
      Set_Visible_Flag (Decl, True);
   end Name_Visible;

   procedure Iterator_Decl (Decl : Iir; Arg : Arg_Type)
   is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Subtype_Declaration
           | Iir_Kind_Enumeration_Literal --  By use clause
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kinds_Interface_Object_Declaration
           | Iir_Kind_Interface_Package_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration
           | Iir_Kinds_Quantity_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kinds_Concurrent_Statement
           | Iir_Kinds_Sequential_Statement =>
            Handle_Decl (Decl, Arg);
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            if not Is_Second_Subprogram_Specification (Decl) then
               Handle_Decl (Decl, Arg);
            end if;
         when Iir_Kind_Type_Declaration =>
            declare
               Def : constant Iir := Get_Type_Definition (Decl);
               List : Iir_Flist;
               El : Iir;
            begin
               -- Handle incomplete type declaration.
               if Get_Kind (Def) = Iir_Kind_Incomplete_Type_Definition then
                  return;
               end if;

               Handle_Decl (Decl, Arg);

               if Get_Kind (Def) = Iir_Kind_Enumeration_Type_Definition then
                  List := Get_Enumeration_Literal_List (Def);
                  for I in Flist_First .. Flist_Last (List) loop
                     El := Get_Nth_Element (List, I);
                     Handle_Decl (El, Arg);
                  end loop;
               end if;
            end;
         when Iir_Kind_Anonymous_Type_Declaration =>
            Handle_Decl (Decl, Arg);

            declare
               Def : constant Iir := Get_Type_Definition (Decl);
               El : Iir;
            begin
               if Get_Kind (Def) = Iir_Kind_Physical_Type_Definition then
                  El := Get_Unit_Chain (Def);
                  while El /= Null_Iir loop
                     Handle_Decl (El, Arg);
                     El := Get_Chain (El);
                  end loop;
               end if;
            end;
         when Iir_Kind_Interface_Type_Declaration =>
            Handle_Decl (Decl, Arg);
            declare
               El : Iir;
            begin
               El := Get_Interface_Type_Subprograms (Decl);
               while El /= Null_Iir loop
                  Handle_Decl (El, Arg);
                  El := Get_Chain (El);
               end loop;
            end;
         when Iir_Kind_Use_Clause
           | Iir_Kind_Context_Reference =>
            Handle_Decl (Decl, Arg);
         when Iir_Kind_Library_Clause =>
            Handle_Decl (Decl, Arg);
--             El := Get_Library_Declaration (Decl);
--             if El /= Null_Iir then
--                --  May be empty.
--                Handle_Decl (El, Arg);
--             end if;

         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            null;

         when Iir_Kind_Package_Body =>
            null;

         when Iir_Kind_Attribute_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Disconnection_Specification =>
            null;
         when Iir_Kinds_Signal_Attribute
           | Iir_Kind_Signal_Attribute_Declaration =>
            null;

         when Iir_Kind_Protected_Type_Body
           | Iir_Kind_Suspend_State_Declaration =>
            --  FIXME: allowed only in debugger (if the current scope is
            --  within a package body) ?
            null;

         when others =>
            Error_Kind ("iterator_decl", Decl);
      end case;
   end Iterator_Decl;

   --  Handle context_clause of context reference CTXT.
   procedure Add_One_Context_Reference (Ctxt : Iir)
   is
      Name : constant Iir := Get_Selected_Name (Ctxt);
      Ent : constant Iir := Get_Named_Entity (Name);
      Item : Iir;
   begin
      if Ent = Null_Iir or else Is_Error (Ent) then
         --  Stop now in case of error.
         return;
      end if;
      pragma Assert (Get_Kind (Ent) = Iir_Kind_Context_Declaration);

      Item := Get_Context_Items (Ent);
      while Item /= Null_Iir loop
         case Get_Kind (Item) is
            when Iir_Kind_Use_Clause =>
               Add_Use_Clause (Item);
            when Iir_Kind_Library_Clause =>
               Add_Name (Get_Library_Declaration (Item),
                         Get_Identifier (Item), False);
            when Iir_Kind_Context_Reference =>
               Add_Context_Reference (Item);
            when others =>
               Error_Kind ("add_context_reference", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Add_One_Context_Reference;

   procedure Add_Context_Reference (Ref : Iir)
   is
      Ctxt : Iir;
   begin
      Ctxt := Ref;
      loop
         Add_One_Context_Reference (Ctxt);
         Ctxt := Get_Context_Reference_Chain (Ctxt);
         exit when Ctxt = Null_Iir;
      end loop;
   end Add_Context_Reference;

   --  Make POTENTIALLY (or not) visible DECL.
   procedure Add_Name_Decl (Decl : Iir; Potentially : Boolean) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Use_Clause =>
            if not Potentially then
               Add_Use_Clause (Decl);
            end if;
         when Iir_Kind_Context_Reference =>
            pragma Assert (not Potentially);
            Add_Context_Reference (Decl);
         when Iir_Kind_Library_Clause =>
            Add_Name (Get_Library_Declaration (Decl),
                      Get_Identifier (Decl), Potentially);
         when Iir_Kind_Anonymous_Type_Declaration =>
            null;
         when others =>
            Add_Name (Decl, Get_Identifier (Decl), Potentially);
      end case;
   end Add_Name_Decl;

   procedure Add_Declaration is
      new Iterator_Decl (Arg_Type => Boolean, Handle_Decl => Add_Name_Decl);

   procedure Iterator_Decl_List (Decl_List : Iir_List; Arg : Arg_Type)
   is
      Decl : Iir;
      It : List_Iterator;
   begin
      if Decl_List = Null_Iir_List then
         return;
      end if;
      It := List_Iterate (Decl_List);
      while Is_Valid (It) loop
         Decl := Get_Element (It);
         Handle_Decl (Decl, Arg);
         Next (It);
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

   procedure Add_Declarations_1 is new Iterator_Decl_Chain
     (Arg_Type => Boolean, Handle_Decl => Add_Declaration);

   procedure Add_Declarations (Chain : Iir; Potentially : Boolean := False)
     renames Add_Declarations_1;

   procedure Add_Declarations_List is new Iterator_Decl_List
     (Arg_Type => Boolean, Handle_Decl => Add_Declaration);

   procedure Add_Declarations_From_Interface_Chain (Chain : Iir)
   is
      El : Iir;
      Id : Name_Id;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Id := Get_Identifier (El);

         --  The chain may be from an implicitely declared subprograms, with
         --  anonymous identifiers.  In that case, all interfaces are
         --  anonymous and there is no need to iterate.
         exit when Id = Null_Identifier;

         Add_Declaration (El, False);

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
      Prev_Hide : constant Boolean := Is_Warning_Enabled (Warnid_Hide);
   begin
      --  Temporarly disable hide warning to avoid spurious messages.
      Enable_Warning (Warnid_Hide, False);

      Add_Declarations_From_Interface_Chain (Get_Generic_Chain (Entity));
      Add_Declarations_From_Interface_Chain (Get_Port_Chain (Entity));
      Add_Declarations (Get_Declaration_Chain (Entity), False);
      Add_Declarations_Of_Concurrent_Statement (Entity);

      --  Restore
      Enable_Warning (Warnid_Hide, Prev_Hide);
   end Add_Entity_Declarations;

   --  Add declarations from a package into the current declarative region.
   --  (for a use clause or when a package body is analyzed)
   procedure Add_Package_Declarations
     (Decl: Iir_Package_Declaration; Potentially : Boolean)
   is
      Header : constant Iir := Get_Package_Header (Decl);
   begin
      --  LRM08 12.1 Declarative region
      --  d) A package declaration together with the corresponding body
      --
      --  GHDL: the formal generic declarations are considered to be in the
      --  same declarative region as the package declarations (and therefore
      --  in the same scope), even if they don't occur immediately within a
      --  package declaration.
      if Header /= Null_Iir then
         Add_Declarations (Get_Generic_Chain (Header), Potentially);
      end if;

      Add_Declarations (Get_Declaration_Chain (Decl), Potentially);
   end Add_Package_Declarations;

   procedure Add_Package_Instantiation_Declarations
     (Decl: Iir; Potentially : Boolean) is
   begin
      --  LRM08 4.9 Package instantiation declarations
      --  The package instantiation declaration is equivalent to declaration of
      --  a generic-mapped package, consisting of a package declaration [...]
      Add_Declarations (Get_Generic_Chain (Decl), Potentially);
      Add_Declarations (Get_Declaration_Chain (Decl), Potentially);
   end Add_Package_Instantiation_Declarations;

   --  Add declarations from a package into the current declarative region.
   --  This is needed when a package body is analysed.
   procedure Add_Package_Declarations (Decl: Iir_Package_Declaration) is
   begin
      Add_Package_Declarations (Decl, False);
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
         when Iir_Kind_Architecture_Body =>
            Add_Context_Clauses (Get_Design_Unit (Decl));
         when Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement_Body =>
            --  FIXME: formal, iterator ?
            null;
         when others =>
            Error_Kind ("extend_scope_of_block_declarations", Decl);
      end case;
      Add_Declarations (Get_Declaration_Chain (Decl), False);
      Add_Declarations_Of_Concurrent_Statement (Decl);
   end Extend_Scope_Of_Block_Declarations;

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

   procedure Potentially_Add_Name (Name : Iir) is
   begin
      Add_Name (Name, Get_Identifier (Name), True);
   end Potentially_Add_Name;

   --  LRM08 12.4 Use clauses
   --  Moreover, the following declarations, if any, that occurs immediately
   --  within the package denoted by the prefix of the selected name, are also
   --  identifier:
   procedure Use_Selected_Type_Name (Name : Iir)
   is
      Type_Def : constant Iir := Get_Type (Name);
      Base_Type : constant Iir := Get_Base_Type (Type_Def);
   begin
      case Get_Kind (Base_Type) is
         when Iir_Kind_Enumeration_Type_Definition =>
            --  LRM08 12.4 Use clauses
            --  - If the type mark denotes an enumeration type of a subtype of
            --    an enumeration type, the enumeration literals of the base
            --    type
            declare
               List : constant Iir_Flist :=
                 Get_Enumeration_Literal_List (Base_Type);
               El : Iir;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Potentially_Add_Name (El);
               end loop;
            end;
         when Iir_Kind_Physical_Type_Definition =>
            --  LRM08 12.4 Use clauses
            --  - If the type mark denotes a subtype of a physical type, the
            --    units of the base type
            declare
               El : Iir;
            begin
               El := Get_Unit_Chain (Base_Type);
               while El /= Null_Iir loop
                  Potentially_Add_Name (El);
                  El := Get_Chain (El);
               end loop;
            end;
         when others =>
            null;
      end case;

      --  LRM08 12.4 Use clauses
      --  - The implicit declarations of predefined operations for the type
      --    that are not hidden by homographs explicitly declared immediately
      --    within the package denoted by the prefix of the selected name
      --  - The declarations of homographs, explicitly declared immediately
      --    within the package denotes by the prefix of the selected name,
      --    that hide implicit declarations of predefined operations for the
      --    type
      declare
         Type_Decl : constant Iir := Get_Type_Declarator (Base_Type);
         El : Iir;
         Has_Override : Boolean;
      begin
         Has_Override := False;
         El := Get_Chain (Type_Decl);
         while El /= Null_Iir loop
            if Is_Implicit_Subprogram (El)
              and then Is_Operation_For_Type (El, Base_Type)
            then
               if Get_Visible_Flag (El) then
                  --  Implicit declaration EL was overriden by a user
                  --  declaration.  Don't make it visible.
                  Potentially_Add_Name (El);
               else
                  Has_Override := True;
               end if;
               El := Get_Chain (El);
            else
               exit;
            end if;
         end loop;

         --  Explicitly declared homograph.
         if Has_Override then
            while El /= Null_Iir loop
               if Get_Kind (El) in Iir_Kinds_Subprogram_Declaration
                 and then Get_Hide_Implicit_Flag (El)
                 and then Is_Operation_For_Type (El, Base_Type)
               then
                  Potentially_Add_Name (El);
               end if;
               El := Get_Chain (El);
            end loop;
         end if;
      end;
   end Use_Selected_Type_Name;

   --  LRM02 10.4 Use clauses
   --  Each selected name in a use clause identifiers one or more declarations
   --  that will potentially become directly visible. If the suffix of the
   --  selected name is a simple name, a character literal, or operator
   --  symbol, then the selected name identifiers only the declarations(s) of
   --  that simple name, character literal, or operator symbol contained
   --  within the package or library denoted by the prefix of the selected
   --  name.
   procedure Use_Selected_Name (Name : Iir)
   is
      Nname : Iir;
   begin
      if Name = Null_Iir then
         return;
      end if;

      case Get_Kind (Name) is
         when Iir_Kind_Overload_List =>
            Add_Declarations_List (Get_Overload_List (Name), True);
         when Iir_Kind_Error =>
            null;
         when others =>
            Potentially_Add_Name (Name);

            --  LRM08 12.4 Use clauses
            --  If the suffix of the selected name is a type mark, then the
            --  declaration of the type or subtype denoted by the type mark
            --  is identified. Moreover [...]
            if (Vhdl_Std >= Vhdl_08 or else Flag_Relaxed_Rules) then
               Nname := Strip_Non_Object_Alias (Name);
               if Get_Kind (Nname) in Iir_Kinds_Type_Declaration then
                  Use_Selected_Type_Name (Nname);
               end if;
            end if;
      end case;
   end Use_Selected_Name;

   --  LRM93 10.4 Use clauses
   --  If the suffix is the reserved word ALL, then all the selected name
   --  identifies all declaration that are contained within the package or
   --  library denotes by te prefix of the selected name.
   procedure Use_All_Names (Name: Iir) is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Library_Declaration =>
            Use_Library_All (Name);
         when Iir_Kind_Package_Declaration =>
            Add_Package_Declarations (Name, True);
         when Iir_Kind_Package_Instantiation_Declaration =>
            Add_Package_Instantiation_Declarations (Name, True);
         when Iir_Kind_Interface_Package_Declaration =>
            --  LRM08 6.5.5 Interface package declarations
            --  Within an entity declaration, an architecture body, a
            --  component declaration, or an uninstantiated subprogram or
            --  package declaration that declares a given interface package,
            --  the name of the given interface package denotes an undefined
            --  instance of the uninstantiated package.
            Add_Package_Instantiation_Declarations (Name, True);
         when Iir_Kind_Error =>
            null;
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
         if Name = Null_Iir then
            pragma Assert (Flags.Flag_Force_Analysis);
            null;
         else
            if Get_Kind (Name) = Iir_Kind_Selected_By_All_Name then
               Name := Get_Prefix (Name);
               if not Is_Error (Name) then
                  Use_All_Names (Get_Named_Entity (Name));
               end if;
            else
               if not Is_Error (Name) then
                  Use_Selected_Name (Get_Named_Entity (Name));
               end if;
            end if;
         end if;
         Cl := Get_Use_Clause_Chain (Cl);
         exit when Cl = Null_Iir;
      end loop;
   end Add_Use_Clause;

   procedure Add_Inherit_Spec (Spec : Iir)
   is
      Name : constant Iir := Get_Name (Spec);
      Unit : Iir;
      Item : Iir;
   begin
      if Name = Null_Iir then
         return;
      end if;
      Unit := Get_Named_Entity (Name);
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Iir loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Declaration =>
               Potentially_Add_Name (Item);
            when others =>
               Error_Kind ("add_inherit_spec", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Add_Inherit_Spec;

   --  Debugging subprograms.
   procedure Disp_All_Names;
   pragma Unreferenced (Disp_All_Names);

   procedure Disp_Scopes;
   pragma Unreferenced (Disp_Scopes);

   procedure Disp_Detailed_Interpretations (Ident : Name_Id);
   pragma Unreferenced (Disp_Detailed_Interpretations);

   procedure Dump_Current_Scope;
   pragma Unreferenced (Dump_Current_Scope);

   procedure Disp_Detailed_Interpretations (Ident : Name_Id)
   is
      Inter: Name_Interpretation_Type;
      Decl : Iir;
   begin
      Log (Name_Table.Image (Ident));
      Log_Line (":");

      Inter := Get_Interpretation (Ident);
      while Valid_Interpretation (Inter) loop
         Log (Name_Interpretation_Type'Image (Inter));
         if Is_Potentially_Visible (Inter) then
            Log (" (use)");
         end if;
         Log (":");
         Decl := Get_Declaration (Inter);
         Log (Iir'Image (Decl));
         Log (":");
         Log (Iir_Kind'Image (Get_Kind (Decl)));
         Log_Line (", loc: " & Image (Get_Location (Decl)));
         if Get_Kind (Decl) in Iir_Kinds_Subprogram_Declaration then
            Log_Line ("   " & Disp_Subprg (Decl));
         end if;
         Inter := Get_Next_Interpretation (Inter);
      end loop;
   end Disp_Detailed_Interpretations;

   procedure Disp_All_Interpretations
     (Interpretation : Name_Interpretation_Type)
   is
      Inter: Name_Interpretation_Type;
   begin
      Inter := Interpretation;
      while Valid_Interpretation (Inter) loop
         Log (Name_Interpretation_Type'Image (Inter));
         Log (".");
         Log (Iir_Kind'Image (Get_Kind (Get_Declaration (Inter))));
         Inter := Get_Next_Interpretation (Inter);
      end loop;
      Log_Line;
   end Disp_All_Interpretations;

   procedure Disp_All_Names
   is
      Inter: Name_Interpretation_Type;
   begin
      for I in 0 .. Name_Table.Last_Name_Id loop
         Inter := Get_Interpretation (I);
         if Valid_Interpretation (Inter) then
            Log (Name_Table.Image (I));
            Log (Name_Id'Image (I));
            Log (":");
            Disp_All_Interpretations (Inter);
         end if;
      end loop;
      Log_Line ("interprations.last = "
                & Name_Interpretation_Type'Image (Interpretations.Last));
      Log_Line ("current_region_start ="
                & Name_Interpretation_Type'Image (Current_Region_Start));
   end Disp_All_Names;

   procedure Dump_Interpretation (Inter : Name_Interpretation_Type)
   is
      Decl : Iir;
   begin
      Log (Name_Interpretation_Type'Image (Inter));
      if Is_Potentially_Visible (Inter) then
         Log (" (use)");
      end if;
      Log (": ");
      Decl := Get_Declaration (Inter);
      if Decl = Null_Iir then
         Log_Line ("null: conflict");
      else
         Log (Iir_Kind'Image (Get_Kind (Decl)));
         Log_Line (", loc: " & Image (Get_Location (Decl)));
         if Get_Kind (Decl) in Iir_Kinds_Subprogram_Declaration then
            Log_Line ("   " & Disp_Subprg (Decl));
         end if;
      end if;
   end Dump_Interpretation;

   procedure Dump_A_Scope (First, Last : Name_Interpretation_Type) is
   begin
      if First > Last then
         Log_Line ("scope is empty");
         return;
      end if;

      for Inter in reverse First .. Last loop
         declare
            Cell : Interpretation_Cell renames Interpretations.Table (Inter);
         begin
            Dump_Interpretation (Inter);
            if Cell.Prev_Hidden then
               Log ("  [prev:");
               Log (Name_Interpretation_Type'Image (Cell.Prev));
               if Cell.Prev_Hidden then
                  Log (" hidden");
               end if;
               Log_Line ("]");
            else
               if Cell.Prev < First then
                  Log_Line (" [last in scope]");
               end if;
            end if;
         end;
      end loop;
   end Dump_A_Scope;

   procedure Dump_Current_Scope is
   begin
      Dump_A_Scope (Current_Region_Start, Interpretations.Last);
   end Dump_Current_Scope;

   procedure Disp_Scopes is
   begin
      for I in reverse Scopes.First .. Scopes.Last loop
         declare
            S : Scope_Cell renames Scopes.Table (I);
         begin
            case S.Kind is
               when Scope_Start =>
                  Log ("scope_start at");
               when Scope_Region =>
                  Log ("scope_region at");
            end case;
            Log_Line (Name_Interpretation_Type'Image (S.Saved_Region_Start));
         end;
      end loop;
   end Disp_Scopes;
end Vhdl.Sem_Scopes;
