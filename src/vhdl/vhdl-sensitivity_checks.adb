--  Synthesis checks
--  Copyright (C) 2021-2023 Ondrej Ille, Hipólito Guzmán-Miranda, T. Gingold.
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

with Errorout; use Errorout;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Canon;

package body Vhdl.Sensitivity_Checks is

   type Bool_Vector is array (Natural range <>) of Boolean;
   pragma Pack (Bool_Vector);

   --  The data for checking sensitivity.
   type Context_Type (Len : Natural) is record
      --  The sensitivity list of the process.  Combinatorial signals
      --  referenced must be in the list.
      Sensitivity_List : Iir_List;

      --  List of signals not found in the previous list (to avoid duplicate
      --  errors).
      Missing_List : Iir_List;

      --  For each signal in the sensitivity list, set if it is referenced.
      --  Used to detect extra sensitivity signals.
      Referenced : Bool_Vector (1 .. Len);
   end record;

   --  Return TRUE iff REF and NAME refer to the same selected element of the
   --  same object.
   function Same_Selected_Element (Ref : Iir; Name : Iir) return Boolean
   is
      Pfx_Name, Pfx_Ref : Iir;
   begin
      --  Just one level of selected elements.
      --  TODO: multi-level ?
      if Get_Kind (Name) /= Iir_Kind_Selected_Element then
         return False;
      end if;
      if Get_Named_Entity (Name) /= Get_Named_Entity (Ref) then
         return False;
      end if;

      Pfx_Name := Get_Prefix (Name);
      Pfx_Ref := Get_Prefix (Ref);
      return Get_Named_Entity (Pfx_Name) = Get_Named_Entity (Pfx_Ref);
   end Same_Selected_Element;

   --  Check NAME is present in the sensitivity list.
   procedure Check_Sensitivity_Name (Name : Iir; Ctxt : in out Context_Type)
   is
      It : List_Iterator;
      El : Iir;
      Obj : Iir;
      El_Obj : Iir;
      Idx : Natural;
   begin
      --  Try to find NAME in LIST; if found set the corresponding element
      --  in REF.
      Obj := Get_Object_Prefix (Name, False);
      Obj := Name_To_Object (Obj);
      if Obj = Null_Iir or else not Is_Signal_Name (Obj) then
         --  Error or not a signal.
         return;
      end if;

      --  Linear search...
      --  TODO: maybe just add a flag (reuse Open_Flag ?)
      It := List_Iterate (Ctxt.Sensitivity_List);
      Idx := 1;
      while Is_Valid (It) loop
         El := Get_Element (It);

         El_Obj := Name_To_Object (El);
         if El_Obj = Null_Iir then
            --  Nothing to do in case of error.
            null;
         elsif El_Obj = Obj then
            Ctxt.Referenced (Idx) := True;
            return;
         elsif Get_Kind (El_Obj) = Iir_Kind_Selected_Element
           and then Same_Selected_Element (El_Obj, Name)
         then
            Ctxt.Referenced (Idx) := True;
            return;
         end if;

         Next (It);
         Idx := Idx + 1;
      end loop;

      --  Avoid duplicate warnings.
      if Ctxt.Missing_List /= Null_Iir_List then
         It := List_Iterate (Ctxt.Missing_List);
         while Is_Valid (It) loop
            if Obj = Get_Element (It) then
               --  Warning already reported.
               return;
            end if;
            Next (It);
         end loop;
      end if;

      if Get_Kind (Obj) in Iir_Kinds_Signal_Attribute then
         --  Not sure what could be done for signal attributes.
         --  Anyway, they aren't synthesizable.
         Warning_Msg_Sem (Warnid_Sensitivity, +Name,
                          "unexpected signal attribute for sensitivity");
      else
         Warning_Msg_Sem (Warnid_Sensitivity, +Name,
                          "incomplete sensitivity list, signal %i is missing",
                          +Obj);
      end if;

      --  Add OBJ to the missing list (so that the warning is not repeated).
      if Ctxt.Missing_List = Null_Iir_List then
         Ctxt.Missing_List := Create_Iir_List;
      end if;
      Append_Element (Ctxt.Missing_List, Obj);
   end Check_Sensitivity_Name;

   procedure Check_Sensitivity_Names
     (Names : Iir_List; Ctxt : in out Context_Type)
   is
      It : List_Iterator;
   begin
      It := List_Iterate (Names);
      while Is_Valid (It) loop
         Check_Sensitivity_Name (Get_Element (It), Ctxt);
         Next (It);
      end loop;
   end Check_Sensitivity_Names;

   procedure Check_Sensitivity_Stmt (Stmt : Iir; Ctxt : in out Context_Type)
   is
      Sens : Iir_List;
   begin
      Sens := Create_Iir_List;
      Canon.Canon_Extract_Sensitivity_Statement (Stmt, Sens);
      Check_Sensitivity_Names (Sens, Ctxt);
      Destroy_Iir_List (Sens);
   end Check_Sensitivity_Stmt;

   procedure Check_Sensitivity_Expr (Expr : Iir; Ctxt : in out Context_Type)
   is
      Sens : Iir_List;
   begin
      Sens := Create_Iir_List;
      Canon.Canon_Extract_Sensitivity_Expression (Expr, Sens);
      Check_Sensitivity_Names (Sens, Ctxt);
      Destroy_Iir_List (Sens);
   end Check_Sensitivity_Expr;

   procedure Check_Sensitivity_Stmts (Stmts : Iir; Ctxt : in out Context_Type)
   is
      Stmt : Iir;
   begin
      Stmt := Stmts;
      while Stmt /= Null_Iir loop
         Check_Sensitivity_Stmt (Stmt, Ctxt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Check_Sensitivity_Stmts;

   function Get_Sensitivity_Edge (Expr : Iir) return Iir is
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Function_Call =>
            case Get_Implicit_Definition (Get_Implementation (Expr)) is
               when Iir_Predefined_Boolean_Rising_Edge
                 | Iir_Predefined_Boolean_Falling_Edge
                 | Iir_Predefined_Bit_Rising_Edge
                 | Iir_Predefined_Bit_Falling_Edge
                 | Iir_Predefined_Ieee_1164_Rising_Edge
                 | Iir_Predefined_Ieee_1164_Falling_Edge =>
                  declare
                     Assoc : constant Iir :=
                       Get_Parameter_Association_Chain (Expr);
                  begin
                     if Get_Kind (Assoc)
                       = Iir_Kind_Association_Element_By_Expression
                     then
                        return Get_Actual (Assoc);
                     end if;

                     --  Syntax or semantic error ?
                     return Null_Iir;
                  end;
               when others =>
                  return Null_Iir;
            end case;

         when Iir_Kind_Event_Attribute =>
            return Get_Prefix (Expr);

         when Iir_Kind_And_Operator =>
            declare
               E, R : Iir;
            begin
               E := Get_Left (Expr);
               R := Get_Sensitivity_Edge (E);
               if R /= Null_Iir then
                  return R;
               end if;

               E := Get_Right (Expr);
               R := Get_Sensitivity_Edge (E);
               return R;
            end;

         when Iir_Kind_Parenthesis_Expression =>
            return Get_Sensitivity_Edge (Get_Expression (Expr));

         when others =>
            return Null_Iir;
      end case;
   end Get_Sensitivity_Edge;

   procedure Check_Sensitivity_If (Stmt : Iir; Ctxt : in out Context_Type)
   is
      Branch : Iir;
      Cond : Iir;
      Name : Iir;
   begin
      Branch := Stmt;
      while Branch /= Null_Iir loop
         Cond := Get_Condition (Branch);
         if Cond = Null_Iir then
            --  Final else clause.
            Check_Sensitivity_Stmts
              (Get_Sequential_Statement_Chain (Branch), Ctxt);
         else
            Name := Get_Sensitivity_Edge (Cond);
            if Name /= Null_Iir then
               --  There is an edge expression (like 'rising_edge (x)').
               --  Simply check the expression and discard the whole
               --  statement.
               if Get_Kind (Name) in Iir_Kinds_Denoting_Name then
                  Check_Sensitivity_Name (Name, Ctxt);
               else
                  Check_Sensitivity_Expr (Name, Ctxt);
               end if;
               return;
            else
               --  A 'normal' if statement.
               Check_Sensitivity_Expr
                 (Cond, Ctxt);
               Check_Sensitivity_Stmts
                 (Get_Sequential_Statement_Chain (Branch), Ctxt);
            end if;
         end if;
         Branch := Get_Else_Clause (Branch);
      end loop;
   end Check_Sensitivity_If;

   procedure Report_Redundant_Sensitivity (Ctxt : Context_Type)
   is
      It : List_Iterator;
      Idx : Natural;
      El : Iir;
   begin
      It := List_Iterate (Ctxt.Sensitivity_List);
      Idx := 1;
      while Is_Valid (It) loop
         if not Ctxt.Referenced (Idx) then
            El := Get_Element (It);
            if not Is_Error (El) then
               while Get_Kind (El) not in Iir_Kinds_Denoting_Name loop
                  El := Get_Prefix (El);
               end loop;
               Warning_Msg_Sem (Warnid_Sensitivity, +El,
                                "extra signal %i in sensitivity list", +El);
            end if;
         end if;
         Next (It);
         Idx := Idx + 1;
      end loop;
   end Report_Redundant_Sensitivity;

   procedure Check_Sensitivity_List (Proc : Iir)
   is
      List : constant Iir_List := Get_Sensitivity_List (Proc);
      Stmts : constant Iir := Get_Sequential_Statement_Chain (Proc);
      Nbr_Sens : constant Natural := Get_Nbr_Elements (List);
      Ctxt : Context_Type (Nbr_Sens);
      Stmt : Iir;
   begin
      --  TODO: sanity check the sensitivity list.

      --  Build the context
      Ctxt := (Len => Nbr_Sens,
               Sensitivity_List => List,
               Missing_List => Null_Iir_List,
               Referenced => (others => False));

      Stmt := Stmts;
      while Stmt /= Null_Node loop
         if Get_Kind (Stmt) = Iir_Kind_If_Statement then
            --  Special handling
            Check_Sensitivity_If (Stmt, Ctxt);
         else
            Check_Sensitivity_Stmt (Stmt, Ctxt);
         end if;
         Stmt := Get_Chain (Stmt);
      end loop;

      if Ctxt.Missing_List /= Null_Iir_List then
         Destroy_Iir_List (Ctxt.Missing_List);
      end if;

      --  Report unreferenced elements (might be redundant!)
      Report_Redundant_Sensitivity (Ctxt);
   end Check_Sensitivity_List;
end Vhdl.Sensitivity_Checks;
