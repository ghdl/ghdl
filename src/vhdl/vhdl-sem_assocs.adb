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
with Vhdl.Evaluation; use Vhdl.Evaluation;
with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Flags; use Flags;
with Types; use Types;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Parse;
with Std_Names;
with Vhdl.Sem_Names; use Vhdl.Sem_Names;
with Vhdl.Sem_Types;
with Vhdl.Sem_Decls;
with Vhdl.Std_Package;
with Vhdl.Sem_Scopes;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Vhdl.Xrefs;

package body Vhdl.Sem_Assocs is
   function Rewrite_Non_Object_Association (Assoc : Iir; Inter : Iir)
                                           return Iir
   is
      N_Assoc : Iir;
      Actual : Iir;
   begin
      Actual := Get_Actual (Assoc);
      case Get_Kind (Inter) is
         when Iir_Kind_Interface_Package_Declaration =>
            N_Assoc := Create_Iir (Iir_Kind_Association_Element_Package);
         when Iir_Kind_Interface_Type_Declaration =>
            N_Assoc := Create_Iir (Iir_Kind_Association_Element_Type);
            if Get_Kind (Actual) = Iir_Kind_Parenthesis_Name then
               --  Convert parenthesis name to array subtype.
               declare
                  N_Actual : Iir;
                  Sub_Assoc : Iir;
                  Indexes : Iir_List;
                  Old : Iir;
               begin
                  N_Actual := Create_Iir (Iir_Kind_Array_Subtype_Definition);
                  Location_Copy (N_Actual, Actual);
                  Set_Subtype_Type_Mark (N_Actual, Get_Prefix (Actual));
                  Sub_Assoc := Get_Association_Chain (Actual);
                  Indexes := Create_Iir_List;
                  while Is_Valid (Sub_Assoc) loop
                     if Get_Kind (Sub_Assoc)
                       /= Iir_Kind_Association_Element_By_Expression
                     then
                        Error_Msg_Sem
                          (+Sub_Assoc, "index constraint must be a range");
                     else
                        if Get_Formal (Sub_Assoc) /= Null_Iir then
                           Error_Msg_Sem
                             (+Sub_Assoc, "formal part not allowed");
                        end if;
                        Append_Element (Indexes, Get_Actual (Sub_Assoc));
                     end if;
                     Old := Sub_Assoc;
                     Sub_Assoc := Get_Chain (Sub_Assoc);
                     Free_Iir (Old);
                  end loop;
                  Old := Actual;
                  Free_Iir (Old);
                  Set_Index_Constraint_List
                    (N_Actual, List_To_Flist (Indexes));
                  Actual := N_Actual;
               end;
            end if;
         when Iir_Kinds_Interface_Subprogram_Declaration =>
            N_Assoc := Create_Iir (Iir_Kind_Association_Element_Subprogram);
            if Get_Kind (Actual) = Iir_Kind_String_Literal8 then
               Actual := Vhdl.Parse.String_To_Operator_Symbol (Actual);
            end if;
         when Iir_Kind_Interface_Terminal_Declaration =>
            N_Assoc := Create_Iir (Iir_Kind_Association_Element_Terminal);
         when others =>
            Error_Kind ("rewrite_non_object_association", Inter);
      end case;
      Location_Copy (N_Assoc, Assoc);
      Set_Formal (N_Assoc, Get_Formal (Assoc));
      Set_Actual (N_Assoc, Actual);
      Set_Chain (N_Assoc, Get_Chain (Assoc));
      Set_Whole_Association_Flag (N_Assoc, True);
      Free_Iir (Assoc);
      return N_Assoc;
   end Rewrite_Non_Object_Association;

   function Extract_Non_Object_Association
     (Assoc_Chain : Iir; Inter_Chain : Iir) return Iir
   is
      Inter : Iir;
      Assoc : Iir;
      --  N_Assoc : Iir;
      Prev_Assoc : Iir;
      Formal : Iir;
      Res : Iir;
   begin
      Inter := Inter_Chain;
      Assoc := Assoc_Chain;
      Prev_Assoc := Null_Iir;
      Res := Null_Iir;

      --  Common case: only objects in interfaces.
      while Is_Valid (Inter) loop
         exit when Get_Kind (Inter)
           not in Iir_Kinds_Interface_Object_Declaration;
         Inter := Get_Chain (Inter);
      end loop;
      if Is_Null (Inter) then
         --  Only interface object, nothing to to.
         return Assoc_Chain;
      end if;

      Inter := Inter_Chain;
      loop
         --  Don't try to detect errors.
         if Is_Null (Assoc) then
            return Res;
         end if;

         Formal := Get_Formal (Assoc);
         if Formal = Null_Iir then
            --  Positional association.

            if Inter = Null_Iir then
               --  But after a named one.  Be silent on that error.
               null;
            elsif Get_Kind (Inter)
              not in Iir_Kinds_Interface_Object_Declaration
            then
               Assoc := Rewrite_Non_Object_Association (Assoc, Inter);
            end if;
         else
            if Kind_In (Formal, Iir_Kind_Simple_Name, Iir_Kind_Operator_Symbol)
            then
               --  A candidate.  Search the corresponding interface.
               Inter := Find_Name_In_Chain
                 (Inter_Chain, Get_Identifier (Formal));
               if Inter /= Null_Iir
                 and then
                 Get_Kind (Inter) not in Iir_Kinds_Interface_Object_Declaration
               then
                  Assoc := Rewrite_Non_Object_Association (Assoc, Inter);
               end if;
            end if;

            --  No more association by position.
            Inter := Null_Iir;
         end if;

         if Prev_Assoc = Null_Iir then
            Res := Assoc;
         else
            Set_Chain (Prev_Assoc, Assoc);
         end if;
         Prev_Assoc := Assoc;
         Assoc := Get_Chain (Assoc);
         if Is_Valid (Inter) then
            Inter := Get_Chain (Inter);
         end if;
      end loop;
   end Extract_Non_Object_Association;

   --  Analyze all arguments of ASSOC_CHAIN
   --  Return TRUE if no error.
   function Sem_Actual_Of_Association_Chain (Assoc_Chain : Iir)
     return Boolean
   is
      Has_Named : Boolean;
      Ok : Boolean;
      Assoc : Iir;
      Res : Iir;
      Formal : Iir;
   begin
      --  Analyze all arguments.
      --  OK is false if there is an error during semantic of one of the
      --  argument, but continue analyze.
      Has_Named := False;
      Ok := True;
      Assoc := Assoc_Chain;
      while Assoc /= Null_Iir loop
         Formal := Get_Formal (Assoc);
         if Formal /= Null_Iir then
            Has_Named := True;
            --  FIXME: check FORMAL is well composed.
         elsif Has_Named then
            --  FIXME: do the check in parser.
            Error_Msg_Sem (+Assoc, "positional argument after named argument");
            Ok := False;
         end if;
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression then
            Res := Sem_Expression_Ov (Get_Actual (Assoc), Null_Iir);
            if Res = Null_Iir then
               Ok := False;
            else
               Set_Actual (Assoc, Res);
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
      return Ok;
   end Sem_Actual_Of_Association_Chain;

   procedure Check_Parameter_Association_Restriction
     (Inter : Iir; Base_Actual : Iir; Loc : Iir) is
   begin
      case Iir_Parameter_Modes (Get_Mode (Inter)) is
         when Iir_In_Mode =>
            if Can_Interface_Be_Read (Base_Actual) then
               return;
            end if;
         when Iir_Out_Mode =>
            if Can_Interface_Be_Updated (Base_Actual) then
               return;
            end if;
         when Iir_Inout_Mode =>
            if Can_Interface_Be_Read (Base_Actual)
              and then Can_Interface_Be_Updated (Base_Actual)
            then
               return;
            end if;
      end case;
      Error_Msg_Sem
        (+Loc, "cannot associate an " & Get_Mode_Name (Get_Mode (Base_Actual))
           & " object with " & Get_Mode_Name (Get_Mode (Inter)) & " %n",
         +Inter);
   end Check_Parameter_Association_Restriction;

   procedure Check_Subprogram_Association_Expression
     (Formal : Iir; Actual : Iir; Assoc : Iir; Loc : Iir)
   is
      Prefix : Iir;
      Object : Iir;
   begin
      if Actual = Null_Iir then
         --  Skip in case of errors.
         return;
      end if;

      Object := Name_To_Object (Actual);
      if Object /= Null_Iir then
         Prefix := Get_Object_Prefix (Object);
      else
         Prefix := Actual;
      end if;

      case Get_Kind (Formal) is
         when Iir_Kind_Interface_Signal_Declaration =>
            --  LRM93 2.1.1
            --  In a subprogram call, the actual designator
            --  associated with a formal parameter of class
            --  signal must be a signal.
            case Get_Kind (Prefix) is
               when Iir_Kind_Interface_Signal_Declaration
                 | Iir_Kind_Signal_Declaration
                 | Iir_Kind_Guard_Signal_Declaration
                 | Iir_Kinds_Signal_Attribute =>
                  --  LRM93 2.1.1.2
                  --  If an actual signal is associated with
                  --  a signal parameter of any mode, the actual
                  --  must be denoted by a static signal name.
                  if Get_Name_Staticness (Object) < Globally then
                     Error_Msg_Sem
                       (+Actual, "actual signal must be a static name");
                  else
                     --  Inherit has_active_flag.
                     Set_Has_Active_Flag
                       (Prefix, Get_Has_Active_Flag (Formal));
                  end if;
               when others =>
                  Error_Msg_Sem
                    (+Loc, "signal parameter requires a signal expression");
            end case;

            case Get_Kind (Prefix) is
               when Iir_Kind_Interface_Signal_Declaration =>
                  Check_Parameter_Association_Restriction
                    (Formal, Prefix, Loc);
               when Iir_Kind_Guard_Signal_Declaration =>
                  if Get_Mode (Formal) /= Iir_In_Mode then
                     Error_Msg_Sem
                       (+Loc, "cannot associate a guard signal with "
                          & Get_Mode_Name (Get_Mode (Formal))
                          & " %n", +Formal);
                  end if;
               when Iir_Kinds_Signal_Attribute =>
                  if Get_Mode (Formal) /= Iir_In_Mode then
                     Error_Msg_Sem
                       (+Loc, "cannot associate a signal attribute with "
                          & Get_Mode_Name (Get_Mode (Formal))
                          & " %n", +Formal);
                  end if;
               when others =>
                  null;
            end case;

            --  LRM 2.1.1.2  Signal parameters
            --  It is an error if a conversion function or type
            --  conversion appears in either the formal part or the
            --  actual part of an association element that associates
            --  an actual signal with a formal signal parameter.
            if Assoc /= Null_Iir
              and then (Get_Actual_Conversion (Assoc) /= Null_Iir
                          or Get_Formal_Conversion (Assoc) /= Null_Iir)
            then
               Error_Msg_Sem
                 (+Assoc, "conversion are not allowed for signal parameters");
            end if;
         when Iir_Kind_Interface_Variable_Declaration =>
            --  LRM93 2.1.1
            --  The actual designator associated with a formal of
            --  class variable must be a variable.
            case Get_Kind (Prefix) is
               when Iir_Kind_Interface_Variable_Declaration =>
                  Check_Parameter_Association_Restriction
                    (Formal, Prefix, Loc);
               when Iir_Kind_Variable_Declaration
                 | Iir_Kind_Dereference
                 | Iir_Kind_Implicit_Dereference =>
                  null;
               when Iir_Kind_Interface_File_Declaration
                 | Iir_Kind_File_Declaration =>
                  --  LRM87 4.3.1.4
                  --  Such an object is a member of the variable
                  --  class of objects;
                  if Flags.Vhdl_Std >= Vhdl_93
                    and then not Flags.Flag_Relaxed_Files87
                  then
                     Error_Msg_Sem
                       (+Loc, "variable parameter cannot be a file (vhdl93)");
                  end if;
               when others =>
                  Error_Msg_Sem
                    (+Loc, "variable parameter must be a variable");
            end case;
         when Iir_Kind_Interface_File_Declaration =>
            --  LRM93 2.1.1
            --  The actual designator associated with a formal
            --  of class file must be a file.
            case Get_Kind (Prefix) is
               when Iir_Kind_Interface_File_Declaration
                 | Iir_Kind_File_Declaration =>
                  null;
               when Iir_Kind_Variable_Declaration
                 | Iir_Kind_Interface_Variable_Declaration =>
                  if Flags.Vhdl_Std >= Vhdl_93
                    and then not Flags.Flag_Relaxed_Files87
                  then
                     Error_Msg_Sem
                       (+Loc, "file parameter must be a file (vhdl93)");
                  end if;
               when others =>
                  Error_Msg_Sem (+Loc, "file parameter must be a file");
            end case;

            --  LRM 2.1.1.3  File parameters
            --  It is an error if an association element associates
            --  an actual with a formal parameter of a file type and
            --  that association element contains a conversion
            --  function or type conversion.
            if Assoc /= Null_Iir
              and then (Get_Actual_Conversion (Assoc) /= Null_Iir
                          or Get_Formal_Conversion (Assoc) /= Null_Iir)
            then
               Error_Msg_Sem (+Assoc, "conversion are not allowed "
                                & "for file parameters");
            end if;
         when Iir_Kind_Interface_Constant_Declaration =>
            --  LRM93 2.1.1
            --  The actual designator associated with a formal of
            --  class constant must be an expression.
            --  GHDL: unless this is in a formal_part.
            if Assoc = Null_Iir or else not Get_In_Formal_Flag (Assoc) then
               Check_Read (Actual);
            end if;
         when others =>
            Error_Kind ("check_subprogram_association_expression", Formal);
      end case;

      case Get_Kind (Prefix) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration =>
            Set_Use_Flag (Prefix, True);
         when others =>
            null;
      end case;
   end Check_Subprogram_Association_Expression;

   procedure Check_Subprogram_Associations
     (Inter_Chain : Iir; Assoc_Chain : Iir)
   is
      Assoc : Iir;
      Formal_Inter : Iir;
      Actual : Iir;
      Inter : Iir;
   begin
      Assoc := Assoc_Chain;
      Inter := Inter_Chain;
      while Assoc /= Null_Iir loop
         Formal_Inter := Get_Association_Interface (Assoc, Inter);
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               if Get_Default_Value (Formal_Inter) = Null_Iir then
                  Error_Msg_Sem (+Assoc, "no parameter for %n", +Formal_Inter);
               end if;
            when Iir_Kind_Association_Element_By_Expression =>
               Actual := Get_Actual (Assoc);
               Check_Subprogram_Association_Expression
                 (Formal_Inter, Actual, Assoc, Assoc);
            when Iir_Kind_Association_Element_By_Individual =>
               null;
            when others =>
               Error_Kind ("check_subprogram_associations", Assoc);
         end case;
         Next_Association_Interface (Assoc, Inter);
      end loop;
   end Check_Subprogram_Associations;

   --  Assocs_Right_Map (FORMAL_MODE, ACTUAL_MODE) is true iff it is allowed
   --  to associate a formal port of mode FORMAL_MODE with an actual port of
   --  mode ACTUAL_MODE.
   subtype Iir_Known_Mode is Iir_Mode range Iir_Linkage_Mode .. Iir_In_Mode;
   type Assocs_Right_Map is array (Iir_Known_Mode, Iir_Known_Mode) of Boolean;

   --  LRM93 1.1.1.2 Ports
   Vhdl93_Assocs_Map : constant Assocs_Right_Map :=
     (Iir_In_Mode =>
        (Iir_In_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Out_Mode =>
        (Iir_Out_Mode | Iir_Inout_Mode => True,
         others => False),
      Iir_Inout_Mode =>
        (Iir_Inout_Mode => True,
         others => False),
      Iir_Buffer_Mode =>
        (Iir_Buffer_Mode => True, others => False),
      Iir_Linkage_Mode =>
        (others => True));

   --  LRM02 1.1.1.2 Ports
   Vhdl02_Assocs_Map : constant Assocs_Right_Map :=
     (Iir_In_Mode =>
        (Iir_In_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Out_Mode =>
        (Iir_Out_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Inout_Mode =>
        (Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Buffer_Mode =>
        (Iir_Out_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Linkage_Mode =>
        (others => True));

   --  LRM08 6.5.6.3 Port clauses
   Vhdl08_Assocs_Map : constant Assocs_Right_Map :=
     (Iir_In_Mode =>
        (Iir_In_Mode | Iir_Out_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Out_Mode =>
        (Iir_Out_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Inout_Mode =>
        (Iir_Out_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Buffer_Mode =>
        (Iir_Out_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
         others => False),
      Iir_Linkage_Mode => (others => True));

   --  Check for restrictions in LRM 1.1.1.2
   --  Return FALSE in case of error.
   function Check_Port_Association_Mode_Restrictions
     (Formal : Iir_Interface_Signal_Declaration;
      Actual : Iir_Interface_Signal_Declaration;
      Assoc : Iir)
     return Boolean
   is
      Fmode : constant Iir_Mode := Get_Mode (Formal);
      Amode : constant Iir_Mode := Get_Mode (Actual);
   begin
      pragma Assert (Fmode /= Iir_Unknown_Mode);
      pragma Assert (Amode /= Iir_Unknown_Mode);

      case Flags.Vhdl_Std is
         when Vhdl_87 | Vhdl_93 | Vhdl_00 =>
            if Vhdl93_Assocs_Map (Fmode, Amode) then
               return True;
            end if;
         when Vhdl_02 =>
            if Vhdl02_Assocs_Map (Fmode, Amode) then
               return True;
            end if;
         when Vhdl_08 | Vhdl_19 =>
            if Vhdl08_Assocs_Map (Fmode, Amode) then
               return True;
            end if;
      end case;

      if Assoc /= Null_Iir then
         Error_Msg_Sem
           (+Assoc, "cannot associate " & Get_Mode_Name (Fmode) & " %n"
              & " with actual port of mode "
              & Get_Mode_Name (Amode), +Formal);
      end if;
      return False;
   end Check_Port_Association_Mode_Restrictions;

   --  Check restrictions of LRM02 12.2.4
   procedure Check_Port_Association_Bounds_Restrictions
     (Formal : Iir; Actual : Iir; Assoc : Iir)
   is
      Inter : constant Iir := Get_Object_Prefix (Formal, False);

      function Is_Scalar_Type_Compatible (Src : Iir; Dest : Iir)
                                         return Boolean
      is
         Src_Range : Iir;
         Dst_Range : Iir;
      begin
         if Get_Kind (Src) not in Iir_Kinds_Scalar_Type_And_Subtype_Definition
         then
            --  It's only for scalar types.
            return True;
         end if;
         if Get_Kind (Dest) not in Iir_Kinds_Scalar_Type_And_Subtype_Definition
         then
            --  Avoid a crash in case of errors.
            return True;
         end if;
         if Get_Kind (Src) = Iir_Kind_Foreign_Vector_Type_Definition
           or else Get_Kind (Dest) = Iir_Kind_Foreign_Vector_Type_Definition
         then
            return True;
         end if;

         Src_Range := Get_Range_Constraint (Src);
         Dst_Range := Get_Range_Constraint (Dest);
         if Get_Expr_Staticness (Src_Range) /= Locally
           or else Get_Expr_Staticness (Dst_Range) /= Locally
         then
            return True;
         end if;

         --  FIXME: non-static bounds have to be checked at run-time
         --  (during elaboration).

         --  In vhdl08, the subtypes must be compatible.  Use the that rule
         --  for relaxed rules.
         if Vhdl_Std >= Vhdl_08
           or else Flag_Relaxed_Rules
         then
            return Eval_Is_Range_In_Bound (Src, Dest, True);
         end if;

         --  Prior vhdl08, the subtypes must be identical.
         if not Eval_Is_Eq (Get_Left_Limit (Src_Range),
                            Get_Left_Limit (Dst_Range))
           or else not Eval_Is_Eq (Get_Right_Limit (Src_Range),
                                      Get_Right_Limit (Dst_Range))
           or else Get_Direction (Src_Range) /= Get_Direction (Dst_Range)
         then
            return False;
         end if;

         return True;
      end Is_Scalar_Type_Compatible;

      procedure Error_Msg
      is
         Id : Msgid_Type;
         Orig : Report_Origin;
      begin
         if Flag_Elaborate then
            Id := Msgid_Error;
            Orig := Elaboration;
         else
            Id := Warnid_Port_Bounds;
            Orig := Semantic;
         end if;
         Report_Msg
           (Id, Orig, +Assoc,
            "bounds or direction of actual don't match with %n",
            (1 => +Inter));
      end Error_Msg;

      Ftype : constant Iir := Get_Type (Formal);
      Atype : constant Iir := Get_Type (Actual);
      F_Conv : constant Iir := Get_Formal_Conversion (Assoc);
      A_Conv : constant Iir := Get_Actual_Conversion (Assoc);
      F2a_Type : Iir;
      A2f_Type : Iir;
   begin
      --  LRM02 12.2.4 The port map aspect
      --  If an actual signal is associated with a port of any mode, and if
      --  the type of the formal is a scalar type, then it is an error if
      --  (after applying any conversion function or type conversion
      --  expression present in the actual part) the bounds and direction of
      --  the subtype denoted by the subtype indication of the formal are not
      --  identical to the bounds and direction of the subtype denoted by the
      --  subtype indication of the actual.

      --  LRM08 14.3.5 Port map aspect
      --  If an actual signal is associated with a port of mode IN or INOUT,
      --  and if the type of the formal is a scalar type, then it is an error
      --  if (after applying any conversion function or type conversion
      --  expression present in the actual part) the subtype of the actual is
      --  not compatible with the subtype of the formal.  [...]
      --
      --  Similarly, if an actual signal is associated with a port of mode
      --  OUT, INOUT, or BUFFER, and the type of the actual is a scalar type,
      --  then it is an error if (after applying any conversion function or
      --  type conversion expression present in the formal part) the subtype
      --  or the formal is not compatible with the subtype of the actual.
      if Is_Valid (F_Conv) then
         F2a_Type := Get_Type (F_Conv);
      else
         F2a_Type := Ftype;
      end if;
      if Is_Valid (A_Conv) then
         A2f_Type := Get_Type (A_Conv);
      else
         A2f_Type := Atype;
      end if;
      if Get_Mode (Inter) in Iir_In_Modes
        and then not Is_Scalar_Type_Compatible (A2f_Type, Ftype)
      then
         Error_Msg;
      end if;
      if Get_Mode (Inter) in Iir_Out_Modes
        and then not Is_Scalar_Type_Compatible (F2a_Type, Atype)
      then
         Error_Msg;
      end if;
   end Check_Port_Association_Bounds_Restrictions;

   --  Handle indexed name
   --  FORMAL is the formal name to be handled.
   --  BASE_ASSOC is an association_by_individual in which the formal will be
   --   inserted.
   procedure Add_Individual_Assoc_Indexed_Name
     (Choice : out Iir; Base_Assoc : Iir; Formal : Iir)
   is
      Index_List : constant Iir_Flist := Get_Index_List (Formal);
      Nbr : constant Natural := Get_Nbr_Elements (Index_List);
      Last_Choice : Iir;
      Index : Iir;
      Staticness : Iir_Staticness;
      Sub_Assoc : Iir;
   begin
      --  Find element.
      Sub_Assoc := Base_Assoc;
      for I in 0 .. Nbr - 1 loop
         Index := Get_Nth_Element (Index_List, I);

         --  Evaluate index.
         Staticness := Get_Expr_Staticness (Index);
         if Staticness = Locally then
            Index := Eval_Expr (Index);
            Set_Nth_Element (Index_List, I, Index);
         else
            Error_Msg_Sem (+Index, "index expression must be locally static");
            Set_Choice_Staticness (Base_Assoc, None);
         end if;

         --  Find index in choice list.
         Last_Choice := Null_Iir;
         Choice := Get_Individual_Association_Chain (Sub_Assoc);
         while Choice /= Null_Iir loop
            case Get_Kind (Choice) is
               when Iir_Kind_Choice_By_Expression =>
                  if Eval_Pos (Get_Choice_Expression (Choice))
                    = Eval_Pos (Index)
                  then
                     goto Found;
                  end if;
               when Iir_Kind_Choice_By_Range =>
                  declare
                     Choice_Range : constant Iir := Get_Choice_Range (Choice);
                  begin
                     if Get_Expr_Staticness (Choice_Range) = Locally
                       and then
                       Eval_Int_In_Range (Eval_Pos (Index), Choice_Range)
                     then
                        --  FIXME: overlap.
                        raise Internal_Error;
                     end if;
                  end;
               when others =>
                  Error_Kind ("add_individual_assoc_index_name", Choice);
            end case;
            Last_Choice := Choice;
            Choice := Get_Chain (Choice);
         end loop;

         --  If not found, append it.
         Choice := Create_Iir (Iir_Kind_Choice_By_Expression);
         Set_Choice_Expression (Choice, Index);
         Set_Choice_Staticness (Choice, Staticness);
         Location_Copy (Choice, Formal);
         if Last_Choice = Null_Iir then
            Set_Individual_Association_Chain (Sub_Assoc, Choice);
         else
            Set_Chain (Last_Choice, Choice);
         end if;

         << Found >> null;

         if I < Nbr - 1 then
            --  Create an intermediate assoc by individual.
            Sub_Assoc := Get_Associated_Expr (Choice);
            if Sub_Assoc = Null_Iir then
               Sub_Assoc := Create_Iir
                 (Iir_Kind_Association_Element_By_Individual);
               Location_Copy (Sub_Assoc, Index);
               Set_Associated_Expr (Choice, Sub_Assoc);
               Set_Choice_Staticness (Sub_Assoc, Locally);
            end if;
         end if;
      end loop;
   end Add_Individual_Assoc_Indexed_Name;

   procedure Add_Individual_Assoc_Slice_Name
     (Choice : out Iir; Sub_Assoc : Iir; Formal : Iir)
   is
      Index : Iir;
      Staticness : Iir_Staticness;
   begin
      --  FIXME: handle cases such as param(5 to 6)(5)

      --  Find element.
      Index := Get_Suffix (Formal);

      --  Evaluate index.
      Staticness := Get_Expr_Staticness (Index);
      if Staticness = Locally then
         Index := Eval_Range (Index);
         Set_Suffix (Formal, Index);
      else
         Error_Msg_Sem (+Index, "range expression must be locally static");
         Set_Choice_Staticness (Sub_Assoc, None);
      end if;

      Choice := Create_Iir (Iir_Kind_Choice_By_Range);
      Location_Copy (Choice, Formal);
      Set_Choice_Range (Choice, Index);
      Set_Chain (Choice, Get_Individual_Association_Chain (Sub_Assoc));
      Set_Choice_Staticness (Choice, Staticness);
      Set_Individual_Association_Chain (Sub_Assoc, Choice);
   end Add_Individual_Assoc_Slice_Name;

   procedure Add_Individual_Assoc_Selected_Name
     (Choice : out Iir; Sub_Assoc : Iir; Formal : Iir)
   is
      Element : constant Iir := Get_Named_Entity (Formal);
      Last_Choice : Iir;
   begin
      --  Try to find the existing choice.
      Last_Choice := Null_Iir;
      Choice := Get_Individual_Association_Chain (Sub_Assoc);
      while Choice /= Null_Iir loop
         if Get_Choice_Name (Choice) = Element then
            return;
         end if;
         Last_Choice := Choice;
         Choice := Get_Chain (Choice);
      end loop;

      --  If not found, append it.
      Choice := Create_Iir (Iir_Kind_Choice_By_Name);
      Location_Copy (Choice, Formal);
      Set_Choice_Name (Choice, Element);
      if Last_Choice = Null_Iir then
         Set_Individual_Association_Chain (Sub_Assoc, Choice);
      else
         Set_Chain (Last_Choice, Choice);
      end if;
   end Add_Individual_Assoc_Selected_Name;

   --  Subroutine of Add_Individual_Association.
   --  Search/build the tree of choices for FORMAL, starting for IASSOC.
   --  The root of the tree is an association by individual node.  Each node
   --  points to a chain of choices, whose associated expression is either an
   --  association by individual (and the tree continue) or an association
   --  by expression coming from the initial association (and this is a leaf).
   procedure Add_Individual_Association_1
     (Iassoc : in out Iir; Formal : Iir; Last : Boolean)
   is
      Base_Assoc : constant Iir := Iassoc;
      Formal_Object : constant Iir := Name_To_Object (Formal);
      Sub : Iir;
      Choice : Iir;
   begin
      pragma Assert
        (Get_Kind (Iassoc) = Iir_Kind_Association_Element_By_Individual);

      --  Recurse to start from the basename of the formal.
      case Get_Kind (Formal_Object) is
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Element =>
            Add_Individual_Association_1
              (Iassoc, Get_Prefix (Formal_Object), False);
         when Iir_Kinds_Interface_Object_Declaration =>
            --  At the root of the formal.
            pragma Assert
              (Formal_Object = Get_Named_Entity (Get_Formal (Iassoc)));
            return;
         when others =>
            Error_Kind ("add_individual_association_1", Formal);
      end case;

      --  Add the choices for the indexes/slice/element.
      case Get_Kind (Formal_Object) is
         when Iir_Kind_Indexed_Name =>
            Add_Individual_Assoc_Indexed_Name (Choice, Iassoc, Formal_Object);
         when Iir_Kind_Slice_Name =>
            Add_Individual_Assoc_Slice_Name (Choice, Iassoc, Formal_Object);
         when Iir_Kind_Selected_Element =>
            Add_Individual_Assoc_Selected_Name (Choice, Iassoc, Formal_Object);
         when others =>
            Error_Kind ("add_individual_association_1(3)", Formal);
      end case;

      Sub := Get_Associated_Expr (Choice);
      if Sub = Null_Iir then
         if not Last then
            --  Create the individual association for the choice.
            Sub := Create_Iir (Iir_Kind_Association_Element_By_Individual);
            Location_Copy (Sub, Formal);
            Set_Choice_Staticness (Sub, Locally);
            Set_Formal (Sub, Formal);
            Set_Associated_Expr (Choice, Sub);
         end if;
      else
         if Last
           or else Get_Kind (Sub) /= Iir_Kind_Association_Element_By_Individual
         then
            --  A final association.
            pragma Assert
              (Get_Kind (Sub) = Iir_Kind_Association_Element_By_Expression);
            Error_Msg_Sem
              (+Formal, "individual association of %n"
                 & " conflicts with that at %l",
               (+Get_Interface_Of_Formal (Get_Formal (Iassoc)),
                +Sub));
         else
            if Get_Choice_Staticness (Sub) /= Locally then
               --  Propagate error.
               Set_Choice_Staticness (Base_Assoc, None);
            end if;
         end if;
      end if;

      if Last then
         Iassoc := Choice;
      else
         Iassoc := Sub;
      end if;
   end Add_Individual_Association_1;

   --  Insert ASSOC into the tree of individual assoc rooted by IASSOC.
   --  This is done so that duplicate or missing associations are found (using
   --  the same routine for aggregate/case statement).
   procedure Add_Individual_Association (Iassoc : Iir; Assoc : Iir)
   is
      Formal : constant Iir := Get_Formal (Assoc);
      Res_Iass : Iir;
      Prev : Iir;
   begin
      --  Create the individual association for the formal.
      Res_Iass := Iassoc;
      Add_Individual_Association_1 (Res_Iass, Formal, True);

      Prev := Get_Associated_Expr (Res_Iass);
      if Prev = Null_Iir then
         --  It is the first one, add it.
         Set_Associated_Expr (Res_Iass, Assoc);
      end if;
   end Add_Individual_Association;

   procedure Finish_Individual_Association1 (Assoc : Iir; Atype : Iir);

   procedure Finish_Individual_Assoc_Array_Subtype
     (Assoc : Iir; Atype : Iir; Dim : Positive)
   is
      Index_Tlist : constant Iir_Flist := Get_Index_Subtype_List (Atype);
      Nbr_Dims : constant Natural := Get_Nbr_Elements (Index_Tlist);
      Index_Type : constant Iir := Get_Nth_Element (Index_Tlist, Dim - 1);
      Chain : constant Iir := Get_Individual_Association_Chain (Assoc);
      Low, High : Iir;
      El_Type : Iir;
      El : Iir;
   begin
      Sem_Check_Continuous_Choices
        (Chain, Index_Type, Low, High, Get_Location (Assoc), False);
      if Dim < Nbr_Dims then
         El := Chain;
         while El /= Null_Iir loop
            pragma Assert (Get_Kind (El) = Iir_Kind_Choice_By_Expression);
            Finish_Individual_Assoc_Array_Subtype
              (Get_Associated_Expr (El), Atype, Dim + 1);
            El := Get_Chain (El);
         end loop;
      else
         El_Type := Get_Element_Subtype (Atype);
         El := Chain;
         while El /= Null_Iir loop
            Finish_Individual_Association1 (Get_Associated_Expr (El), El_Type);
            El := Get_Chain (El);
         end loop;
      end if;
   end Finish_Individual_Assoc_Array_Subtype;

   procedure Finish_Individual_Assoc_Array
     (Actual : Iir; Assoc : Iir; Dim : Natural)
   is
      Actual_Type : constant Iir := Get_Actual_Type (Actual);
      Index_Tlist : constant Iir_Flist := Get_Index_Subtype_List (Actual_Type);
      Actual_Index : Iir;
      Base_Type : Iir;
      Base_Index : Iir;
      Low, High : Iir;
      Chain : Iir;
   begin
      Actual_Index := Get_Nth_Element (Index_Tlist, Dim - 1);
      if Actual_Index /= Null_Iir then
         Base_Index := Actual_Index;
      else
         Base_Type := Get_Base_Type (Actual_Type);
         Base_Index := Get_Index_Type (Base_Type, Dim - 1);
      end if;
      Chain := Get_Individual_Association_Chain (Assoc);
      Sem_Choices_Range
        (Chain, Base_Index, Low, High, Get_Location (Assoc), True, False);
      Set_Individual_Association_Chain (Assoc, Chain);
      if Actual_Index = Null_Iir then
         declare
            Index_Constraint : Iir;
            Index_Subtype_Constraint : Iir;
         begin
            --  Create an index subtype.
            case Get_Kind (Base_Index) is
               when Iir_Kind_Integer_Subtype_Definition =>
                  Actual_Index :=
                    Create_Iir (Iir_Kind_Integer_Subtype_Definition);
               when Iir_Kind_Enumeration_Type_Definition
                 | Iir_Kind_Enumeration_Subtype_Definition =>
                  Actual_Index :=
                    Create_Iir (Iir_Kind_Enumeration_Subtype_Definition);
               when others =>
                  Error_Kind ("finish_individual_assoc_array", Base_Index);
            end case;
            Location_Copy (Actual_Index, Actual);
            Set_Parent_Type (Actual_Index, Base_Index);
            Index_Constraint := Get_Range_Constraint (Base_Index);

            Index_Subtype_Constraint := Create_Iir (Iir_Kind_Range_Expression);
            Location_Copy (Index_Subtype_Constraint, Actual);
            Set_Range_Constraint (Actual_Index, Index_Subtype_Constraint);
            Set_Type_Staticness (Actual_Index, Locally);
            Set_Direction (Index_Subtype_Constraint,
                           Get_Direction (Index_Constraint));

            --  For ownership purpose, the bounds must be copied otherwise
            --  they would be referenced before being defined.  This is non
            --  optimal but it doesn't happen often.
            Low := Copy_Constant (Low);
            High := Copy_Constant (High);

            case Get_Direction (Index_Constraint) is
               when Dir_To =>
                  Set_Left_Limit (Index_Subtype_Constraint, Low);
                  Set_Left_Limit_Expr (Index_Subtype_Constraint, Low);
                  Set_Right_Limit (Index_Subtype_Constraint, High);
                  Set_Right_Limit_Expr (Index_Subtype_Constraint, High);
               when Dir_Downto =>
                  Set_Left_Limit (Index_Subtype_Constraint, High);
                  Set_Left_Limit_Expr (Index_Subtype_Constraint, High);
                  Set_Right_Limit (Index_Subtype_Constraint, Low);
                  Set_Right_Limit_Expr (Index_Subtype_Constraint, Low);
            end case;
            Set_Expr_Staticness (Index_Subtype_Constraint, Locally);
            Set_Nth_Element (Get_Index_Subtype_List (Actual_Type), Dim - 1,
                             Actual_Index);
         end;
      else
         declare
            Act_High, Act_Low : Iir;
         begin
            Get_Low_High_Limit (Get_Range_Constraint (Actual_Type),
                                Act_Low, Act_High);
            if Eval_Pos (Act_Low) /= Eval_Pos (Low)
              or Eval_Pos (Act_High) /= Eval_Pos (High)
            then
               Error_Msg_Sem
                 (+Assoc, "indexes of individual association mismatch");
            end if;
         end;
      end if;

      declare
         Nbr_Dims : constant Natural := Get_Nbr_Elements (Index_Tlist);
         El_Type : Iir;
         El : Iir;
      begin
         if Dim = Nbr_Dims then
            El_Type := Get_Element_Subtype (Actual_Type);
            El := Chain;
            while El /= Null_Iir loop
               Finish_Individual_Association1
                 (Get_Associated_Expr (El), El_Type);
               El := Get_Chain (El);
            end loop;
         end if;
      end;
   end Finish_Individual_Assoc_Array;

   procedure Finish_Individual_Assoc_Record (Assoc : Iir; Atype : Iir)
   is
      El_List : constant Iir_Flist := Get_Elements_Declaration_List (Atype);
      Nbr_El : constant Natural := Get_Nbr_Elements (El_List);
      Matches : Iir_Array (0 .. Nbr_El - 1);
      Ch : Iir;
      Pos : Natural;
      Rec_El : Iir;
   begin
      --  Check for duplicate associations.
      Matches := (others => Null_Iir);
      Ch := Get_Individual_Association_Chain (Assoc);
      while Ch /= Null_Iir loop
         Rec_El := Get_Choice_Name (Ch);
         Pos := Natural (Get_Element_Position (Rec_El));
         if Matches (Pos) /= Null_Iir then
            Error_Msg_Sem (+Ch, "individual %n already associated at %l",
                           (+Rec_El, +Matches (Pos)));
         else
            Matches (Pos) := Ch;
         end if;
         Ch := Get_Chain (Ch);
      end loop;

      --  Check for missing associations.
      for I in Matches'Range loop
         Rec_El := Get_Nth_Element (El_List, I);
         if Matches (I) = Null_Iir then
            Error_Msg_Sem (+Assoc, "%n not associated", +Rec_El);
         else
            Finish_Individual_Association1
              (Get_Associated_Expr (Matches (I)), Get_Type (Rec_El));
         end if;
      end loop;

      if Get_Constraint_State (Atype) /= Fully_Constrained then
         --  Some (sub-)elements are unbounded, create a bounded subtype.
         declare
            Inter : constant Iir :=
              Get_Interface_Of_Formal (Get_Formal (Assoc));
            Ntype       : Iir;
            Nel_List    : Iir_Flist;
            Nrec_El     : Iir;
            Rec_El_Type : Iir;
            Staticness  : Iir_Staticness;
            Assoc_Expr  : Iir;
            Assoc_Type  : Iir;
         begin
            Ntype := Create_Iir (Iir_Kind_Record_Subtype_Definition);
            Set_Is_Ref (Ntype, True);
            Location_Copy (Ntype, Assoc);
            Set_Parent_Type (Ntype, Atype);
            if Get_Kind (Atype) = Iir_Kind_Record_Subtype_Definition then
               Set_Resolution_Indication
                 (Ntype, Get_Resolution_Indication (Atype));
            end if;
            if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration
            then
               --  The subtype is used for signals.
               Set_Has_Signal_Flag (Ntype, True);
            end if;

            Nel_List := Create_Iir_Flist (Nbr_El);
            Set_Elements_Declaration_List (Ntype, Nel_List);

            Staticness := Locally;
            for I in Matches'Range loop
               Rec_El := Get_Nth_Element (El_List, I);
               Rec_El_Type := Get_Type (Rec_El);
               if (Get_Kind (Rec_El_Type)
                     not in Iir_Kinds_Composite_Type_Definition)
                 or else
                 Get_Constraint_State (Rec_El_Type) = Fully_Constrained
                 or else
                 Matches (I) = Null_Iir  --  In case of error.
               then
                  Nrec_El := Rec_El;
               else
                  Nrec_El := Create_Iir (Iir_Kind_Record_Element_Constraint);
                  Ch := Matches (I);
                  Location_Copy (Nrec_El, Ch);
                  Set_Parent (Nrec_El, Ntype);
                  Set_Identifier (Nrec_El, Get_Identifier (Rec_El));
                  pragma Assert (I = Natural (Get_Element_Position (Rec_El)));
                  Set_Element_Position (Nrec_El, Iir_Index32 (I));
                  Assoc_Expr := Get_Associated_Expr (Ch);
                  if (Get_Kind (Assoc_Expr)
                      = Iir_Kind_Association_Element_By_Individual)
                  then
                     Assoc_Type := Get_Actual_Type (Assoc_Expr);
                     Set_Subtype_Indication (Nrec_El, Assoc_Type);
                  else
                     Assoc_Type := Get_Type (Get_Actual (Assoc_Expr));
                  end if;
                  Set_Type (Nrec_El, Assoc_Type);
                  Append_Owned_Element_Constraint (Ntype, Nrec_El);
               end if;
               Staticness := Min (Staticness,
                                  Get_Type_Staticness (Get_Type (Nrec_El)));
               Set_Nth_Element (Nel_List, I, Nrec_El);
            end loop;
            Set_Type_Staticness (Ntype, Staticness);
            Set_Constraint_State (Ntype, Fully_Constrained);

            Set_Actual_Type (Assoc, Ntype);
            Set_Actual_Type_Definition (Assoc, Ntype);
         end;
      else
         Set_Actual_Type (Assoc, Atype);
      end if;
   end Finish_Individual_Assoc_Record;

   --  Free recursively all the choices of ASSOC.  Once the type is computed
   --  this is not needed anymore.
   procedure Clean_Individual_Association (Assoc : Iir)
   is
      El, N_El : Iir;
      Expr : Iir;
   begin
      El := Get_Individual_Association_Chain (Assoc);
      Set_Individual_Association_Chain (Assoc, Null_Iir);

      while Is_Valid (El) loop
         N_El := Get_Chain (El);

         pragma Assert (Get_Kind (El) in Iir_Kinds_Choice);
         Expr := Get_Associated_Expr (El);
         if Get_Kind (Expr) = Iir_Kind_Association_Element_By_Individual then
            Clean_Individual_Association (Expr);
            Free_Iir (Expr);
         end if;

         Free_Iir (El);
         El := N_El;
      end loop;
   end Clean_Individual_Association;

   procedure Finish_Individual_Association1 (Assoc : Iir; Atype : Iir)
   is
      Ntype : Iir;
   begin
      if Get_Kind (Assoc) /= Iir_Kind_Association_Element_By_Individual then
         --  End of recursion.  The association is an element association,
         --  not an individual one.
         return;
      end if;

      case Get_Kind (Atype) is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition =>
            if Get_Constraint_State (Atype) = Fully_Constrained then
               Finish_Individual_Assoc_Array_Subtype (Assoc, Atype, 1);
               Set_Actual_Type (Assoc, Atype);
            else
               Ntype := Create_Array_Subtype (Atype, Get_Location (Assoc));
               Set_Index_Constraint_Flag (Ntype, True);
               Set_Constraint_State (Ntype, Fully_Constrained);
               Set_Has_Signal_Flag (Ntype, Get_Has_Signal_Flag (Atype));
               Set_Actual_Type (Assoc, Ntype);
               Set_Actual_Type_Definition (Assoc, Ntype);
               Finish_Individual_Assoc_Array (Assoc, Assoc, 1);
            end if;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            Finish_Individual_Assoc_Record (Assoc, Atype);
         when Iir_Kinds_Scalar_Type_And_Subtype_Definition =>
            null;
         when others =>
            Error_Kind ("finish_individual_association", Atype);
      end case;
   end Finish_Individual_Association1;

   --  Called by sem_individual_association to finish the analyze of
   --  individual association ASSOC: compute bounds, detect missing elements.
   procedure Finish_Individual_Association (Assoc : Iir)
   is
      Inter : Iir;
      Atype : Iir;
   begin
      --  Guard.
      if Get_Choice_Staticness (Assoc) /= Locally then
         return;
      end if;

      Inter := Get_Interface_Of_Formal (Get_Formal (Assoc));
      Atype := Get_Type (Inter);
      Set_Whole_Association_Flag (Assoc, True);

      Finish_Individual_Association1 (Assoc, Atype);

      --  Free the hierarchy, keep only the top individual association.
      Clean_Individual_Association (Assoc);
   end Finish_Individual_Association;

   --  Analyze all individual associations of ASSOCS:
   --  Create an Iir_Kind_Association_Element_By_Individual node before each
   --  group of individual association for the same formal, call
   --  and call Add_Individual_Association for each individual association for
   --  the same formal, and finally call Finish_Individual_Association at the
   --  end of each group.
   --
   --  The purpose of By_Individual association is to have the type of the
   --  actual (might be an array subtype), and also to be sure that all
   --  sub-elements are associated.  For that a tree is created.  The tree is
   --  rooted by the top Association_Element_By_Individual, which contains a
   --  chain of choices (like the aggregate).  The child of a choice is either
   --  an Association_Element written by the user, or a new subtree rooted
   --  by another Association_Element_By_Individual.
   --
   --  Eg:
   --    formal (1, 0).ela => act1,
   --    formal (1, 0).elb => act2,
   --    formal (1, 1)     => act3,
   --    formal (2, 0)     => act4,
   --    formal (2, 1)     => act5,
   --
   --  Association_Element_By_Individual (Root)
   --  +- Choice_By_Expression (1)
   --  |  +- Association_Element_By_Individual
   --  |     +- Choice_By_Expression (0)
   --  |     |  +- Association_Element_By_Individual
   --  |     |     +- Choice_By_Name (ela)
   --  |     |        +- Association_Element_By_Expression (act1)
   --  |     |     +- Choice_By_Name (elb)
   --  |     |        +- Association_Element_By_Expression (act2)
   --  |     +- Choice_By_Expression (1)
   --  |        +- Association_Element_By_Expression (act3)
   --  +- Choice_By_Expression (2)
   --     +- Association_Element_By_Individual
   --        +- Choice_By_Expression (0)
   --        |  +- Association_Element_By_Expression (act4)
   --        +- Choice_By_Expression (1)
   --           +- Association_Element_By_Expression (act5)
   --
   --  The tree doesn't follow all the ownership rules: the formal of
   --  sub association_element are directly set to the association,
   --  and the associated_expr of the choices are directly set to
   --  formals.
   --
   --  This tree is temporary (used only during analysis of the individual
   --  association) and removed once the check is done.
   procedure Sem_Individual_Association (Assoc_Chain : in out Iir)
   is
      Assoc : Iir;
      Prev_Assoc : Iir;
      Iassoc : Iir_Association_Element_By_Individual;
      Cur_Iface : Iir;
      Formal : Iir;
   begin
      Iassoc := Null_Iir;
      Cur_Iface := Null_Iir;
      Prev_Assoc := Null_Iir;
      Assoc := Assoc_Chain;
      while Assoc /= Null_Iir loop
         Formal := Get_Formal (Assoc);
         if Formal /= Null_Iir then
            Formal := Get_Object_Prefix (Formal);
         end if;
         if Formal = Null_Iir or else Formal /= Cur_Iface then
            --  New formal name, analyze the current individual association
            --  (if any).
            if Iassoc /= Null_Iir then
               Finish_Individual_Association (Iassoc);
            end if;
            Cur_Iface := Formal;
            Iassoc := Null_Iir;
         end if;

         if Get_Whole_Association_Flag (Assoc) = False then
            --  Individual association.
            if Iassoc = Null_Iir then
               --  The first one for the interface: create a new individual
               --  association.
               Iassoc :=
                 Create_Iir (Iir_Kind_Association_Element_By_Individual);
               Location_Copy (Iassoc, Assoc);
               Set_Choice_Staticness (Iassoc, Locally);
               pragma Assert (Cur_Iface /= Null_Iir);
               Set_Formal
                 (Iassoc,
                  Build_Simple_Name (Cur_Iface, Get_Location (Formal)));
               --  Insert IASSOC.
               if Prev_Assoc = Null_Iir then
                  Assoc_Chain := Iassoc;
               else
                  Set_Chain (Prev_Assoc, Iassoc);
               end if;
               Set_Chain (Iassoc, Assoc);
            end if;

            --  Add this individual association to the tree.
            Add_Individual_Association (Iassoc, Assoc);
         end if;
         Prev_Assoc := Assoc;
         Assoc := Get_Chain (Assoc);
      end loop;
      --  There is maybe a remaining iassoc.
      if Iassoc /= Null_Iir then
         Finish_Individual_Association (Iassoc);
      end if;
   end Sem_Individual_Association;

   function Is_Conversion_Function (Assoc_Chain : Iir) return Boolean is
   begin
      --  [...] whose single parameter of the function [...]
      if not Is_Chain_Length_One (Assoc_Chain) then
         return False;
      end if;
      if Get_Kind (Assoc_Chain) /= Iir_Kind_Association_Element_By_Expression
      then
         return False;
      end if;
      --  FIXME: unfortunatly, the formal may already be set with the
      --  interface.
--       if Get_Formal (Assoc_Chain) /= Null_Iir then
--          return Null_Iir;
--       end if;
      return True;
   end Is_Conversion_Function;

   function Is_Valid_Type_Conversion
     (Conv : Iir; Res_Base_Type : Iir; Param_Base_Type : Iir) return Boolean
   is
      Atype : constant Iir := Get_Type (Conv);
   begin
      return Get_Base_Type (Atype) = Res_Base_Type
        and then Are_Types_Closely_Related (Atype, Param_Base_Type);
   end Is_Valid_Type_Conversion;

   function Is_Valid_Function_Conversion
     (Call : Iir; Res_Base_Type : Iir; Param_Base_Type : Iir) return Boolean
   is
      Imp : constant Iir := Get_Implementation (Call);
      Res_Type : constant Iir := Get_Type (Imp);
      Inters : constant Iir := Get_Interface_Declaration_Chain (Imp);
      Param_Type : Iir;
   begin
      if Inters = Null_Iir then
         return False;
      end if;
      Param_Type := Get_Type (Inters);

      return Get_Base_Type (Res_Type) = Res_Base_Type
        and then Get_Base_Type (Param_Type) = Param_Base_Type;
   end Is_Valid_Function_Conversion;

   function Is_Valid_Conversion
     (Func : Iir; Res_Base_Type : Iir; Param_Base_Type : Iir) return Boolean is
   begin
      case Get_Kind (Func) is
         when Iir_Kind_Function_Call =>
            return Is_Valid_Function_Conversion
              (Func, Res_Base_Type, Param_Base_Type);
         when Iir_Kind_Type_Conversion =>
            return Is_Valid_Type_Conversion
              (Func, Res_Base_Type, Param_Base_Type);
         when others =>
            Error_Kind ("is_valid_conversion", Func);
      end case;
   end Is_Valid_Conversion;

   function Extract_Conversion
     (Conv : Iir; Res_Type : Iir; Param_Type : Iir; Loc : Iir) return Iir
   is
      List : Iir_List;
      It : List_Iterator;
      Res_Base_Type : Iir;
      Param_Base_Type : Iir;
      El : Iir;
      Res : Iir;
   begin
      Res_Base_Type := Get_Base_Type (Res_Type);
      if Param_Type = Null_Iir then
         --  In case of error.
         return Null_Iir;
      end if;
      Param_Base_Type := Get_Base_Type (Param_Type);
      if Is_Overload_List (Conv) then
         List := Get_Overload_List (Conv);
         Res := Null_Iir;
         It := List_Iterate (List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            if Is_Valid_Conversion (El, Res_Base_Type, Param_Base_Type) then
               if Res /= Null_Iir then
                  raise Internal_Error;
               end if;
               Free_Iir (Conv);
               Res := El;
            end if;
            Next (It);
         end loop;
      else
         if Is_Valid_Conversion (Conv, Res_Base_Type, Param_Base_Type) then
            Res := Conv;
         else
            Error_Msg_Sem (+Loc, "conversion function or type does not match");
            return Null_Iir;
         end if;
      end if;

      if Get_Kind (Res) = Iir_Kind_Function_Call then
         declare
            Imp : constant Iir := Get_Implementation (Res);
            Inter : constant Iir := Get_Interface_Declaration_Chain (Imp);
         begin
            if Get_Kind (Inter) /= Iir_Kind_Interface_Constant_Declaration then
               Error_Msg_Sem
                 (+Loc, "interface of function must be a constant interface");
            end if;
            if Get_Chain (Inter) /= Null_Iir then
               --  LRM08 6.5.7 Association lists
               --  In this case, the function name shall denote a function
               --  whose single parameter is of the type of the formal
               --  and [...]
               Error_Msg_Sem
                 (+Loc, "conversion function must have only one parameter");
            end if;
         end;
      end if;

      return Res;
   end Extract_Conversion;

   function Extract_In_Conversion
     (Conv : Iir; Res_Type : Iir; Param_Type : Iir) return Iir
   is
      Func : Iir;
      Assoc : Iir;
   begin
      if Conv = Null_Iir then
         return Null_Iir;
      end if;
      Func := Extract_Conversion (Conv, Res_Type, Param_Type, Conv);
      if Func = Null_Iir then
         return Null_Iir;
      end if;
      case Get_Kind (Func) is
         when Iir_Kind_Function_Call =>
            Assoc := Get_Parameter_Association_Chain (Func);
            Free_Iir (Assoc);
            Set_Parameter_Association_Chain (Func, Null_Iir);
            Name_To_Method_Object (Func, Conv);
            return Func;
         when Iir_Kind_Type_Conversion =>
            return Func;
         when others =>
            Error_Kind ("extract_in_conversion", Func);
      end case;
   end Extract_In_Conversion;

   function Extract_Out_Conversion
     (Conv : Iir; Res_Type : Iir; Param_Type : Iir) return Iir
   is
      Func : Iir;
   begin
      if Conv = Null_Iir then
         return Null_Iir;
      end if;
      Func := Extract_Conversion (Conv, Res_Type, Param_Type, Conv);

      return Func;
   end Extract_Out_Conversion;

   procedure Sem_Association_Open
     (Assoc : Iir;
      Finish : Boolean;
      Match : out Compatibility_Level)
   is
      Formal : Iir;
   begin
      if Finish then
         --  LRM 4.3.3.2  Associations lists
         --  It is an error if an actual of open is associated with a
         --  formal that is associated individually.
         if Get_Whole_Association_Flag (Assoc) = False then
            Error_Msg_Sem
              (+Assoc, "cannot associate individually with open");
         end if;

         Formal := Get_Formal (Assoc);
         if Formal /= Null_Iir then
            Set_Formal (Assoc, Finish_Sem_Name (Formal));
         end if;
      end if;
      Match := Fully_Compatible;
   end Sem_Association_Open;

   procedure Sem_Association_Package_Type_Not_Finish
     (Assoc : Iir;
      Inter : Iir;
      Match : out Compatibility_Level)
   is
      Formal : constant Iir := Get_Formal (Assoc);
   begin
      if Formal = Null_Iir then
         --  Can be associated only once
         Match := Fully_Compatible;
      else
         if Kind_In (Formal, Iir_Kind_Simple_Name, Iir_Kind_Operator_Symbol)
           and then Get_Identifier (Formal) = Get_Identifier (Inter)
         then
            Match := Fully_Compatible;
         else
            Match := Not_Compatible;
         end if;
      end if;
   end Sem_Association_Package_Type_Not_Finish;

   procedure Sem_Association_Package_Type_Finish (Assoc : Iir; Inter : Iir)
   is
      Formal : constant Iir := Get_Formal (Assoc);
   begin
      if Formal /= Null_Iir then
         pragma Assert (Get_Identifier (Formal) = Get_Identifier (Inter));
         pragma Assert (Get_Named_Entity (Formal) = Inter);
         Set_Formal (Assoc, Finish_Sem_Name (Formal));
      end if;
   end Sem_Association_Package_Type_Finish;

   procedure Sem_Association_Package
     (Assoc : Iir;
      Inter : Iir;
      Finish : Boolean;
      Match : out Compatibility_Level)
   is
      Actual : Iir;
      Package_Inter : Iir;
   begin
      if not Finish then
         Sem_Association_Package_Type_Not_Finish (Assoc, Inter, Match);
         return;
      end if;

      Match := Not_Compatible;
      Sem_Association_Package_Type_Finish (Assoc, Inter);

      --  Analyze actual.
      Actual := Get_Actual (Assoc);
      Actual := Sem_Denoting_Name (Actual);
      Set_Actual (Assoc, Actual);

      Actual := Get_Named_Entity (Actual);
      if Is_Error (Actual) then
         return;
      end if;

      --  LRM08 6.5.7.2 Generic map aspects
      --  An actual associated with a formal generic package in a
      --  generic map aspect shall be the name that denotes an instance
      --  of the uninstantiated package named in the formal generic
      --  package declaration [...]
      if Get_Kind (Actual) /= Iir_Kind_Package_Instantiation_Declaration then
         Error_Msg_Sem
           (+Assoc, "actual of association is not a package instantiation");
         return;
      end if;

      Package_Inter := Get_Uninstantiated_Package_Decl (Inter);
      if Get_Uninstantiated_Package_Decl (Actual) /= Package_Inter then
         Error_Msg_Sem
           (+Assoc,
            "actual package name is not an instance of interface package");
         return;
      end if;

      --  LRM08 6.5.7.2 Generic map aspects
      --  b) If the formal generic package declaration includes an interface
      --     generic map aspect in the form that includes the box (<>) symbol,
      --     then the instantiated package denotes by the actual may be any
      --     instance of the uninstantiated package named in the formal
      --     generic package declaration.
      if Get_Generic_Map_Aspect_Chain (Inter) = Null_Iir then
         null;
      else
         --  Other cases not yet handled.
         raise Internal_Error;
      end if;

      Match := Fully_Compatible;

      return;
   end Sem_Association_Package;

   --  Create an implicit association_element_subprogram for the declaration
   --  of function ID for ACTUAL_Type (a type/subtype definition).
   function Sem_Implicit_Operator_Association
     (Id : Name_Id; Actual_Type : Iir; Actual_Name : Iir) return Iir
   is
      use Sem_Scopes;

      --  Return TRUE if DECL is a function declaration with a comparaison
      --  operator profile.
      function Has_Comparaison_Profile (Decl : Iir) return Boolean
      is
         Inter : Iir;
      begin
         --  A function declaration.
         if not Is_Function_Declaration (Decl) then
            return False;
         end if;
         --  That returns a boolean.
         if (Get_Base_Type (Get_Return_Type (Decl))
               /= Vhdl.Std_Package.Boolean_Type_Definition)
         then
            return False;
         end if;

         --  With 2 interfaces of type ATYPE.
         Inter := Get_Interface_Declaration_Chain (Decl);
         for I in 1 .. 2 loop
            if Inter = Null_Iir then
               return False;
            end if;
            if Get_Base_Type (Get_Type (Inter)) /= Get_Base_Type (Actual_Type)
            then
               return False;
            end if;
            Inter := Get_Chain (Inter);
         end loop;
         if Inter /= Null_Iir then
            return False;
         end if;
         return True;
      end Has_Comparaison_Profile;

      Interp : Name_Interpretation_Type;
      Decl : Iir;
      Res : Iir;
   begin
      Interp := Get_Interpretation (Id);
      while Valid_Interpretation (Interp) loop
         Decl := Get_Declaration (Interp);
         if Has_Comparaison_Profile (Decl) then
            Res := Create_Iir (Iir_Kind_Association_Element_Subprogram);
            Location_Copy (Res, Actual_Name);
            Set_Actual
              (Res, Build_Simple_Name (Decl, Get_Location (Actual_Name)));
            Set_Use_Flag (Decl, True);
            return Res;
         end if;
         Interp := Get_Next_Interpretation (Interp);
      end loop;

      Error_Msg_Sem (+Actual_Name, "cannot find a %i declaration for type %i",
                     (+Id, +Actual_Name));
      return Null_Iir;
   end Sem_Implicit_Operator_Association;

   procedure Sem_Association_Type (Assoc : Iir;
                                   Inter : Iir;
                                   Finish : Boolean;
                                   Match : out Compatibility_Level)
   is
      Inter_Def : constant Iir := Get_Type (Inter);
      Actual : Iir;
      Actual_Type : Iir;
      Op_Eq, Op_Neq : Iir;
   begin
      if not Finish then
         Sem_Association_Package_Type_Not_Finish (Assoc, Inter, Match);
         return;
      end if;

      Match := Fully_Compatible;
      Sem_Association_Package_Type_Finish (Assoc, Inter);
      Actual := Get_Actual (Assoc);

      --  LRM08 6.5.7.2 Generic map aspects
      --  An actual associated with a formal generic type must be a subtype
      --  indication.
      --  FIXME: ghdl only supports type_mark!
      Actual := Sem_Types.Sem_Subtype_Indication (Actual);
      Set_Actual (Assoc, Actual);

      --  Set type association for analysis of reference to this interface.
      pragma Assert (Is_Null (Get_Associated_Type (Inter_Def)));
      if Get_Kind (Actual) in Iir_Kinds_Subtype_Definition then
         Actual_Type := Actual;
      else
         Actual_Type := Get_Type (Actual);
      end if;
      Set_Actual_Type (Assoc, Actual_Type);
      Set_Associated_Type (Inter_Def, Actual_Type);

      --  FIXME: it is not clear at all from the LRM how the implicit
      --  associations are done...
      Op_Eq := Sem_Implicit_Operator_Association
        (Std_Names.Name_Op_Equality, Actual_Type, Actual);
      if Op_Eq /= Null_Iir then
         Op_Neq := Sem_Implicit_Operator_Association
           (Std_Names.Name_Op_Inequality, Actual_Type, Actual);
         Set_Chain (Op_Eq, Op_Neq);
         Set_Subprogram_Association_Chain (Assoc, Op_Eq);
      end if;
   end Sem_Association_Type;

   function Has_Interface_Subprogram_Profile
     (Inter : Iir;
      Decl : Iir;
      Explain_Loc : Location_Type := No_Location) return Boolean
   is
      --  Handle previous assocation of interface type before full
      --  instantiation.
      function Get_Inter_Type (Inter : Iir) return Iir
      is
         Res : Iir;
      begin
         Res := Get_Type (Inter);
         if Get_Kind (Res) = Iir_Kind_Interface_Type_Definition then
            --  FIXME: recurse ?
            return Get_Associated_Type (Res);
         else
            return Res;
         end if;
      end Get_Inter_Type;

      Explain : constant Boolean := Explain_Loc /= No_Location;
      El_Inter, El_Decl : Iir;
   begin
      case Iir_Kinds_Interface_Subprogram_Declaration (Get_Kind (Inter)) is
         when Iir_Kind_Interface_Function_Declaration =>
            if not Is_Function_Declaration (Decl) then
               if Explain then
                  Error_Msg_Sem (Explain_Loc, " actual is not a function");
               end if;
               return False;
            end if;
            if Get_Base_Type (Get_Inter_Type (Inter))
              /= Get_Base_Type (Get_Type (Decl))
            then
               if Explain then
                  Error_Msg_Sem (Explain_Loc, " return type doesn't match");
               end if;
               return False;
            end if;
         when Iir_Kind_Interface_Procedure_Declaration =>
            if not Is_Procedure_Declaration (Decl) then
               if Explain then
                  Error_Msg_Sem (Explain_Loc, " actual is not a procedure");
               end if;
               return False;
            end if;
      end case;

      El_Inter := Get_Interface_Declaration_Chain (Inter);
      El_Decl := Get_Interface_Declaration_Chain (Decl);
      loop
         exit when Is_Null (El_Inter) and Is_Null (El_Decl);
         if Is_Null (El_Inter) or Is_Null (El_Decl) then
            if Explain then
               Error_Msg_Sem
                 (Explain_Loc, " number of interfaces doesn't match");
            end if;
            return False;
         end if;
         if Get_Base_Type (Get_Inter_Type (El_Inter))
           /= Get_Base_Type (Get_Type (El_Decl))
         then
            if Explain then
               Error_Msg_Sem
                 (Explain_Loc,
                  " type of interface %i doesn't match", +El_Inter);
            end if;
            return False;
         end if;
         El_Inter := Get_Chain (El_Inter);
         El_Decl := Get_Chain (El_Decl);
      end loop;

      return True;
   end Has_Interface_Subprogram_Profile;

   procedure Sem_Association_Subprogram (Assoc : Iir;
                                         Inter : Iir;
                                         Finish : Boolean;
                                         Match : out Compatibility_Level)
   is
      Discard : Boolean;
      pragma Unreferenced (Discard);
      Actual : Iir;
      Res : Iir;
   begin
      if not Finish then
         Sem_Association_Package_Type_Not_Finish (Assoc, Inter, Match);
         return;
      end if;

      Match := Fully_Compatible;
      Sem_Association_Package_Type_Finish (Assoc, Inter);
      Actual := Get_Actual (Assoc);

      --  LRM08 6.5.7.2 Generic map aspects
      --  An actual associated with a formal generic subprogram shall be a name
      --  that denotes a subprogram whose profile conforms to that of the
      --  formal, or the reserved word OPEN.  The actual, if a predefined
      --  attribute name that denotes a function, shall be one of the
      --  predefined attributes 'IMAGE, 'VALUE, 'POS, 'VAL, 'SUCC, 'PREV,
      --  'LEFTOF, or 'RIGHTOF.
      Sem_Name (Actual);
      Res := Get_Named_Entity (Actual);

      if Is_Error (Res) then
         return;
      end if;

      case Get_Kind (Res) is
         when Iir_Kinds_Subprogram_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration =>
            if not Has_Interface_Subprogram_Profile (Inter, Res) then
               Error_Msg_Sem
                 (+Assoc, "profile of %n doesn't match profile of %n",
                  (+Actual, +Inter));
               --  Explain
               Discard := Has_Interface_Subprogram_Profile
                 (Inter, Res, Get_Location (Assoc));
               return;
            end if;
         when Iir_Kind_Overload_List =>
            declare
               Nbr_Errors : Natural;
               List : Iir_List;
               It : List_Iterator;
               El, R : Iir;
            begin
               Nbr_Errors := 0;
               R := Null_Iir;
               List := Get_Overload_List (Res);
               It := List_Iterate (List);
               while Is_Valid (It) loop
                  El := Get_Element (It);
                  if Has_Interface_Subprogram_Profile (Inter, El) then
                     if Is_Null (R) then
                        R := El;
                     else
                        if Nbr_Errors = 0 then
                           Error_Msg_Sem
                             (+Assoc,
                              "many possible actual subprogram for %n:",
                              +Inter);
                           Error_Msg_Sem
                             (+Assoc, " %n declared at %l", (+R, + R));
                        else
                           Error_Msg_Sem
                             (+Assoc, " %n declared at %l", (+El, +El));
                        end if;
                        Nbr_Errors := Nbr_Errors + 1;
                     end if;
                  end if;
                  Next (It);
               end loop;
               if Is_Null (R) then
                  Error_Msg_Sem
                    (+Assoc, "no matching name for %n", +Inter);
                  if True then
                     Error_Msg_Sem
                       (+Assoc, " these names were incompatible:");
                     It := List_Iterate (List);
                     while Is_Valid (It) loop
                        El := Get_Element (It);
                        Error_Msg_Sem
                          (+Assoc, " %n declared at %l", (+El, +El));
                        Next (It);
                     end loop;
                  end if;
                  return;
               elsif Nbr_Errors > 0 then
                  return;
               end if;
               Free_Overload_List (Res);
               Res := R;
            end;
         when others =>
            Error_Kind ("sem_association_subprogram", Res);
      end case;

      Set_Named_Entity (Actual, Res);
      Vhdl.Xrefs.Xref_Name (Actual);
      Sem_Decls.Mark_Subprogram_Used (Res);
   end Sem_Association_Subprogram;

   procedure Sem_Association_Terminal
     (Assoc : Iir;
      Inter : Iir;
      Finish : Boolean;
      Match : out Compatibility_Level)
   is
      Actual_Name : Iir;
      Actual : Iir;
   begin
      if not Finish then
         Sem_Association_Package_Type_Not_Finish (Assoc, Inter, Match);
         return;
      end if;

      Match := Not_Compatible;
      Sem_Association_Package_Type_Finish (Assoc, Inter);

      --  Analyze actual.
      Actual_Name := Get_Actual (Assoc);
      Sem_Name (Actual_Name);
      Actual := Get_Named_Entity (Actual_Name);

      if Is_Error (Actual) then
         return;
      elsif Is_Overload_List (Actual) then
         Error_Msg_Sem (+Actual_Name, "terminal name expected");
         return;
      else
         Actual := Finish_Sem_Name (Actual_Name);
         case Get_Kind (Get_Object_Prefix (Actual)) is
            when Iir_Kind_Terminal_Declaration
              | Iir_Kind_Interface_Terminal_Declaration =>
               null;
            when others =>
               Error_Msg_Sem
                 (+Actual_Name, "%n is not a terminal name", +Actual);
               return;
         end case;
      end if;

      Set_Actual (Assoc, Actual);

      if (Get_Base_Nature (Get_Nature (Get_Named_Entity (Actual)))
            /= Get_Base_Nature (Get_Nature (Inter)))
      then
         Error_Msg_Sem
           (+Actual, "nature of actual is not the same as formal nature");
         return;
      end if;

      Match := Fully_Compatible;

      return;
   end Sem_Association_Terminal;

   --  Associate ASSOC with interface INTERFACE
   --  This sets MATCH.
   procedure Sem_Association_By_Expression
     (Assoc : Iir;
      Inter : Iir;
      Formal_Name : Iir;
      Formal_Conv : Iir;
      Finish : Boolean;
      Match : out Compatibility_Level)
   is
      Formal_Type : Iir;
      Actual: Iir;
      Out_Conv, In_Conv : Iir;
      Expr : Iir;
      Res_Type : Iir;
   begin
      Out_Conv := Formal_Conv;
      if Formal_Name /= Null_Iir then
         Formal_Type := Get_Type (Formal_Name);
      else
         Formal_Type := Get_Type (Inter);
      end if;

      --  If the formal type is an interface type of the same interface list,
      --  use the associated type of the formal type to analyze the actual.
      if Get_Kind (Formal_Type) = Iir_Kind_Interface_Type_Definition
        and then (Get_Parent (Get_Type_Declarator (Formal_Type))
                    = Get_Parent (Inter))
      then
         Formal_Type := Get_Associated_Type (Formal_Type);
         if Formal_Type = Null_Iir then
            --  Interface type are only allowed within generic map aspect,
            --  which are analyzed in one step (so Finish is true).
            pragma Assert (Finish);
            Error_Msg_Sem (+Assoc, "expression associated before its type");
            Match := Not_Compatible;
            return;
         end if;
      end if;

      --  Extract conversion from actual.
      --  LRM08 6.5.7.1 Association lists
      Actual := Get_Actual (Assoc);
      In_Conv := Null_Iir;
      if Get_Kind (Inter) /= Iir_Kind_Interface_Constant_Declaration then
         declare
            --  Actual before the extraction of the conversion.
            Prev_Actual : constant Iir := Actual;
         begin
            --  Extract conversion and new actual (conv_expr).
            case Get_Kind (Actual) is
               when Iir_Kind_Function_Call =>
                  Expr := Get_Parameter_Association_Chain (Actual);
                  if Is_Conversion_Function (Expr) then
                     In_Conv := Actual;
                     Actual := Get_Actual (Expr);
                  end if;
               when Iir_Kind_Type_Conversion =>
                  if Flags.Vhdl_Std > Vhdl_87 then
                     In_Conv := Actual;
                     Actual := Get_Expression (Actual);
                  end if;
               when others =>
                  null;
            end case;

            if Actual = Null_Iir then
               Match := Fully_Compatible;
               return;
            end if;

            --  There could be an ambiguity between a conversion and a normal
            --  actual expression.  Check if the new actual is an object and
            --  if the object is of the corresponding class.
            if Is_Valid (In_Conv) then
               if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
                  if not Is_Signal_Object (Actual) then
                     --  Actual is not a signal object.  This is not a
                     --  conversion but a regular association.
                     In_Conv := Null_Iir;
                     Actual := Prev_Actual;
                  end if;
               else
                  --  Variable: let as is.
                  null;
               end if;
            end if;
         end;
      end if;

      --  4 cases: F:out_conv, G:in_conv.
      --    A  => B     type of A = type of B
      --  F(A) => B     type of B = type of F
      --    A  => G(B)  type of A = type of G
      --  F(A) => G(B)  type of B = type of F, type of A = type of G
      if Out_Conv = Null_Iir and then In_Conv = Null_Iir then
         Match := Is_Expr_Compatible (Formal_Type, Actual);
      else
         Match := Fully_Compatible;
         if In_Conv /= Null_Iir then
            Match := Compatibility_Level'Min
              (Match, Is_Expr_Compatible (Formal_Type, In_Conv));
         end if;
         if Out_Conv /= Null_Iir then
            Match := Compatibility_Level'Min
              (Match, Is_Expr_Compatible (Get_Type (Out_Conv), Actual));
         end if;
      end if;

      if Match = Not_Compatible then
         if Finish and then not Is_Error (Actual) then
            Report_Start_Group;
            Error_Msg_Sem
              (+Assoc, "can't associate %n with %n", (+Actual, +Inter));
            Error_Msg_Sem
              (+Assoc, "(type of %n is " & Disp_Type_Of (Actual) & ")",
               (1 => +Actual));
            Error_Msg_Sem
              (+Inter, "(type of %n is " & Disp_Type_Of (Inter) & ")", +Inter);
            Report_End_Group;
         end if;
         return;
      end if;

      if not Finish then
         return;
      end if;

      --  At that point, the analysis is being finished.

      if Out_Conv = Null_Iir and then In_Conv = Null_Iir then
         Res_Type := Formal_Type;
      else
         if Out_Conv /= Null_Iir then
            Res_Type := Search_Compatible_Type (Get_Type (Out_Conv),
                                                Get_Type (Actual));
         else
            Res_Type := Get_Type (Actual);
         end if;

         if In_Conv /= Null_Iir then
            In_Conv := Extract_In_Conversion (In_Conv, Formal_Type, Res_Type);
         end if;
         if Out_Conv /= Null_Iir then
            Out_Conv := Extract_Out_Conversion (Out_Conv,
                                                Res_Type, Formal_Type);
         end if;
      end if;

      if Res_Type = Null_Iir then
         --  In case of error, do not go farther.
         Match := Not_Compatible;
         return;
      end if;

      if Formal_Name /= Null_Iir then
         declare
            Formal : Iir;
            Conv_Assoc : Iir;
         begin
            --  Extract formal from the conversion (and unlink it from the
            --  conversion, as the owner of the formal is the association, not
            --  the conversion).
            Formal := Finish_Sem_Name (Get_Formal (Assoc));
            case Get_Kind (Formal) is
               when Iir_Kind_Function_Call =>
                  pragma Assert (Formal_Conv /= Null_Iir);
                  Set_Formal_Conversion (Assoc, Formal);
                  Conv_Assoc := Get_Parameter_Association_Chain (Formal);
                  Set_Parameter_Association_Chain (Formal, Null_Iir);
                  Formal := Get_Actual (Conv_Assoc);
                  Free_Iir (Conv_Assoc);
                  --  Name_To_Method_Object (Func, Conv);
               when Iir_Kind_Type_Conversion =>
                  pragma Assert (Formal_Conv /= Null_Iir);
                  Conv_Assoc := Formal;
                  Set_Formal_Conversion (Assoc, Formal);
                  Formal := Get_Expression (Formal);
                  Set_Expression (Conv_Assoc, Null_Iir);
               when others =>
                  pragma Assert (Formal_Conv = Null_Iir);
                  null;
            end case;
            Set_Formal (Assoc, Formal);

            --  Use the type of the formal to analyze the actual.  In
            --  particular, the formal may be constrained while the actual is
            --  not.
            --  (but not when the formal_type is an interface type, as it
            --  will bring nothing more and could have been substitued by
            --  its associated type).
            Formal_Type := Get_Type (Formal);
            if (Out_Conv = Null_Iir and In_Conv = Null_Iir)
              and then
              Get_Kind (Formal_Type) /= Iir_Kind_Interface_Type_Definition
            then
               Res_Type := Formal_Type;
            end if;
         end;
      end if;

      --  LRM08 6.5.7 Association lists
      --  The formal part of a named association element may be in the form of
      --  a function call [...] if and only if the formal is an interface
      --  object, the mode of the formal is OUT, INOUT, BUFFER or LINKAGE [...]
      if Out_Conv /= Null_Iir
        and then Get_Mode (Inter) = Iir_In_Mode
      then
         Error_Msg_Sem
           (+Assoc, "can't use an out conversion for an in interface");
      end if;

      --  LRM08 6.5.7 Association lists
      --  The actual part of an association element may be in the form of a
      --  function call [...] if and only if the mode of the format is IN,
      --  INOUT or LINKAGE [...]
      Set_Actual_Conversion (Assoc, In_Conv);
      if In_Conv /= Null_Iir
        and then Get_Mode (Inter) in Iir_Buffer_Mode .. Iir_Out_Mode
      then
         Error_Msg_Sem
           (+Assoc, "can't use an in conversion for an out/buffer interface");
      end if;

      --  LRM08 5.3.2.2 Index constraints and discrete ranges
      --  e) [...]
      --    3) [...]
      --      -- For an interface object or subelement whose mode is IN, INOUT
      --         or LINKAGE, if the actual part includes a conversion function
      --         or a type conversion, then the result type of that function
      --         or the type mark of the type conversion shall define a
      --         constraint for the index range corresponding to the index
      --         range of the objet, [...]
      --      -- For an interface object or subelement whose mode is OUT,
      --         BUFFER, INOUT or LINKAGE, if the formal part includes a
      --         conversion function or a type conversion, then the parameter
      --         subtype of that function or the type mark of the type
      --         conversion shall define a constraint for the index range
      --         corresponding to the index range of the object, [...]
      if not Is_Fully_Constrained_Type (Formal_Type) then
         if (Get_Mode (Inter) in Iir_In_Modes
               or else Get_Mode (Inter) = Iir_Linkage_Mode)
           and then In_Conv /= Null_Iir
           and then not Is_Fully_Constrained_Type (Get_Type (In_Conv))
         then
            Error_Msg_Sem
              (+Assoc, "type of actual conversion must be fully constrained");
         end if;
         if (Get_Mode (Inter) in Iir_Out_Modes
               or else Get_Mode (Inter) = Iir_Linkage_Mode)
           and then Out_Conv /= Null_Iir
           and then not Is_Fully_Constrained_Type (Get_Type (Out_Conv))
         then
            Error_Msg_Sem
              (+Assoc, "type of formal conversion must be fully constrained");
         end if;
      end if;

      --  FIXME: LRM refs
      --  This is somewhat wrong.  A missing conversion is not an error but
      --  may result in a type mismatch.
      if Get_Mode (Inter) = Iir_Inout_Mode then
         if In_Conv = Null_Iir and then Out_Conv /= Null_Iir then
            Error_Msg_Sem
              (+Assoc, "out conversion without corresponding in conversion");
         elsif In_Conv /= Null_Iir and then Out_Conv = Null_Iir then
            Error_Msg_Sem
              (+Assoc, "in conversion without corresponding out conversion");
         end if;
      end if;
      Set_Actual (Assoc, Actual);

      --  Analyze actual.
      Expr := Sem_Expression (Actual, Res_Type);
      if Expr /= Null_Iir then
         Expr := Eval_Expr_Check_If_Static (Expr, Res_Type);
         Set_Actual (Assoc, Expr);
         if In_Conv = Null_Iir and then Out_Conv = Null_Iir then
            if not Eval_Is_In_Bound (Expr, Formal_Type, True) then
               Error_Msg_Sem
                 (+Assoc, "actual constraints don't match formal ones");
            end if;
         end if;
      end if;
   end Sem_Association_By_Expression;

   --  Associate ASSOC with interface INTERFACE
   --  This sets MATCH.
   procedure Sem_Association (Assoc : Iir;
                              Inter : Iir;
                              Formal : Iir;
                              Formal_Conv : Iir;
                              Finish : Boolean;
                              Match : out Compatibility_Level) is
   begin
      case Iir_Kinds_Interface_Declaration (Get_Kind (Inter)) is
         when Iir_Kinds_Interface_Object_Declaration =>
            if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
               Sem_Association_Open (Assoc, Finish, Match);
            else
               Sem_Association_By_Expression
                 (Assoc, Inter, Formal, Formal_Conv, Finish, Match);
            end if;

         when Iir_Kind_Interface_Terminal_Declaration =>
            if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
               Sem_Association_Open (Assoc, Finish, Match);
            else
               Sem_Association_Terminal (Assoc, Inter, Finish, Match);
            end if;

         when Iir_Kind_Interface_Package_Declaration =>
            Sem_Association_Package (Assoc, Inter, Finish, Match);

         when Iir_Kind_Interface_Type_Declaration =>
            Sem_Association_Type (Assoc, Inter, Finish, Match);

         when Iir_Kinds_Interface_Subprogram_Declaration =>
            Sem_Association_Subprogram (Assoc, Inter, Finish, Match);
      end case;
   end Sem_Association;

   procedure Sem_Association_Chain
     (Interface_Chain : Iir;
      Assoc_Chain: in out Iir;
      Finish: Boolean;
      Missing : Missing_Type;
      Loc : Iir;
      Match : out Compatibility_Level)
   is
      Assoc : Iir;
      Inter : Iir;

      --  True if -Whide is enabled (save the state).
      Warn_Hide_Enabled : Boolean;

      type Param_Assoc_Type is (None, Open, Individual, Whole);

      type Assoc_Array is array (Natural range <>) of Param_Assoc_Type;
      Nbr_Inter : constant Natural := Get_Chain_Length (Interface_Chain);
      Inter_Matched : Assoc_Array (0 .. Nbr_Inter - 1) := (others => None);

      Last_Individual : Iir;
      Has_Individual : Boolean;
      Pos : Integer;
      Formal : Iir;

      First_Named_Assoc : Iir;
      Last_Named_Assoc : Iir;

      Formal_Name : Iir;
      Formal_Conv : Iir;
   begin
      Match := Fully_Compatible;
      First_Named_Assoc := Null_Iir;
      Has_Individual := False;

      --  Clear associated type of interface type.
      Inter := Interface_Chain;
      while Inter /= Null_Iir loop
         if Get_Kind (Inter) = Iir_Kind_Interface_Type_Declaration then
            Set_Associated_Type (Get_Type (Inter), Null_Iir);
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      --  Loop on every assoc element, try to match it.
      Inter := Interface_Chain;
      Last_Individual := Null_Iir;
      Pos := 0;

      --  First positional associations
      Assoc := Assoc_Chain;
      while Assoc /= Null_Iir loop
         Formal := Get_Formal (Assoc);
         exit when Formal /= Null_Iir;

         --  Try to match actual of ASSOC with the interface.
         if Inter = Null_Iir then
            if Finish then
               Error_Msg_Sem (+Assoc, "too many actuals for %n", +Loc);
            end if;
            Match := Not_Compatible;
            return;
         end if;
         Set_Whole_Association_Flag (Assoc, True);
         Sem_Association (Assoc, Inter, Null_Iir, Null_Iir, Finish, Match);
         if Match = Not_Compatible then
            return;
         end if;
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
            Inter_Matched (Pos) := Open;
         else
            Inter_Matched (Pos) := Whole;
         end if;
         Set_Whole_Association_Flag (Assoc, True);
         Inter := Get_Chain (Inter);

         Pos := Pos + 1;
         Assoc := Get_Chain (Assoc);
      end loop;

      --  Then association by name.
      if Assoc /= Null_Iir then
         --  Make interfaces visible
         --
         --  LRM08 12.3 Visibility
         --  A declaration is visible by selection at places that are defined
         --  as follows:
         --  j) For a formal parameter declaration of a given subprogram
         --     declaration: at the place of the formal part (before the
         --     compound delimiter =>) of a named parameter association
         --     element of a corresponding subprogram call.
         --  k) For a local generic declaration of a given component
         --     declaration ...
         --  l) For a local port declaration of a given component declaration:
         --     ...
         --  m) For a formal generic declaration of a given entity declaration:
         --     ...
         --  n) For a formal port declaration of a given entity declaration:
         --     ...
         --  o) For a formal generic declaration or a formal port declaration
         --     of a given block statement: ...
         --  p) For a formal generic declaration of a given package
         --     declaration: ...
         --  q) For a formal generic declaration of a given subprogram
         --     declarations: ...
         --
         --  At a place in which a given declaration is visible by selection,
         --  every declaration with the same designator as the given
         --  declaration and that would otherwise be directly visible is
         --  hidden.
         Sem_Scopes.Open_Declarative_Region;

         --  Do not warn about hidding here, way to common, way useless.
         Warn_Hide_Enabled := Is_Warning_Enabled (Warnid_Hide);
         Enable_Warning (Warnid_Hide, False);

         Sem_Scopes.Add_Declarations_From_Interface_Chain (Interface_Chain);

         Enable_Warning (Warnid_Hide, Warn_Hide_Enabled);

         First_Named_Assoc := Assoc;
         loop
            if Formal = Null_Iir then
               --  Positional after named argument.  Already caught by
               --  Sem_Actual_Of_Association_Chain (because it is called only
               --  once, while sem_association_chain may be called several
               --  times).
               Match := Not_Compatible;
               exit;
            end if;

            --  Last assoc to be cleaned up.
            Last_Named_Assoc := Assoc;

            if Finish then
               Sem_Name (Formal);
            else
               Sem_Name_Soft (Formal);
            end if;
            Formal_Name := Get_Named_Entity (Formal);
            if Is_Error (Formal_Name) then
               Match := Not_Compatible;
               --  Continue analysis in order to catch more errors.
            end if;

            Assoc := Get_Chain (Assoc);
            exit when Assoc = Null_Iir;
            Formal := Get_Formal (Assoc);
         end loop;

         --  Remove visibility by selection of interfaces.  This is needed
         --  to correctly analyze actuals.
         Sem_Scopes.Close_Declarative_Region;

         if Match /= Not_Compatible then
            Assoc := First_Named_Assoc;
            loop
               Formal := Get_Formal (Assoc);
               Formal_Name := Get_Named_Entity (Formal);

               --  Extract conversion
               Formal_Conv := Null_Iir;
               case Get_Kind (Formal_Name) is
                  when Iir_Kind_Function_Call =>
                     --  Only one actual
                     declare
                        Call_Assoc : constant Iir :=
                          Get_Parameter_Association_Chain (Formal_Name);
                     begin
                        if (Get_Kind (Call_Assoc)
                              /= Iir_Kind_Association_Element_By_Expression)
                          or else Get_Chain (Call_Assoc) /= Null_Iir
                          or else Get_Formal (Call_Assoc) /= Null_Iir
                          or else (Get_Actual_Conversion (Call_Assoc)
                                     /= Null_Iir)
                        then
                           if Finish then
                              Error_Msg_Sem
                                (+Assoc, "ill-formed formal conversion");
                           end if;
                           Match := Not_Compatible;
                           exit;
                        end if;
                        Formal_Conv := Formal_Name;
                        Formal_Name := Get_Actual (Call_Assoc);
                     end;
                  when Iir_Kind_Type_Conversion =>
                     Formal_Conv := Formal_Name;
                     Formal_Name := Get_Expression (Formal_Name);
                  when Iir_Kind_Slice_Name
                    | Iir_Kind_Indexed_Name
                    | Iir_Kind_Selected_Element
                    | Iir_Kind_Simple_Name =>
                     null;
                  when others =>
                     Formal_Name := Formal;
               end case;
               case Get_Kind (Formal_Name) is
                  when Iir_Kind_Selected_Element
                    | Iir_Kind_Slice_Name
                    | Iir_Kind_Indexed_Name =>
                     Inter := Get_Base_Name (Formal_Name);
                     Set_Whole_Association_Flag (Assoc, False);
                  when Iir_Kind_Simple_Name
                    | Iir_Kind_Operator_Symbol =>
                     Inter := Get_Named_Entity (Formal_Name);
                     Formal_Name := Inter;
                     Set_Whole_Association_Flag (Assoc, True);
                  when others =>
                     --  Error
                     if Finish then
                        Error_Msg_Sem (+Assoc, "formal is not a name");
                     end if;
                     Match := Not_Compatible;
                     exit;
               end case;

               --  Simplify overload list (for interface subprogram).
               --  FIXME: Interface must hide previous subprogram declarations,
               --  so there should be no need to filter.
               if Is_Overload_List (Inter) then
                  declare
                     List : constant Iir_List := Get_Overload_List (Inter);
                     It : List_Iterator;
                     Filtered_Inter : Iir;
                     El : Iir;
                  begin
                     Filtered_Inter := Null_Iir;
                     It := List_Iterate (List);
                     while Is_Valid (It) loop
                        El := Get_Element (It);
                        if Get_Kind (El) in Iir_Kinds_Interface_Declaration
                          and then
                          Get_Parent (El) = Get_Parent (Interface_Chain)
                        then
                           Add_Result (Filtered_Inter, El);
                        end if;
                        Next (It);
                     end loop;
                     Free_Overload_List (Inter);
                     Inter := Filtered_Inter;

                     pragma Assert
                       (Get_Kind (Formal) = Iir_Kind_Simple_Name
                          or else
                          Get_Kind (Formal) = Iir_Kind_Operator_Symbol);
                     Set_Named_Entity (Formal, Inter);

                     if Inter = Null_Iir then
                        if Finish then
                           Error_Msg_Sem (+Assoc, "no interface %i for %n",
                                          (+Formal, +Loc));
                        end if;
                        Match := Not_Compatible;
                        exit;
                     end if;

                     if Is_Overload_List (Inter) then
                        if Finish then
                           Error_Msg_Sem (+Assoc, "ambiguous formal name");
                        end if;
                        Match := Not_Compatible;
                        exit;
                     end if;
                  end;
               end if;
               if Get_Kind (Inter) not in Iir_Kinds_Interface_Declaration
                 or else Interface_Chain = Null_Iir
                 or else Get_Parent (Inter) /= Get_Parent (Interface_Chain)
               then
                  if Finish then
                     Error_Msg_Sem
                       (+Formal, "%n is not an interface name", +Inter);
                  end if;
                  Match := Not_Compatible;
                  exit;
               end if;

               --  LRM 4.3.2.2  Association Lists
               --  The formal part of a named element association may be
               --  in the form of a function call, [...], if and only
               --  if the mode of the formal is OUT, INOUT, BUFFER, or
               --  LINKAGE, and the actual is not OPEN.
               if Formal_Conv /= Null_Iir
                 and then (Get_Kind (Inter)
                             not in Iir_Kinds_Interface_Object_Declaration
                             or else Get_Mode (Inter) = Iir_In_Mode)
               then
                  if Finish then
                     Error_Msg_Sem
                       (+Assoc,
                        "formal conversion allowed only for interface object");
                  end if;
                  Match := Not_Compatible;
                  exit;
               end if;

               --  Find the Interface.
               declare
                  Inter1 : Iir;
               begin
                  Inter1 := Interface_Chain;
                  Pos := 0;
                  while Inter1 /= Null_Iir loop
                     exit when Inter = Inter1;
                     Inter1 := Get_Chain (Inter1);
                     Pos := Pos + 1;
                  end loop;
                  if Inter1 = Null_Iir then
                     if Finish then
                        Error_Msg_Sem
                          (+Assoc,
                           "no corresponding interface for %i", +Inter);
                     end if;
                     Match := Not_Compatible;
                     exit;
                  end if;
               end;

               Sem_Association
                 (Assoc, Inter, Formal_Name, Formal_Conv, Finish, Match);
               exit when Match = Not_Compatible;

               if Get_Whole_Association_Flag (Assoc) then
                  --  Whole association.
                  Last_Individual := Null_Iir;
                  if Inter_Matched (Pos) = None then
                     if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open
                     then
                        Inter_Matched (Pos) := Open;
                     else
                        Inter_Matched (Pos) := Whole;
                     end if;
                  else
                     if Finish then
                        Error_Msg_Sem
                          (+Assoc, "%n already associated", +Inter);
                     end if;
                     Match := Not_Compatible;
                     exit;
                  end if;
               else
                  --  Individual association.
                  Has_Individual := True;
                  if Inter_Matched (Pos) /= Whole then
                     if Finish
                       and then Inter_Matched (Pos) = Individual
                       and then Last_Individual /= Inter
                     then
                        Error_Msg_Sem
                          (+Assoc,
                           "non consecutive individual association for %n",
                           +Inter);
                        Match := Not_Compatible;
                        exit;
                     end if;
                     Last_Individual := Inter;
                     Inter_Matched (Pos) := Individual;
                  else
                     if Finish then
                        Error_Msg_Sem
                          (+Assoc, "%n already associated", +Inter);
                        Match := Not_Compatible;
                        exit;
                     end if;
                  end if;
               end if;

               Assoc := Get_Chain (Assoc);
               exit when Assoc = Null_Iir;
            end loop;
         end if;

         if Finish and Has_Individual and Match /= Not_Compatible then
            Sem_Individual_Association (Assoc_Chain);
         end if;

         if not Finish then
            --  Always cleanup if not finishing: there can be other tries in
            --  case of overloading.
            Assoc := First_Named_Assoc;
            while Assoc /= Null_Iir loop
               Formal := Get_Formal (Assoc);
               --  User may have used by position assoc after named
               --  assocs.
               if Is_Valid (Formal) then
                  Sem_Name_Clean (Formal);
               end if;
               exit when Assoc = Last_Named_Assoc;
               Assoc := Get_Chain (Assoc);
            end loop;
         end if;

         if Match = Not_Compatible then
            return;
         end if;
      end if;

      if Missing = Missing_Allowed then
         --  No need to check for missing associations.
         return;
      end if;

      --  LRM93 8.6 Procedure Call Statement
      --  For each formal parameter of a procedure, a procedure call must
      --  specify exactly one corresponding actual parameter.
      --  This actual parameter is specified either explicitly, by an
      --  association element (other than the actual OPEN) in the association
      --  list, or in the absence of such an association element, by a default
      --  expression (see Section 4.3.3.2).

      --  LRM93 7.3.3 Function Calls
      --  For each formal parameter of a function, a function call must
      --  specify exactly one corresponding actual parameter.
      --  This actual parameter is specified either explicitly, by an
      --  association element (other than the actual OPEN) in the association
      --  list, or in the absence of such an association element, by a default
      --  expression (see Section 4.3.3.2).

      --  LRM93 1.1.1.2 / LRM08 6.5.6.3 Port clauses
      --  A port of mode IN may be unconnected or unassociated only if its
      --  declaration includes a default expression.
      --  A port of any mode other than IN may be unconnected or unassociated
      --  as long as its type is not an unconstrained array type.

      --  LRM08 6.5.6.2 Generic clauses
      --  It is an error if no such actual [instantiated package] is specified
      --  for a given formal generic package (either because the formal generic
      --  is unassociated or because the actual is OPEN).

      Inter := Interface_Chain;
      Pos := 0;
      while Inter /= Null_Iir loop
         if Inter_Matched (Pos) <= Open then
            if Sem_Check_Missing_Association
              (Inter, Missing, Finish, Inter_Matched (Pos) = Open, Loc)
            then
               Match := Not_Compatible;
               if not Finish then
                  return;
               end if;
            end if;
         end if;

         Inter := Get_Chain (Inter);
         Pos := Pos + 1;
      end loop;
   end Sem_Association_Chain;

   function Sem_Check_Missing_Association (Inter : Iir;
                                           Missing : Missing_Type;
                                           Finish : Boolean;
                                           Is_Open : Boolean;
                                           Loc : Iir) return Boolean
   is
      Err : Boolean;
   begin
      --  Interface is unassociated (none or open).
      Err := False;
      case Get_Kind (Inter) is
         when Iir_Kinds_Interface_Object_Declaration =>
            case Missing is
               when Missing_Parameter
                  | Missing_Generic =>
                  if Get_Mode (Inter) /= Iir_In_Mode
                    or else Get_Default_Value (Inter) = Null_Iir
                  then
                     Err := True;
                     if Finish then
                        Error_Msg_Sem (+Loc, "no actual for %n", +Inter);
                     else
                        return True;
                     end if;
                  end if;
               when Missing_Port =>
                  case Get_Mode (Inter) is
                     when Iir_In_Mode =>
                        --  No overloading for components/entities.
                        pragma Assert (Finish);
                        if Get_Default_Value (Inter) = Null_Iir then
                           Error_Msg_Sem
                             (+Loc, "%n of mode IN must be connected", +Inter);
                           Err := True;
                        elsif not Is_Open then
                           Warning_Msg_Sem
                             (Warnid_No_Assoc, +Loc,
                              "%n of mode IN is not connected", +Inter);
                        end if;
                     when Iir_Out_Mode
                        | Iir_Linkage_Mode
                        | Iir_Inout_Mode
                        | Iir_Buffer_Mode =>
                        --  No overloading for components/entities.
                        pragma Assert (Finish);
                        if not Is_Fully_Constrained_Type (Get_Type (Inter))
                        then
                           Error_Msg_Sem
                             (+Loc,
                              "unconstrained %n must be connected", +Inter);
                           Err := True;
                        elsif not Is_Open then
                           Warning_Msg_Sem
                             (Warnid_No_Assoc, +Loc,
                              "%n of mode OUT is not connected", +Inter);
                        end if;
                     when Iir_Unknown_Mode =>
                        raise Internal_Error;
                  end case;
               when Missing_Allowed =>
                  null;
            end case;
         when Iir_Kind_Interface_Package_Declaration =>
            if Get_Generic_Map_Aspect_Chain (Inter) = Null_Iir then
               Error_Msg_Sem (+Loc, "%n must be associated", +Inter);
               Err := True;
            end if;
         when Iir_Kind_Interface_Function_Declaration
            | Iir_Kind_Interface_Procedure_Declaration =>
            Error_Msg_Sem (+Loc, "%n must be associated", +Inter);
            Err := True;
         when others =>
            Error_Kind ("sem_association_chain", Inter);
      end case;
      return Err;
   end Sem_Check_Missing_Association;
end Vhdl.Sem_Assocs;
