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
with Evaluation; use Evaluation;
with Errorout; use Errorout;
with Flags; use Flags;
with Types; use Types;
with Iirs_Utils; use Iirs_Utils;
with Parse;
with Std_Names;
with Sem_Names; use Sem_Names;
with Sem_Types;
with Std_Package;
with Sem_Scopes;
with Iir_Chains; use Iir_Chains;
with Xrefs;

package body Sem_Assocs is
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
                  Set_Index_Constraint_List (N_Actual, Indexes);
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
                  Actual := N_Actual;
               end;
            end if;
         when Iir_Kinds_Interface_Subprogram_Declaration =>
            N_Assoc := Create_Iir (Iir_Kind_Association_Element_Subprogram);
            if Get_Kind (Actual) = Iir_Kind_String_Literal8 then
               Actual := Parse.String_To_Operator_Symbol (Actual);
            end if;
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

   procedure Check_Subprogram_Associations
     (Inter_Chain : Iir; Assoc_Chain : Iir)
   is
      Assoc : Iir;
      Formal_Inter : Iir;
      Actual : Iir;
      Prefix : Iir;
      Object : Iir;
      Inter : Iir;
   begin
      Assoc := Assoc_Chain;
      Inter := Inter_Chain;
      while Assoc /= Null_Iir loop
         Formal_Inter := Get_Association_Interface (Assoc, Inter);
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               if Get_Default_Value (Formal_Inter) = Null_Iir then
                  Error_Msg_Sem
                    (+Assoc, "no parameter for %n", +Formal_Inter);
               end if;
            when Iir_Kind_Association_Element_By_Expression =>
               Actual := Get_Actual (Assoc);
               Object := Name_To_Object (Actual);
               if Object /= Null_Iir then
                  Prefix := Get_Object_Prefix (Object);
               else
                  Prefix := Actual;
               end if;

               case Get_Kind (Formal_Inter) is
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
                                (+Actual,
                                 "actual signal must be a static name");
                           else
                              --  Inherit has_active_flag.
                              Set_Has_Active_Flag
                                (Prefix, Get_Has_Active_Flag (Formal_Inter));
                           end if;
                        when others =>
                           Error_Msg_Sem
                             (+Assoc,
                              "signal parameter requires a signal expression");
                     end case;

                     case Get_Kind (Prefix) is
                        when Iir_Kind_Interface_Signal_Declaration =>
                           Check_Parameter_Association_Restriction
                             (Formal_Inter, Prefix, Assoc);
                        when Iir_Kind_Guard_Signal_Declaration =>
                           if Get_Mode (Formal_Inter) /= Iir_In_Mode then
                              Error_Msg_Sem
                                (+Assoc,
                                 "cannot associate a guard signal with "
                                 & Get_Mode_Name (Get_Mode (Formal_Inter))
                                 & " %n", +Formal_Inter);
                           end if;
                        when Iir_Kinds_Signal_Attribute =>
                           if Get_Mode (Formal_Inter) /= Iir_In_Mode then
                              Error_Msg_Sem
                                (+Assoc,
                                 "cannot associate a signal attribute with "
                                 & Get_Mode_Name (Get_Mode (Formal_Inter))
                                 & " %n", +Formal_Inter);
                           end if;
                        when others =>
                           null;
                     end case;

                     --  LRM 2.1.1.2  Signal parameters
                     --  It is an error if a conversion function or type
                     --  conversion appears in either the formal part or the
                     --  actual part of an association element that associates
                     --  an actual signal with a formal signal parameter.
                     if Get_In_Conversion (Assoc) /= Null_Iir
                       or Get_Out_Conversion (Assoc) /= Null_Iir
                     then
                        Error_Msg_Sem
                          (+Assoc,
                           "conversion are not allowed for signal parameters");
                     end if;
                  when Iir_Kind_Interface_Variable_Declaration =>
                     --  LRM93 2.1.1
                     --  The actual designator associated with a formal of
                     --  class variable must be a variable.
                     case Get_Kind (Prefix) is
                        when Iir_Kind_Interface_Variable_Declaration =>
                           Check_Parameter_Association_Restriction
                             (Formal_Inter, Prefix, Assoc);
                        when Iir_Kind_Variable_Declaration
                          | Iir_Kind_Dereference
                          | Iir_Kind_Implicit_Dereference =>
                           null;
                        when Iir_Kind_Interface_File_Declaration
                          | Iir_Kind_File_Declaration =>
                           --  LRM87 4.3.1.4
                           --  Such an object is a member of the variable
                           --  class of objects;
                           if Flags.Vhdl_Std >= Vhdl_93 then
                              Error_Msg_Sem
                                (+Assoc, "variable parameter cannot be a "
                                   & "file (vhdl93)");
                           end if;
                        when others =>
                           Error_Msg_Sem
                             (+Assoc, "variable parameter must be a variable");
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
                           if Flags.Vhdl_Std >= Vhdl_93 then
                              Error_Msg_Sem (+Assoc, "file parameter "
                                               & "must be a file (vhdl93)");
                           end if;
                        when others =>
                           Error_Msg_Sem
                             (+Assoc, "file parameter must be a file");
                     end case;

                     --  LRM 2.1.1.3  File parameters
                     --  It is an error if an association element associates
                     --  an actual with a formal parameter of a file type and
                     --  that association element contains a conversion
                     --  function or type conversion.
                     if Get_In_Conversion (Assoc) /= Null_Iir
                       or Get_Out_Conversion (Assoc) /= Null_Iir
                     then
                        Error_Msg_Sem (+Assoc, "conversion are not allowed "
                                         & "for file parameters");
                     end if;
                  when Iir_Kind_Interface_Constant_Declaration =>
                     --  LRM93 2.1.1
                     --  The actual designator associated with a formal of
                     --  class constant must be an expression.
                     Check_Read (Actual);
                  when others =>
                     Error_Kind
                       ("check_subprogram_association(3)", Formal_Inter);
               end case;
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
         when Vhdl_87 | Vhdl_93c | Vhdl_93 | Vhdl_00 =>
            if Vhdl93_Assocs_Map (Fmode, Amode) then
               return True;
            end if;
         when Vhdl_02 =>
            if Vhdl02_Assocs_Map (Fmode, Amode) then
               return True;
            end if;
         when Vhdl_08 =>
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
         --  for 93c and relaxed rules.
         if Vhdl_Std >= Vhdl_08
           or else Vhdl_Std = Vhdl_93c
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
      F_Conv : constant Iir := Get_Out_Conversion (Assoc);
      A_Conv : constant Iir := Get_In_Conversion (Assoc);
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
   --  SUB_ASSOC is an association_by_individual in which the formal will be
   --   inserted.
   --  Update SUB_ASSOC so that it designates FORMAL.
   procedure Add_Individual_Assoc_Indexed_Name
     (Sub_Assoc : in out Iir; Formal : Iir)
   is
      Base_Assoc : constant Iir := Sub_Assoc;
      Index_List : constant Iir_List := Get_Index_List (Formal);
      Nbr : constant Natural := Get_Nbr_Elements (Index_List);
      Choice : Iir;
      Last_Choice : Iir;
      Index : Iir;
      Staticness : Iir_Staticness;
   begin
      --  Find element.
      for I in 0 .. Nbr - 1 loop
         Index := Get_Nth_Element (Index_List, I);

         --  Evaluate index.
         Staticness := Get_Expr_Staticness (Index);
         if Staticness = Locally then
            Index := Eval_Expr (Index);
            Replace_Nth_Element (Index_List, I, Index);
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
            Sub_Assoc := Get_Associated_Expr (Choice);
            if Sub_Assoc = Null_Iir then
               Sub_Assoc := Create_Iir
                 (Iir_Kind_Association_Element_By_Individual);
               Location_Copy (Sub_Assoc, Index);
               Set_Associated_Expr (Choice, Sub_Assoc);
               Set_Choice_Staticness (Sub_Assoc, Locally);
            end if;
         else
            Sub_Assoc := Choice;
         end if;
      end loop;
   end Add_Individual_Assoc_Indexed_Name;

   procedure Add_Individual_Assoc_Slice_Name
     (Sub_Assoc : in out Iir; Formal : Iir)
   is
      Choice : Iir;
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

      Sub_Assoc := Choice;
   end Add_Individual_Assoc_Slice_Name;

   procedure Add_Individual_Assoc_Selected_Name
     (Sub_Assoc : in out Iir; Formal : Iir)
   is
      Choice : Iir;
   begin
      Choice := Create_Iir (Iir_Kind_Choice_By_Name);
      Location_Copy (Choice, Formal);
      Set_Choice_Name (Choice, Get_Selected_Element (Formal));
      Set_Chain (Choice, Get_Individual_Association_Chain (Sub_Assoc));
      Set_Individual_Association_Chain (Sub_Assoc, Choice);

      Sub_Assoc := Choice;
   end Add_Individual_Assoc_Selected_Name;

   --  Subroutine of Add_Individual_Association.
   --  Search/build the tree of choices for FORMAL, starting for IASSOC.
   procedure Add_Individual_Association_1 (Iassoc : in out Iir; Formal : Iir)
   is
      Base_Assoc : constant Iir := Iassoc;
      Formal_Object : constant Iir := Name_To_Object (Formal);
      Sub : Iir;
   begin
      pragma Assert
        (Get_Kind (Base_Assoc) = Iir_Kind_Association_Element_By_Individual);

      --  Recurse to start from the basename of the formal.
      case Get_Kind (Formal_Object) is
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Element =>
            Add_Individual_Association_1 (Iassoc, Get_Prefix (Formal_Object));
         when Iir_Kinds_Interface_Object_Declaration =>
            return;
         when others =>
            Error_Kind ("add_individual_association_1", Formal);
      end case;

      case Get_Kind (Iassoc) is
         when Iir_Kind_Association_Element_By_Individual =>
            null;
         when Iir_Kind_Choice_By_Expression =>
            Sub := Get_Associated_Expr (Iassoc);
            if Sub = Null_Iir then
               Sub := Create_Iir (Iir_Kind_Association_Element_By_Individual);
               Location_Copy (Sub, Formal);
               Set_Choice_Staticness (Sub, Locally);
               Set_Formal (Sub, Iassoc);
               Set_Associated_Expr (Iassoc, Sub);
               Iassoc := Sub;
            else
               case Get_Kind (Sub) is
                  when Iir_Kind_Association_Element_By_Individual =>
                     Iassoc := Sub;
                  when others =>
                     Error_Msg_Sem
                       (+Formal, "individual association of %n"
                          & " conflicts with that at %l",
                        (+Get_Interface_Of_Formal (Get_Formal (Iassoc)),
                         +Sub));
                     return;
               end case;
            end if;
         when others =>
            Error_Kind ("add_individual_association_1(2)", Iassoc);
      end case;

      Sub := Iassoc;
      case Get_Kind (Formal_Object) is
         when Iir_Kind_Indexed_Name =>
            Add_Individual_Assoc_Indexed_Name (Iassoc, Formal_Object);
         when Iir_Kind_Slice_Name =>
            Add_Individual_Assoc_Slice_Name (Iassoc, Formal_Object);
         when Iir_Kind_Selected_Element =>
            Add_Individual_Assoc_Selected_Name (Iassoc, Formal_Object);
         when others =>
            Error_Kind ("add_individual_association_1(3)", Formal);
      end case;

      if Get_Choice_Staticness (Sub) /= Locally then
         --  Propagate error.
         Set_Choice_Staticness (Base_Assoc, None);
      end if;
   end Add_Individual_Association_1;

   --  Insert ASSOC into the tree of individual assoc rooted by IASSOC.
   procedure Add_Individual_Association (Iassoc : Iir; Assoc : Iir)
   is
      Formal : constant Iir := Get_Formal (Assoc);
      Res_Iass : Iir;
      Prev : Iir;
   begin
      --  Create the individual association for the formal.
      Res_Iass := Iassoc;
      Add_Individual_Association_1 (Res_Iass, Formal);

      Prev := Get_Associated_Expr (Res_Iass);
      if Prev /= Null_Iir then
         Error_Msg_Sem
           (+Assoc, "individual association of %n conflicts with that at %l",
            (+Get_Interface_Of_Formal (Get_Formal (Assoc)), +Prev));
      else
         Set_Associated_Expr (Res_Iass, Assoc);
      end if;
   end Add_Individual_Association;

   procedure Finish_Individual_Assoc_Array_Subtype
     (Assoc : Iir; Atype : Iir; Dim : Positive)
   is
      Index_Tlist : constant Iir_List := Get_Index_Subtype_List (Atype);
      Nbr_Dims : constant Natural := Get_Nbr_Elements (Index_Tlist);
      Index_Type : constant Iir := Get_Nth_Element (Index_Tlist, Dim - 1);
      Low, High : Iir;
      Chain : Iir;
      El : Iir;
   begin
      Chain := Get_Individual_Association_Chain (Assoc);
      Sem_Check_Continuous_Choices
        (Chain, Index_Type, False, Get_Location (Assoc), Low, High);
      Set_Individual_Association_Chain (Assoc, Chain);
      if Dim < Nbr_Dims then
         El := Chain;
         while El /= Null_Iir loop
            pragma Assert (Get_Kind (El) = Iir_Kind_Choice_By_Expression);
            Finish_Individual_Assoc_Array_Subtype
              (Get_Associated_Expr (El), Atype, Dim + 1);
            El := Get_Chain (El);
         end loop;
      end if;
   end Finish_Individual_Assoc_Array_Subtype;

   procedure Finish_Individual_Assoc_Array
     (Actual : Iir; Assoc : Iir; Dim : Natural)
   is
      Actual_Type : Iir;
      Actual_Index : Iir;
      Base_Type : Iir;
      Base_Index : Iir;
      Low, High : Iir;
      Chain : Iir;
   begin
      Actual_Type := Get_Actual_Type (Actual);
      Actual_Index := Get_Nth_Element (Get_Index_Subtype_List (Actual_Type),
                                       Dim - 1);
      if Actual_Index /= Null_Iir then
         Base_Index := Actual_Index;
      else
         Base_Type := Get_Base_Type (Actual_Type);
         Base_Index := Get_Index_Type (Base_Type, Dim - 1);
      end if;
      Chain := Get_Individual_Association_Chain (Assoc);
      Sem_Choices_Range
        (Chain, Base_Index, True, False, Get_Location (Assoc), Low, High);
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
            Set_Base_Type (Actual_Index, Get_Base_Type (Base_Index));
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
               when Iir_To =>
                  Set_Left_Limit (Index_Subtype_Constraint, Low);
                  Set_Left_Limit_Expr (Index_Subtype_Constraint, Low);
                  Set_Right_Limit (Index_Subtype_Constraint, High);
                  Set_Right_Limit_Expr (Index_Subtype_Constraint, High);
               when Iir_Downto =>
                  Set_Left_Limit (Index_Subtype_Constraint, High);
                  Set_Left_Limit_Expr (Index_Subtype_Constraint, High);
                  Set_Right_Limit (Index_Subtype_Constraint, Low);
                  Set_Right_Limit_Expr (Index_Subtype_Constraint, Low);
            end case;
            Set_Expr_Staticness (Index_Subtype_Constraint, Locally);
            Append_Element (Get_Index_Subtype_List (Actual_Type),
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
   end Finish_Individual_Assoc_Array;

   procedure Finish_Individual_Assoc_Record (Assoc : Iir; Atype : Iir)
   is
      El_List : constant Iir_List := Get_Elements_Declaration_List (Atype);
      Matches : Iir_Array (0 .. Get_Nbr_Elements (El_List) - 1);
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
         end if;
      end loop;

      if Get_Constraint_State (Atype) /= Fully_Constrained then
         --  Some (sub-)elements are unbounded, create a bounded subtype.
         declare
            Ntype : Iir;
            Nel_List : Iir_List;
            Nrec_El : Iir;
            Rec_El_Type : Iir;
            Staticness : Iir_Staticness;
         begin
            Ntype := Create_Iir (Iir_Kind_Record_Subtype_Definition);
            Location_Copy (Ntype, Assoc);
            Set_Base_Type (Ntype, Get_Base_Type (Atype));
            if Get_Kind (Atype) = Iir_Kind_Record_Subtype_Definition then
               Set_Resolution_Indication
                 (Ntype, Get_Resolution_Indication (Atype));
            end if;
            Nel_List := Create_Iir_List;
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
                  Set_Base_Element_Declaration
                    (Nrec_El, Get_Base_Element_Declaration (Rec_El));
                  Set_Element_Position
                    (Nrec_El, Get_Element_Position (Rec_El));
                  Ch := Get_Associated_Expr (Ch);
                  Set_Type (Nrec_El, Get_Type (Get_Actual (Ch)));
               end if;
               Staticness := Min (Staticness,
                                  Get_Type_Staticness (Get_Type (Nrec_El)));
               Append_Element (Nel_List, Nrec_El);
            end loop;
            Set_Type_Staticness (Ntype, Staticness);
            Set_Constraint_State (Ntype, Fully_Constrained);

            Set_Actual_Type (Assoc, Ntype);
         end;
      else
         Set_Actual_Type (Assoc, Atype);
      end if;
   end Finish_Individual_Assoc_Record;

   --  Free recursively all the choices of ASSOC.
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

   --  Called by sem_individual_association to finish the analyze of
   --  individual association ASSOC: compute bounds, detect missing elements.
   procedure Finish_Individual_Association (Assoc : Iir)
   is
      Formal : Iir;
      Atype : Iir;
   begin
      --  Guard.
      if Assoc = Null_Iir or else Get_Choice_Staticness (Assoc) /= Locally then
         return;
      end if;

      Formal := Get_Interface_Of_Formal (Get_Formal (Assoc));
      Atype := Get_Type (Formal);
      Set_Whole_Association_Flag (Assoc, True);

      case Get_Kind (Atype) is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition =>
            if Get_Constraint_State (Atype) = Fully_Constrained then
               Finish_Individual_Assoc_Array_Subtype (Assoc, Atype, 1);
               Set_Actual_Type (Assoc, Atype);
            else
               Atype := Create_Array_Subtype (Atype, Get_Location (Assoc));
               Set_Index_Constraint_Flag (Atype, True);
               Set_Constraint_State (Atype, Fully_Constrained);
               Set_Actual_Type (Assoc, Atype);
               Set_Actual_Type_Definition (Assoc, Atype);
               Finish_Individual_Assoc_Array (Assoc, Assoc, 1);
            end if;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            Finish_Individual_Assoc_Record (Assoc, Atype);
         when others =>
            Error_Kind ("finish_individual_association", Atype);
      end case;

      --  Free the hierarchy, keep only the top individual association.
      Clean_Individual_Association (Assoc);
   end Finish_Individual_Association;

   --  Sem individual associations of ASSOCS:
   --  Add an Iir_Kind_Association_Element_By_Individual before each
   --  group of individual association for the same formal, and call
   --  Finish_Individual_Association with each of these added nodes.
   --
   --  The purpose of By_Individual association is to have the type of the
   --  actual (might be an array subtype), and also to be sure that all
   --  sub-elements are associated.  For that a tree is created.  The tree is
   --  rooted by the top Association_Element_By_Individual, which contains a
   --  chain of choices (like the aggregate).  The child of a choice is either
   --  an Association_Element written by the user, or a new subtree rooted
   --  by another Association_Element_By_Individual.  The tree doesn't
   --  follow all the ownership rules: the formal of sub association_element
   --  are directly set to the association, and the associated_expr of the
   --  choices are directly set to formals.
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
            --  New formal name, sem the current assoc.
            Finish_Individual_Association (Iassoc);
            Cur_Iface := Formal;
            Iassoc := Null_Iir;
         end if;
         if Get_Whole_Association_Flag (Assoc) = False then
            --  New individual association.
            if Iassoc = Null_Iir then
               Iassoc :=
                 Create_Iir (Iir_Kind_Association_Element_By_Individual);
               Location_Copy (Iassoc, Assoc);
               Set_Choice_Staticness (Iassoc, Locally);
               pragma Assert (Cur_Iface /= Null_Iir);
               Set_Formal (Iassoc, Build_Simple_Name (Cur_Iface, Iassoc));
               --  Insert IASSOC.
               if Prev_Assoc = Null_Iir then
                  Assoc_Chain := Iassoc;
               else
                  Set_Chain (Prev_Assoc, Iassoc);
               end if;
               Set_Chain (Iassoc, Assoc);
            end if;
            Add_Individual_Association (Iassoc, Assoc);
         end if;
         Prev_Assoc := Assoc;
         Assoc := Get_Chain (Assoc);
      end loop;
      --  There is maybe a remaining iassoc.
      Finish_Individual_Association (Iassoc);
   end Sem_Individual_Association;

   function Is_Conversion_Function (Assoc_Chain : Iir) return Boolean
   is
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

   function Is_Expanded_Name (Name : Iir) return Boolean
   is
      Pfx : Iir;
   begin
      Pfx := Name;
      loop
         case Get_Kind (Pfx) is
            when Iir_Kind_Simple_Name =>
               return True;
            when Iir_Kind_Selected_Name =>
               Pfx := Get_Prefix (Pfx);
            when others =>
               return False;
         end case;
      end loop;
   end Is_Expanded_Name;

   function Extract_Type_Of_Conversions (Convs : Iir) return Iir
   is
      --  Return TRUE iff FUNC is valid as a conversion function/type.
      function Extract_Type_Of_Conversion (Func : Iir) return Iir is
      begin
         case Get_Kind (Func) is
            when Iir_Kind_Function_Declaration =>
               if Is_Chain_Length_One (Get_Interface_Declaration_Chain (Func))
               then
                  return Get_Type (Func);
               else
                  return Null_Iir;
               end if;
            when Iir_Kind_Type_Declaration
              | Iir_Kind_Subtype_Declaration =>
               if Flags.Vhdl_Std = Vhdl_87 then
                  return Null_Iir;
               end if;
               return Get_Type (Func);
            when others =>
               return Null_Iir;
         end case;
      end Extract_Type_Of_Conversion;

      Res_List : Iir_List;
      Ov_List : Iir_List;
      El : Iir;
      Conv_Type : Iir;
   begin
      if not Is_Overload_List (Convs) then
         return Extract_Type_Of_Conversion (Convs);
      else
         Ov_List := Get_Overload_List (Convs);
         Res_List := Create_Iir_List;
         for I in Natural loop
            El := Get_Nth_Element (Ov_List, I);
            exit when El = Null_Iir;
            Conv_Type := Extract_Type_Of_Conversion (El);
            if Conv_Type /= Null_Iir then
               Add_Element (Res_List, Conv_Type);
            end if;
         end loop;
         return Simplify_Overload_List (Res_List);
      end if;
   end Extract_Type_Of_Conversions;

   --  ASSOC is an association element not analyzed and whose formal is a
   --  parenthesis name.  Try to extract a conversion function/type.  In case
   --  of success, return a new association element.  In case of failure,
   --  return NULL_IIR.
   function Sem_Formal_Conversion (Assoc : Iir) return Iir
   is
      Formal : constant Iir := Get_Formal (Assoc);
      Assoc_Chain : constant Iir := Get_Association_Chain (Formal);
      Res : Iir;
      Conv : Iir;
      Name : Iir;
      Conv_Func : Iir;
      Conv_Type : Iir;
   begin
      --  Nothing to do if the formal isn't a conversion.
      if not Is_Conversion_Function (Assoc_Chain) then
         return Null_Iir;
      end if;

      --  Both the conversion function and the formal name must be names.
      Conv := Get_Prefix (Formal);
      --  FIXME: what about operator names (such as "not").
      if Get_Kind (Conv) /= Iir_Kind_Simple_Name
        and then not Is_Expanded_Name (Conv)
      then
         return Null_Iir;
      end if;
      Name := Get_Actual (Assoc_Chain);
      if Get_Kind (Name) not in Iir_Kinds_Name then
         return Null_Iir;
      end if;

      Sem_Name_Soft (Conv);
      Conv_Func := Get_Named_Entity (Conv);
      if Get_Kind (Conv_Func) = Iir_Kind_Error then
         Conv_Type := Null_Iir;
      else
         Conv_Type := Extract_Type_Of_Conversions (Conv_Func);
      end if;
      if Conv_Type = Null_Iir then
         Sem_Name_Clean (Conv);
         return Null_Iir;
      end if;
      Set_Type (Conv, Conv_Type);

      --  Create a new association with a conversion function.
      Res := Create_Iir (Iir_Kind_Association_Element_By_Expression);
      Set_Out_Conversion (Res, Conv);
      Set_Formal (Res, Name);
      Set_Actual (Res, Get_Actual (Assoc));
      return Res;
   end Sem_Formal_Conversion;

   --  NAME is the formal name of an association, without any conversion
   --  function or type.
   --  Try to analyze NAME with INTERFACE.
   --  In case of success, set PREFIX to the most prefix of NAME and NAME_TYPE
   --  to the type of NAME.
   --  In case of failure, set NAME_TYPE to NULL_IIR.
   procedure Sem_Formal_Name (Name : Iir;
                              Inter : Iir;
                              Prefix : out Iir;
                              Name_Type : out Iir)
   is
      Base_Type : Iir;
      Rec_El : Iir;
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name =>
            if Get_Identifier (Name) = Get_Identifier (Inter) then
               Prefix := Name;
               Name_Type := Get_Type (Inter);
            else
               Name_Type := Null_Iir;
            end if;
            return;
         when Iir_Kind_Selected_Name =>
            Sem_Formal_Name (Get_Prefix (Name), Inter, Prefix, Name_Type);
            if Name_Type = Null_Iir then
               return;
            end if;
            Base_Type := Get_Base_Type (Name_Type);
            if Get_Kind (Base_Type) /= Iir_Kind_Record_Type_Definition then
               Name_Type := Null_Iir;
               return;
            end if;
            Rec_El := Find_Name_In_List
              (Get_Elements_Declaration_List (Base_Type),
               Get_Identifier (Name));
            if Rec_El = Null_Iir then
               Name_Type := Null_Iir;
               return;
            end if;
            Name_Type := Get_Type (Rec_El);
            return;
         when Iir_Kind_Parenthesis_Name =>
            --  More difficult: slice or indexed array.
            Sem_Formal_Name (Get_Prefix (Name), Inter, Prefix, Name_Type);
            if Name_Type = Null_Iir then
               return;
            end if;
            Base_Type := Get_Base_Type (Name_Type);
            if Get_Kind (Base_Type) /= Iir_Kind_Array_Type_Definition then
               Name_Type := Null_Iir;
               return;
            end if;
            declare
               Chain : Iir;
               Index_List : Iir_List;
               Idx : Iir;
            begin
               Chain := Get_Association_Chain (Name);
               Index_List := Get_Index_Subtype_List (Base_Type);
               --  Check for matching length.
               if Get_Chain_Length (Chain) /= Get_Nbr_Elements (Index_List)
               then
                  Name_Type := Null_Iir;
                  return;
               end if;
               if Get_Kind (Chain)
                 /= Iir_Kind_Association_Element_By_Expression
               then
                  Name_Type := Null_Iir;
                  return;
               end if;
               Idx := Get_Actual (Chain);
               if (not Is_Chain_Length_One (Chain))
                 or else (Get_Kind (Idx) /= Iir_Kind_Range_Expression
                          and then not Is_Range_Attribute_Name (Idx))
               --  FIXME: what about subtype !
               then
                  --  Indexed name.
                  Name_Type := Get_Element_Subtype (Base_Type);
                  return;
               end if;
               --  Slice.
               return;
            end;
         when others =>
            Error_Kind ("sem_formal_name", Name);
      end case;
   end Sem_Formal_Name;

   --  Return a type or a list of types for a formal expression FORMAL
   --   corresponding to INTERFACE.  Possible cases are:
   --  * FORMAL is the simple name with the same identifier as INTERFACE,
   --    FORMAL_TYPE is set to the type of INTERFACE and CONV_TYPE is set
   --    to NULL_IIR.
   --  * FORMAL is a selected, indexed or slice name whose extreme prefix is
   --    a simple name with the same identifier as INTERFACE, FORMAL_TYPE
   --    is set to the type of the name, and CONV_TYPE is set to NULL_IIR.
   --  * FORMAL is a function call, whose only argument is an
   --    association_element_by_expression, whose actual is a name
   --    whose prefix is the same identifier as INTERFACE (note, since FORMAL
   --    is not analyzed, this is parenthesis name), CONV_TYPE is set to
   --    the type or list of type of return type of conversion functions and
   --    FORMAL_TYPE is set to the type of the name.
   --  * otherwise, FORMAL cannot match INTERFACE and both FORMAL_TYPE and
   --    CONV_TYPE are set to NULL_IIR.
   --  If FINISH is true, the simple name is replaced by INTERFACE.

   type Param_Assoc_Type is (None, Open, Individual, Whole);

   function Sem_Formal (Formal : Iir; Inter : Iir) return Param_Assoc_Type
   is
      Prefix : Iir;
      Formal_Type : Iir;
   begin
      case Get_Kind (Formal) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Operator_Symbol =>
            --  Certainly the most common case: FORMAL_NAME => VAL.
            --  It is also the easiest.  So, handle it completly now.
            if Get_Identifier (Formal) = Get_Identifier (Inter) then
               Formal_Type := Get_Type (Inter);
               Set_Named_Entity (Formal, Inter);
               Set_Type (Formal, Formal_Type);
               Set_Base_Name (Formal, Inter);
               return Whole;
            end if;
            return None;
         when Iir_Kind_Selected_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Parenthesis_Name =>
            null;
         when others =>
            --  Should have been caught by sem_association_list.
            Error_Kind ("sem_formal", Formal);
      end case;
      --  Check for a sub-element.
      Sem_Formal_Name (Formal, Inter, Prefix, Formal_Type);
      if Formal_Type /= Null_Iir then
         Set_Type (Formal, Formal_Type);
         Set_Named_Entity (Prefix, Inter);
         return Individual;
      else
         return None;
      end if;
   end Sem_Formal;

   function Is_Valid_Conversion
     (Func : Iir; Res_Base_Type : Iir; Param_Base_Type : Iir) return Boolean
   is
      R_Type : Iir;
      P_Type : Iir;
   begin
      case Get_Kind (Func) is
         when Iir_Kind_Function_Declaration =>
            R_Type := Get_Type (Func);
            P_Type := Get_Type (Get_Interface_Declaration_Chain (Func));
            if Get_Base_Type (R_Type) = Res_Base_Type
              and then Get_Base_Type (P_Type) = Param_Base_Type
            then
               return True;
            else
               return False;
            end if;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            R_Type := Get_Type (Func);
            if Get_Base_Type (R_Type) = Res_Base_Type
              and then Are_Types_Closely_Related (R_Type, Param_Base_Type)
            then
               return True;
            else
               return False;
            end if;
         when Iir_Kind_Function_Call =>
            return Is_Valid_Conversion (Get_Implementation (Func),
                                        Res_Base_Type, Param_Base_Type);
         when Iir_Kind_Type_Conversion =>
            return Is_Valid_Conversion (Get_Type_Mark (Func),
                                        Res_Base_Type, Param_Base_Type);
         when Iir_Kinds_Denoting_Name =>
            return Is_Valid_Conversion (Get_Named_Entity (Func),
                                        Res_Base_Type, Param_Base_Type);
         when others =>
            Error_Kind ("is_valid_conversion(2)", Func);
      end case;
   end Is_Valid_Conversion;

   function Extract_Conversion
     (Conv : Iir; Res_Type : Iir; Param_Type : Iir; Loc : Iir) return Iir
   is
      List : Iir_List;
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
         for I in Natural loop
            El := Get_Nth_Element (List, I);
            exit when El = Null_Iir;
            if Is_Valid_Conversion (El, Res_Base_Type, Param_Base_Type) then
               if Res /= Null_Iir then
                  raise Internal_Error;
               end if;
               Free_Iir (Conv);
               Res := El;
            end if;
         end loop;
      else
         if Is_Valid_Conversion (Conv, Res_Base_Type, Param_Base_Type) then
            Res := Conv;
         else
            Res := Null_Iir;
            Error_Msg_Sem (+Loc, "conversion function or type does not match");
         end if;
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
      Res : Iir;
   begin
      if Conv = Null_Iir then
         return Null_Iir;
      end if;
      Func := Extract_Conversion (Get_Named_Entity (Conv),
                                  Res_Type, Param_Type, Conv);
      if Func = Null_Iir then
         return Null_Iir;
      end if;
      pragma Assert (Get_Kind (Conv) in Iir_Kinds_Denoting_Name);
      Set_Named_Entity (Conv, Func);

      case Get_Kind (Func) is
         when Iir_Kind_Function_Declaration =>
            Res := Create_Iir (Iir_Kind_Function_Call);
            Location_Copy (Res, Conv);
            Set_Implementation (Res, Func);
            Set_Prefix (Res, Conv);
            Set_Base_Name (Res, Res);
            Set_Parameter_Association_Chain (Res, Null_Iir);
            Set_Type (Res, Get_Return_Type (Func));
            Set_Expr_Staticness (Res, None);
            Mark_Subprogram_Used (Func);
         when Iir_Kind_Subtype_Declaration
           | Iir_Kind_Type_Declaration =>
            Res := Create_Iir (Iir_Kind_Type_Conversion);
            Location_Copy (Res, Conv);
            Set_Type_Mark (Res, Conv);
            Set_Type (Res, Get_Type (Func));
            Set_Expression (Res, Null_Iir);
            Set_Expr_Staticness (Res, None);
         when others =>
            Error_Kind ("extract_out_conversion", Res);
      end case;
      Xrefs.Xref_Name (Conv);
      return Res;
   end Extract_Out_Conversion;

   procedure Sem_Association_Open
     (Assoc : Iir;
      Inter : Iir;
      Finish : Boolean;
      Match : out Compatibility_Level)
   is
      Formal : Iir;
      Assoc_Kind : Param_Assoc_Type;
   begin
      Formal := Get_Formal (Assoc);

      if Formal /= Null_Iir then
         Assoc_Kind := Sem_Formal (Formal, Inter);
         if Assoc_Kind = None then
            Match := Not_Compatible;
            return;
         end if;
         Set_Whole_Association_Flag (Assoc, Assoc_Kind = Whole);
         if Finish then
            Sem_Name (Formal);
            Formal := Finish_Sem_Name (Formal);
            Set_Formal (Assoc, Formal);
            if Get_Kind (Formal) in Iir_Kinds_Denoting_Name
              and then Is_Error (Get_Named_Entity (Formal))
            then
               Match := Not_Compatible;
               return;
            end if;

            --  LRM 4.3.3.2  Associations lists
            --  It is an error if an actual of open is associated with a
            --  formal that is associated individually.
            if Assoc_Kind = Individual then
               Error_Msg_Sem
                 (+Assoc, "cannot associate individually with open");
            end if;
         end if;
      else
         Set_Whole_Association_Flag (Assoc, True);
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
         Set_Named_Entity (Formal, Inter);
         Set_Base_Name (Formal, Inter);
         Xrefs.Xref_Ref (Formal, Inter);
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
      --     then the instantiaed package denotes by the actual may be any
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
         if Get_Kind (Decl) /= Iir_Kind_Function_Declaration then
            return False;
         end if;
         --  That returns a boolean.
         if (Get_Base_Type (Get_Return_Type (Decl))
               /= Std_Package.Boolean_Type_Definition)
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
               El, R : Iir;
            begin
               Nbr_Errors := 0;
               R := Null_Iir;
               List := Get_Overload_List (Res);
               for I in Natural loop
                  El := Get_Nth_Element (List, I);
                  exit when El = Null_Iir;
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
               end loop;
               if Is_Null (R) then
                  Error_Msg_Sem
                    (+Assoc, "no matching name for %n", +Inter);
                  if True then
                     Error_Msg_Sem
                       (+Assoc, " these names were incompatible:");
                     for I in Natural loop
                        El := Get_Nth_Element (List, I);
                        exit when El = Null_Iir;
                        Error_Msg_Sem
                          (+Assoc, " %n declared at %l", (+El, +El));
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
      Xrefs.Xref_Name (Actual);
      Set_Use_Flag (Res, True);
   end Sem_Association_Subprogram;

   --  Associate ASSOC with interface INTERFACE
   --  This sets MATCH.
   procedure Sem_Association_By_Expression
     (Assoc : Iir;
      Inter : Iir;
      Finish : Boolean;
      Match : out Compatibility_Level)
   is
      Formal : Iir;
      Formal_Type : Iir;
      Actual: Iir;
      Out_Conv, In_Conv : Iir;
      Expr : Iir;
      Res_Type : Iir;
      Assoc_Kind : Param_Assoc_Type;
   begin
      Formal := Get_Formal (Assoc);

      --  Pre-analyze formal and extract out conversion.
      if Formal /= Null_Iir then
         Assoc_Kind := Sem_Formal (Formal, Inter);
         if Assoc_Kind = None then
            Match := Not_Compatible;
            return;
         end if;
         Set_Whole_Association_Flag (Assoc, Assoc_Kind = Whole);
         Formal := Get_Formal (Assoc);

         Out_Conv := Get_Out_Conversion (Assoc);
      else
         Set_Whole_Association_Flag (Assoc, True);
         Out_Conv := Null_Iir;
         Formal := Inter;
      end if;
      Formal_Type := Get_Type (Formal);

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
         if Finish then
            Error_Msg_Sem (+Assoc, "can't associate %n with %n",
                           (+Actual, +Inter), Cont => True);
            Error_Msg_Sem
              (+Assoc, "(type of %n is " & Disp_Type_Of (Actual) & ")",
               (1 => +Actual), Cont => True);
            Error_Msg_Sem
              (+Inter, "(type of %n is " & Disp_Type_Of (Inter) & ")", +Inter);
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

      --  Analyze formal.
      if Get_Formal (Assoc) /= Null_Iir then
         Set_Type (Formal, Null_Iir);
         Sem_Name (Formal);
         Expr := Get_Named_Entity (Formal);
         if Get_Kind (Expr) = Iir_Kind_Error then
            return;
         end if;
         Formal := Finish_Sem_Name (Formal);
         Set_Formal (Assoc, Formal);
         Formal_Type := Get_Type (Expr);
         if Out_Conv = Null_Iir and In_Conv = Null_Iir then
            Res_Type := Formal_Type;
         end if;
      end if;

      --  LRM08 6.5.7 Association lists
      --  The formal part of a named association element may be in the form of
      --  a function call [...] if and only if the formal is an interface
      --  object, the mode of the formal is OUT, INOUT, BUFFER or LINKAGE [...]
      Set_Out_Conversion (Assoc, Out_Conv);
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
      Set_In_Conversion (Assoc, In_Conv);
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
            if not Eval_Is_In_Bound (Expr, Formal_Type) then
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
                              Finish : Boolean;
                              Match : out Compatibility_Level) is
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Association_Element_Open =>
            Sem_Association_Open (Assoc, Inter, Finish, Match);

         when Iir_Kind_Association_Element_By_Expression =>
            Sem_Association_By_Expression (Assoc, Inter, Finish, Match);

         when Iir_Kind_Association_Element_Package =>
            Sem_Association_Package (Assoc, Inter, Finish, Match);

         when Iir_Kind_Association_Element_Type =>
            Sem_Association_Type (Assoc, Inter, Finish, Match);

         when Iir_Kind_Association_Element_Subprogram =>
            Sem_Association_Subprogram (Assoc, Inter, Finish, Match);

         when others =>
            Error_Kind ("sem_assocation", Assoc);
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
      --  Set POS and INTERFACE to *the* matching interface if any of ASSOC.
      procedure Search_Interface (Assoc : Iir;
                                  Inter : out Iir;
                                  Pos : out Integer)
      is
         I_Match : Compatibility_Level;
      begin
         Inter := Interface_Chain;
         Pos := 0;
         while Inter /= Null_Iir loop
            -- Formal assoc is not necessarily a simple name, it may
            -- be a conversion function, or even an indexed or
            -- selected name.
            Sem_Association (Assoc, Inter, False, I_Match);
            if I_Match /= Not_Compatible then
               return;
            end if;
            Inter := Get_Chain (Inter);
            Pos := Pos + 1;
         end loop;
      end Search_Interface;

      Assoc: Iir;
      Inter: Iir;

      type Bool_Array is array (Natural range <>) of Param_Assoc_Type;
      Nbr_Arg: constant Natural := Get_Chain_Length (Interface_Chain);
      Arg_Matched: Bool_Array (0 .. Nbr_Arg - 1) := (others => None);

      Last_Individual : Iir;
      Has_Individual : Boolean;
      Pos : Integer;
      Formal : Iir;

      Interface_1 : Iir;
      Pos_1 : Integer;
      Assoc_1 : Iir;
   begin
      Match := Fully_Compatible;
      Has_Individual := False;

      -- Loop on every assoc element, try to match it.
      Inter := Interface_Chain;
      Last_Individual := Null_Iir;
      Pos := 0;

      Assoc := Assoc_Chain;
      while Assoc /= Null_Iir loop
         Formal := Get_Formal (Assoc);
         if Formal = Null_Iir then
            -- Positional argument.
            if Pos < 0 then
               --  Positional after named argument.  Already caught by
               --  Sem_Actual_Of_Association_Chain (because it is called only
               --  once, while sem_association_chain may be called several
               --  times).
               Match := Not_Compatible;
               return;
            end if;
            -- Try to match actual of ASSOC with the interface.
            if Inter = Null_Iir then
               if Finish then
                  Error_Msg_Sem (+Assoc, "too many actuals for %n", +Loc);
               end if;
               Match := Not_Compatible;
               return;
            end if;
            Sem_Association (Assoc, Inter, Finish, Match);
            if Match = Not_Compatible then
               return;
            end if;
            if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
               Arg_Matched (Pos) := Open;
            else
               Arg_Matched (Pos) := Whole;
            end if;
            Set_Whole_Association_Flag (Assoc, True);
            Inter := Get_Chain (Inter);
            Pos := Pos + 1;
         else
            -- FIXME: directly search the formal if finish is true.
            -- Find the Interface.
            case Get_Kind (Formal) is
               when Iir_Kind_Parenthesis_Name =>
                  Assoc_1 := Sem_Formal_Conversion (Assoc);
                  if Assoc_1 /= Null_Iir then
                     Search_Interface (Assoc_1, Interface_1, Pos_1);
                     --  LRM 4.3.2.2  Association Lists
                     --  The formal part of a named element association may be
                     --  in the form of a function call, [...], if and only
                     --  if the mode of the formal is OUT, INOUT, BUFFER, or
                     --  LINKAGE, and the actual is not OPEN.
                     if Interface_1 = Null_Iir
                       or else Get_Mode (Interface_1) = Iir_In_Mode
                     then
                        Sem_Name_Clean (Get_Out_Conversion (Assoc_1));
                        Free_Iir (Assoc_1);
                        Assoc_1 := Null_Iir;
                     end if;
                  end if;
                  Search_Interface (Assoc, Inter, Pos);
                  if Inter = Null_Iir then
                     if Assoc_1 /= Null_Iir then
                        Inter := Interface_1;
                        Pos := Pos_1;
                        Free_Parenthesis_Name
                          (Get_Formal (Assoc), Get_Out_Conversion (Assoc_1));
                        Set_Formal (Assoc, Get_Formal (Assoc_1));
                        Set_Out_Conversion
                          (Assoc, Get_Out_Conversion (Assoc_1));
                        Set_Whole_Association_Flag
                          (Assoc, Get_Whole_Association_Flag (Assoc_1));
                        Free_Iir (Assoc_1);
                     end if;
                  else
                     if Assoc_1 /= Null_Iir then
                        raise Internal_Error;
                     end if;
                  end if;
               when others =>
                  Search_Interface (Assoc, Inter, Pos);
            end case;

            if Inter /= Null_Iir then
               if Get_Whole_Association_Flag (Assoc) then
                  --  Whole association.
                  Last_Individual := Null_Iir;
                  if Arg_Matched (Pos) = None then
                     if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open
                     then
                        Arg_Matched (Pos) := Open;
                     else
                        Arg_Matched (Pos) := Whole;
                     end if;
                  else
                     if Finish then
                        Error_Msg_Sem
                          (+Assoc, "%n already associated", +Inter);
                     end if;
                     Match := Not_Compatible;
                     return;
                  end if;
               else
                  --  Individual association.
                  Has_Individual := True;
                  if Arg_Matched (Pos) /= Whole then
                     if Finish
                       and then Arg_Matched (Pos) = Individual
                       and then Last_Individual /= Inter
                     then
                        Error_Msg_Sem
                          (+Assoc,
                           "non consecutive individual association for %n",
                           +Inter);
                        Match := Not_Compatible;
                        return;
                     end if;
                     Last_Individual := Inter;
                     Arg_Matched (Pos) := Individual;
                  else
                     if Finish then
                        Error_Msg_Sem
                          (+Assoc, "%n already associated", +Inter);
                        Match := Not_Compatible;
                        return;
                     end if;
                  end if;
               end if;
               if Finish then
                  Sem_Association (Assoc, Inter, True, Match);
                  --  MATCH can be Not_Compatible due to errors.
               end if;
            else
               -- Not found.
               if Finish then
                  --  FIXME: display the name of subprg or component/entity.
                  --  FIXME: fetch the interface (for parenthesis_name).
                  Error_Msg_Sem (+Assoc, "no interface for %n in association",
                                 +Get_Formal (Assoc));
               end if;
               Match := Not_Compatible;
               return;
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;

      if Finish and then Has_Individual then
         Sem_Individual_Association (Assoc_Chain);
      end if;

      if Missing = Missing_Allowed then
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
      --  It is an error if a port of any mode other than IN is unconnected
      --  or unassociated and its type is an unconstrained array type.

      --  LRM08 6.5.6.2 Generic clauses
      --  It is an error if no such actual [instantiated package] is specified
      --  for a given formal generic package (either because the formal generic
      --  is unassociated or because the actual is OPEN).

      Inter := Interface_Chain;
      Pos := 0;
      while Inter /= Null_Iir loop
         if Arg_Matched (Pos) <= Open then
            case Get_Kind (Inter) is
               when Iir_Kinds_Interface_Object_Declaration =>
                  if Get_Default_Value (Inter) = Null_Iir then
                     case Missing is
                        when Missing_Parameter
                          | Missing_Generic =>
                           if Finish then
                              Error_Msg_Sem (+Loc, "no actual for %n", +Inter);
                           end if;
                           Match := Not_Compatible;
                           return;
                        when Missing_Port =>
                           case Get_Mode (Inter) is
                              when Iir_In_Mode =>
                                 if not Finish then
                                    raise Internal_Error;
                                 end if;
                                 Error_Msg_Sem
                                   (+Loc,
                                    "%n of mode IN must be connected", +Inter);
                                 Match := Not_Compatible;
                                 return;
                              when Iir_Out_Mode
                                | Iir_Linkage_Mode
                                | Iir_Inout_Mode
                                | Iir_Buffer_Mode =>
                                 if not Finish then
                                    raise Internal_Error;
                                 end if;
                                 if not Is_Fully_Constrained_Type
                                   (Get_Type (Inter))
                                 then
                                    Error_Msg_Sem
                                      (+Loc,
                                       "unconstrained %n must be connected",
                                       +Inter);
                                    Match := Not_Compatible;
                                    return;
                                 end if;
                              when Iir_Unknown_Mode =>
                                 raise Internal_Error;
                           end case;
                        when Missing_Allowed =>
                           null;
                     end case;
                  end if;
               when Iir_Kind_Interface_Package_Declaration
                 | Iir_Kind_Interface_Function_Declaration
                 | Iir_Kind_Interface_Procedure_Declaration =>
                  Error_Msg_Sem (+Loc, "%n must be associated", +Inter);
                  Match := Not_Compatible;
               when others =>
                  Error_Kind ("sem_association_chain", Inter);
            end case;
         end if;

         --  Clear associated type of interface type.
         if Get_Kind (Inter) = Iir_Kind_Interface_Type_Declaration then
            Set_Associated_Type (Get_Type (Inter), Null_Iir);
         end if;

         Inter := Get_Chain (Inter);
         Pos := Pos + 1;
      end loop;
   end Sem_Association_Chain;
end Sem_Assocs;
