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
with Sem_Names; use Sem_Names;
with Sem_Expr; use Sem_Expr;
with Iir_Chains; use Iir_Chains;
with Xrefs;

package body Sem_Assocs is
   --  Semantize all arguments of ASSOC_CHAIN
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
      -- Semantize all arguments
      -- OK is false if there is an error during semantic of one of the
      -- argument, but continue semantisation.
      Has_Named := False;
      Ok := True;
      Assoc := Assoc_Chain;
      while Assoc /= Null_Iir loop
         Formal := Get_Formal (Assoc);
         if Formal /= Null_Iir then
            Has_Named := True;
            --  FIXME: check FORMAL is well composed.
         elsif Has_Named then
            Error_Msg_Sem ("positional argument after named argument", Assoc);
            Ok := False;
         end if;
         if Get_Kind (Assoc) /= Iir_Kind_Association_Element_Open then
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

   function Get_Mode_Name (Mode : Iir_Mode) return String is
   begin
      case Mode is
         when Iir_Unknown_Mode =>
            raise Internal_Error;
         when Iir_Linkage_Mode =>
            return "linkage";
         when Iir_Buffer_Mode =>
            return "buffer";
         when Iir_Out_Mode =>
            return "out";
         when Iir_Inout_Mode =>
            return "inout";
         when Iir_In_Mode =>
            return "in";
      end case;
   end Get_Mode_Name;

   procedure Check_Parameter_Association_Restriction
     (Inter : Iir; Base_Actual : Iir; Loc : Iir)
   is
      Act_Mode : Iir_Mode;
      For_Mode : Iir_Mode;
   begin
      Act_Mode := Get_Mode (Base_Actual);
      For_Mode := Get_Mode (Inter);
      case Get_Mode (Inter) is
         when Iir_In_Mode =>
            if Act_Mode in Iir_In_Modes or Act_Mode = Iir_Buffer_Mode then
               return;
            end if;
         when Iir_Out_Mode =>
            --  FIXME: should buffer also be accepted ?
            if Act_Mode in Iir_Out_Modes or Act_Mode = Iir_Buffer_Mode then
               return;
            end if;
         when Iir_Inout_Mode =>
            if Act_Mode = Iir_Inout_Mode then
               return;
            end if;
         when others =>
            Error_Kind ("check_parameter_association_restriction", Inter);
      end case;
      Error_Msg_Sem
        ("cannot associate an " & Get_Mode_Name (Act_Mode)
         & " object with " & Get_Mode_Name (For_Mode) & " "
         & Disp_Node (Inter), Loc);
   end Check_Parameter_Association_Restriction;

   procedure Check_Subprogram_Associations
     (Inter_Chain : Iir; Assoc_Chain : Iir)
   is
      Assoc : Iir;
      Formal : Iir;
      Formal_Inter : Iir;
      Actual : Iir;
      Prefix : Iir;
      Object : Iir;
      Inter : Iir;
   begin
      Assoc := Assoc_Chain;
      Inter := Inter_Chain;
      while Assoc /= Null_Iir loop
         Formal := Get_Formal (Assoc);
         if Formal = Null_Iir then
            --  Association by position.
            Formal_Inter := Inter;
            Inter := Get_Chain (Inter);
         else
            --  Association by name.
            Formal_Inter := Get_Base_Name (Formal);
            Inter := Null_Iir;
         end if;
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               if Get_Default_Value (Formal_Inter) = Null_Iir then
                  Error_Msg_Sem
                    ("no parameter for " & Disp_Node (Formal_Inter), Assoc);
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
                  when Iir_Kind_Signal_Interface_Declaration =>
                     --  LRM93 2.1.1
                     --  In a subprogram call, the actual designator
                     --  associated with a formal parameter of class
                     --  signal must be a signal.
                     case Get_Kind (Prefix) is
                        when Iir_Kind_Signal_Interface_Declaration
                          | Iir_Kind_Signal_Declaration
                          | Iir_Kind_Guard_Signal_Declaration
                          | Iir_Kinds_Signal_Attribute =>
                           --  LRM93 2.1.1.2
                           --  If an actual signal is associated with
                           --  a signal parameter of any mode, the actual
                           --  must be denoted by a static signal name.
                           if Get_Name_Staticness (Object) < Globally then
                              Error_Msg_Sem
                                ("actual signal must be a static name",
                                 Actual);
                           else
                              --  Inherit has_active_flag.
                              Set_Has_Active_Flag
                                (Prefix, Get_Has_Active_Flag (Formal_Inter));
                           end if;
                        when others =>
                           Error_Msg_Sem
                             ("signal parameter requires a signal expression",
                              Assoc);
                     end case;

                     case Get_Kind (Prefix) is
                        when Iir_Kind_Signal_Interface_Declaration =>
                           Check_Parameter_Association_Restriction
                             (Formal_Inter, Prefix, Assoc);
                        when Iir_Kind_Guard_Signal_Declaration =>
                           if Get_Mode (Formal_Inter) /= Iir_In_Mode then
                              Error_Msg_Sem
                                ("cannot associate a guard signal with "
                                 & Get_Mode_Name (Get_Mode (Formal_Inter))
                                 & " " & Disp_Node (Formal_Inter), Assoc);
                           end if;
                        when Iir_Kinds_Signal_Attribute =>
                           if Get_Mode (Formal_Inter) /= Iir_In_Mode then
                              Error_Msg_Sem
                                ("cannot associate a signal attribute with "
                                 & Get_Mode_Name (Get_Mode (Formal_Inter))
                                 & " " & Disp_Node (Formal_Inter), Assoc);
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
                        Error_Msg_Sem ("conversion are not allowed for "
                                       & "signal parameters", Assoc);
                     end if;
                  when Iir_Kind_Variable_Interface_Declaration =>
                     --  LRM93 2.1.1
                     --  The actual designator associated with a formal of
                     --  class variable must be a variable.
                     case Get_Kind (Prefix) is
                        when Iir_Kind_Variable_Interface_Declaration =>
                           Check_Parameter_Association_Restriction
                             (Formal_Inter, Prefix, Assoc);
                        when Iir_Kind_Variable_Declaration
                          | Iir_Kind_Dereference
                          | Iir_Kind_Implicit_Dereference =>
                           null;
                        when Iir_Kind_File_Interface_Declaration
                          | Iir_Kind_File_Declaration =>
                           --  LRM87 4.3.1.4
                           --  Such an object is a member of the variable
                           --  class of objects;
                           if Flags.Vhdl_Std >= Vhdl_93 then
                              Error_Msg_Sem ("in vhdl93, variable parameter "
                                             & "cannot be a file", Assoc);
                           end if;
                        when others =>
                           Error_Msg_Sem
                             ("variable parameter must be a variable", Assoc);
                     end case;
                  when Iir_Kind_File_Interface_Declaration =>
                     --  LRM93 2.1.1
                     --  The actual designator associated with a formal
                     --  of class file must be a file.
                     case Get_Kind (Prefix) is
                        when Iir_Kind_File_Interface_Declaration
                          | Iir_Kind_File_Declaration =>
                           null;
                        when Iir_Kind_Variable_Declaration
                          | Iir_Kind_Variable_Interface_Declaration =>
                           if Flags.Vhdl_Std >= Vhdl_93 then
                              Error_Msg_Sem ("in vhdl93, file parameter "
                                             & "must be a file", Assoc);
                           end if;
                        when others =>
                           Error_Msg_Sem
                             ("file parameter must be a file", Assoc);
                     end case;

                     --  LRM 2.1.1.3  File parameters
                     --  It is an error if an association element associates
                     --  an actual with a formal parameter of a file type and
                     --  that association element contains a conversion
                     --  function or type conversion.
                     if Get_In_Conversion (Assoc) /= Null_Iir
                       or Get_Out_Conversion (Assoc) /= Null_Iir
                     then
                        Error_Msg_Sem ("conversion are not allowed for "
                                       & "file parameters", Assoc);
                     end if;
                  when Iir_Kind_Constant_Interface_Declaration =>
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
         Assoc := Get_Chain (Assoc);
      end loop;
   end Check_Subprogram_Associations;

   --  Assocs_Right_Map (FORMAL_MODE, ACTUAL_MODE) is true iff it is allowed
   --  to associate a formal port of mode FORMAL_MODE with an actual port of
   --  mode ACTUAL_MODE.
   subtype Iir_Known_Mode is Iir_Mode range Iir_Linkage_Mode .. Iir_In_Mode;
   type Assocs_Right_Map is array (Iir_Known_Mode, Iir_Known_Mode) of Boolean;

   Vhdl93_Assocs_Map : constant Assocs_Right_Map :=
     (Iir_Linkage_Mode => (others => True),
      Iir_Buffer_Mode => (Iir_Buffer_Mode => True, others => False),
      Iir_Out_Mode => (Iir_Out_Mode | Iir_Inout_Mode => True,
                       others => False),
      Iir_Inout_Mode => (Iir_Inout_Mode => True,
                         others => False),
      Iir_In_Mode => (Iir_In_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
                      others => False));

   Vhdl02_Assocs_Map : constant Assocs_Right_Map :=
     (Iir_Linkage_Mode => (others => True),
      Iir_Buffer_Mode => (Iir_Out_Mode | Iir_Inout_Mode
                          | Iir_Buffer_Mode => True,
                          others => False),
      Iir_Out_Mode => (Iir_Out_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
                       others => False),
      Iir_Inout_Mode => (Iir_Inout_Mode | Iir_Buffer_Mode => True,
                         others => False),
      Iir_In_Mode => (Iir_In_Mode | Iir_Inout_Mode | Iir_Buffer_Mode => True,
                      others => False));

   --  Check for restrictions in LRM 1.1.1.2
   --  Return FALSE in case of error.
   function Check_Port_Association_Restriction
     (Formal : Iir_Signal_Interface_Declaration;
      Actual : Iir_Signal_Interface_Declaration;
      Assoc : Iir)
     return Boolean
   is
      Fmode : Iir_Mode;
      Amode : Iir_Mode;
   begin
      Fmode := Get_Mode (Formal);
      Amode := Get_Mode (Actual);
      if Fmode = Iir_Unknown_Mode or Amode = Iir_Unknown_Mode then
         raise Internal_Error;
      end if;

      if Flags.Vhdl_Std < Vhdl_02 then
         if Vhdl93_Assocs_Map (Fmode, Amode) then
            return True;
         end if;
      else
         if Vhdl02_Assocs_Map (Fmode, Amode) then
            return True;
         end if;
      end if;

      Error_Msg_Sem
        ("cannot associate " & Get_Mode_Name (Fmode) & " "
         & Disp_Node (Formal) & " with actual port of mode "
         & Get_Mode_Name (Amode), Assoc);
      return False;
   end Check_Port_Association_Restriction;

   --  Handle indexed name
   --  FORMAL is the formal name to be handled.
   --  SUB_ASSOC is an association_by_individual in which the formal will be
   --   inserted.
   --  Update SUB_ASSOC so that it designates FORMAL.
   procedure Add_Individual_Assoc_Indexed_Name
     (Sub_Assoc : in out Iir; Formal : Iir)
   is
      Choice : Iir;
      Last_Choice : Iir;
      Index_List : Iir_List;
      Index : Iir;
      Nbr : Natural;
   begin
      --  Find element.
      Index_List := Get_Index_List (Formal);
      Nbr := Get_Nbr_Elements (Index_List);
      for I in 0 .. Nbr - 1 loop
         Index := Get_Nth_Element (Index_List, I);

         --  Evaluate index.
         Index := Eval_Expr (Index);
         Replace_Nth_Element (Index_List, I, Index);

         --  Find index in choice list.
         Last_Choice := Null_Iir;
         Choice := Get_Individual_Association_Chain (Sub_Assoc);
         while Choice /= Null_Iir loop
            case Get_Kind (Choice) is
               when Iir_Kind_Choice_By_Expression =>
                  if Eval_Pos (Get_Expression (Choice)) = Eval_Pos (Index) then
                     goto Found;
                  end if;
               when Iir_Kind_Choice_By_Range =>
                  if Eval_Int_In_Range (Eval_Pos (Index),
                                        Get_Expression (Choice))
                  then
                     --  FIXME: overlap.
                     raise Internal_Error;
                  end if;
               when others =>
                  Error_Kind ("add_individual_assoc_index_name", Choice);
            end case;
            Last_Choice := Choice;
            Choice := Get_Chain (Choice);
         end loop;

         --  If not found, append it.
         Choice := Create_Iir (Iir_Kind_Choice_By_Expression);
         Set_Expression (Choice, Index);
         Location_Copy (Choice, Formal);
         if Last_Choice = Null_Iir then
            Set_Individual_Association_Chain (Sub_Assoc, Choice);
         else
            Set_Chain (Last_Choice, Choice);
         end if;

         << Found >> null;

         if I < Nbr - 1 then
            Sub_Assoc := Get_Associated (Choice);
            if Sub_Assoc = Null_Iir then
               Sub_Assoc := Create_Iir
                 (Iir_Kind_Association_Element_By_Individual);
               Location_Copy (Sub_Assoc, Index);
               Set_Associated (Choice, Sub_Assoc);
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
   begin
      --  FIXME: handle cases such as param(5 to 6)(5)

      --  Find element.
      Index := Get_Suffix (Formal);

      --  Evaluate index.
      Index := Eval_Expr (Index);
      Set_Suffix (Formal, Index);

      Choice := Create_Iir (Iir_Kind_Choice_By_Range);
      Location_Copy (Choice, Formal);
      Set_Expression (Choice, Index);
      Set_Chain (Choice, Get_Individual_Association_Chain (Sub_Assoc));
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
      Set_Name (Choice, Get_Selected_Element (Formal));
      Set_Chain (Choice, Get_Individual_Association_Chain (Sub_Assoc));
      Set_Individual_Association_Chain (Sub_Assoc, Choice);

      Sub_Assoc := Choice;
   end Add_Individual_Assoc_Selected_Name;

   procedure Add_Individual_Association_1 (Iassoc : in out Iir; Formal : Iir)
   is
      Sub : Iir;
      Formal_Object : Iir;
   begin
      --  Recurse.
      Formal_Object := Name_To_Object (Formal);
      case Get_Kind (Formal_Object) is
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Element =>
            Add_Individual_Association_1 (Iassoc, Get_Prefix (Formal_Object));
         when Iir_Kinds_Interface_Declaration =>
            return;
         when others =>
            Error_Kind ("add_individual_association_1", Formal);
      end case;

      case Get_Kind (Iassoc) is
         when Iir_Kind_Association_Element_By_Individual =>
            null;
         when Iir_Kind_Choice_By_Expression =>
            Sub := Get_Associated (Iassoc);
            if Sub = Null_Iir then
               Sub := Create_Iir (Iir_Kind_Association_Element_By_Individual);
               Location_Copy (Sub, Formal);
               Set_Formal (Sub, Iassoc);
               Iassoc := Sub;
            else
               case Get_Kind (Sub) is
                  when Iir_Kind_Association_Element_By_Individual =>
                     Iassoc := Sub;
                  when others =>
                     Error_Msg_Sem
                       ("individual association of "
                        & Disp_Node (Get_Associated_Formal (Iassoc))
                        & " conflicts with that at " & Disp_Location (Sub),
                        Formal);
                     return;
               end case;
            end if;
         when others =>
            Error_Kind ("add_individual_association_1(2)", Iassoc);
      end case;

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
   end Add_Individual_Association_1;

   --  Insert ASSOC into the tree of individual assoc rooted by IASSOC.
   procedure Add_Individual_Association (Iassoc : Iir; Assoc : Iir)
   is
      Formal : Iir;
      Iass : Iir;
      Prev : Iir;
   begin
      Formal := Get_Formal (Assoc);
      Iass := Iassoc;
      Add_Individual_Association_1 (Iass, Formal);
      Prev := Get_Associated (Iass);
      if Prev /= Null_Iir then
         Error_Msg_Sem ("individual association of "
                        & Disp_Node (Get_Base_Name (Formal))
                        & " conflicts with that at " & Disp_Location (Prev),
                        Assoc);
      else
         Set_Associated (Iass, Assoc);
      end if;
   end Add_Individual_Association;

   procedure Finish_Individual_Assoc_Array_Subtype (Assoc : Iir; Atype : Iir)
   is
      Index_Tlist : Iir_List;
      Index_Type : Iir;
      Low, High : Iir;
      Chain : Iir;
   begin
      Index_Tlist := Get_Index_Subtype_List (Atype);
      for I in Natural loop
         Index_Type := Get_Nth_Element (Index_Tlist, I);
         exit when Index_Type = Null_Iir;
         Chain := Get_Individual_Association_Chain (Assoc);
         Sem_Choices_Range
           (Chain, Index_Type, False, False, Get_Location (Assoc), Low, High);
         Set_Individual_Association_Chain (Assoc, Chain);
      end loop;
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
         Base_Index := Get_Nth_Element (Get_Index_Subtype_List (Base_Type),
                                        Dim - 1);
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

            case Get_Direction (Index_Constraint) is
               when Iir_To =>
                  Set_Left_Limit (Index_Subtype_Constraint, Low);
                  Set_Right_Limit (Index_Subtype_Constraint, High);
               when Iir_Downto =>
                  Set_Left_Limit (Index_Subtype_Constraint, High);
                  Set_Right_Limit (Index_Subtype_Constraint, Low);
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
               Error_Msg_Sem ("indexes of individual association mismatch",
                              Assoc);
            end if;
         end;
      end if;
   end Finish_Individual_Assoc_Array;

   procedure Finish_Individual_Assoc_Record (Assoc : Iir; Atype : Iir)
   is
      Base_Type : constant Iir_Record_Type_Definition := Get_Base_Type (Atype);
      El_List : constant Iir_List := Get_Elements_Declaration_List (Base_Type);
      Matches : Iir_Array (0 .. Get_Nbr_Elements (El_List) - 1);
      Ch : Iir;
      Pos : Natural;
      Rec_El : Iir;
   begin
      Matches := (others => Null_Iir);
      Ch := Get_Individual_Association_Chain (Assoc);
      while Ch /= Null_Iir loop
         Rec_El := Get_Name (Ch);
         Pos := Natural (Get_Element_Position (Rec_El));
         if Matches (Pos) /= Null_Iir then
            Error_Msg_Sem ("individual " & Disp_Node (Rec_El)
                           & " already associated at "
                           & Disp_Location (Matches (Pos)), Ch);
         else
            Matches (Pos) := Ch;
         end if;
         Ch := Get_Chain (Ch);
      end loop;
      for I in Matches'Range loop
         Rec_El := Get_Nth_Element (El_List, I);
         if Matches (I) = Null_Iir then
            Error_Msg_Sem (Disp_Node (Rec_El) & " not associated", Assoc);
         end if;
      end loop;
      Set_Actual_Type (Assoc, Atype);
   end Finish_Individual_Assoc_Record;

   --  Called by sem_individual_association to finish the semantization of
   --  individual association ASSOC.
   procedure Finish_Individual_Association (Assoc : Iir)
   is
      Formal : Iir;
      Atype : Iir;
   begin
      --  Guard.
      if Assoc = Null_Iir then
         return;
      end if;

      Formal := Get_Associated_Formal (Assoc);
      Atype := Get_Type (Formal);

      case Get_Kind (Atype) is
         when Iir_Kind_Array_Subtype_Definition =>
            Finish_Individual_Assoc_Array_Subtype (Assoc, Atype);
         when Iir_Kind_Array_Type_Definition =>
            Atype := Create_Array_Subtype (Atype, Get_Location (Assoc));
            Set_Index_Constraint_Flag (Atype, True);
            Set_Constraint_State (Atype, Fully_Constrained);
            Set_Actual_Type (Assoc, Atype);
            Finish_Individual_Assoc_Array (Assoc, Assoc, 1);
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            Finish_Individual_Assoc_Record (Assoc, Atype);
         when others =>
            Error_Kind ("finish_individual_association", Atype);
      end case;
   end Finish_Individual_Association;

   --  Sem individual associations of ASSOCS:
   --  Add an Iir_Kind_Association_Element_By_Individual before each
   --  group of individual association for the same formal, and call
   --  Finish_Individual_Association with each of these added nodes.
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
            Formal := Get_Base_Name (Formal);
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
               if Cur_Iface = Null_Iir then
                  raise Internal_Error;
               end if;
               Set_Formal (Iassoc, Cur_Iface);
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

   --  Return TRUE iff FUNC is valid as a conversion function/type.
   function Is_Valid_Conversion (Func : Iir) return Boolean is
   begin
      case Get_Kind (Func) is
         when Iir_Kinds_Function_Declaration =>
            if not Is_Chain_Length_One (Get_Interface_Declaration_Chain (Func))
            then
               return False;
            end if;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            if Flags.Vhdl_Std = Vhdl_87 then
               return False;
            end if;
         when others =>
            return False;
      end case;
      return True;
   end Is_Valid_Conversion;

   function Extract_Type_Of_Conversions (Convs : Iir) return Iir
   is
      Res_List : Iir_List;
      Ov_List : Iir_List;
      El : Iir;
   begin
      if not Is_Overload_List (Convs) then
         if Is_Valid_Conversion (Convs) then
            return Get_Type (Convs);
         else
            return Null_Iir;
         end if;
      else
         Ov_List := Get_Overload_List (Convs);
         Res_List := Create_Iir_List;
         for I in Natural loop
            El := Get_Nth_Element (Ov_List, I);
            exit when El = Null_Iir;
            if Is_Valid_Conversion (El) then
               Add_Element (Res_List, Get_Type (El));
            end if;
         end loop;
         return Simplify_Overload_List (Res_List);
      end if;
   end Extract_Type_Of_Conversions;

   --  ASSOC is an association element not semantized and whose formal is a
   --  parenthesis name.  Try to extract a conversion function/type.  In case
   --  of success, return a new association element.  In case of failure,
   --  return NULL_IIR.
   function Sem_Formal_Conversion (Assoc : Iir) return Iir
   is
      Formal : Iir;
      Assoc_Chain : Iir;
      Res : Iir;
      Conv : Iir;
      Name : Iir;
      Conv_Func : Iir;
      Conv_Type : Iir;
   begin
      Formal := Get_Formal (Assoc);
      Assoc_Chain := Get_Association_Chain (Formal);
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
   --  Try to semantize NAME with INTERFACE.
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
               Get_Suffix_Identifier (Name));
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
   --    is not semantized, this is parenthesis name), CONV_TYPE is set to
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
         when Iir_Kind_Simple_Name =>
            --  Certainly the most common case: FORMAL_NAME => VAL.
            --  It is also the easiest.  So, handle it completly now.
            if Get_Identifier (Formal) = Get_Identifier (Inter) then
               Formal_Type := Get_Type (Inter);
               Set_Named_Entity (Formal, Inter);
               Set_Type (Formal, Formal_Type);
               --Xrefs.Xref_Name (Formal);
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
     (Func : Iir; Res_Base_Type : Iir; Param_Base_Type : Iir)
     return Boolean
   is
      R_Type : Iir;
      P_Type : Iir;
   begin
      case Get_Kind (Func) is
         when Iir_Kinds_Function_Declaration =>
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
         when others =>
            Error_Kind ("is_valid_conversion(2)", Func);
      end case;
   end Is_Valid_Conversion;

   function Extract_Conversion
     (Conv : Iir; Res_Type : Iir; Param_Type : Iir; Loc : Iir)
     return Iir
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
            Error_Msg_Sem ("conversion function or type does not match", Loc);
         end if;
      end if;
      return Res;
   end Extract_Conversion;

   function Extract_In_Conversion (Conv : Iir;
                                   Res_Type : Iir; Param_Type : Iir)
                                  return Iir
   is
      Func : Iir;
   begin
      if Conv = Null_Iir then
         return Null_Iir;
      end if;
      Func := Extract_Conversion (Conv, Res_Type, Param_Type, Conv);
      if Func = Null_Iir then
         return Null_Iir;
      end if;
      case Get_Kind (Func) is
         when Iir_Kind_Function_Call
           | Iir_Kind_Type_Conversion =>
            return Func;
         when others =>
            Error_Kind ("extract_in_conversion", Func);
      end case;
   end Extract_In_Conversion;

   function Extract_Out_Conversion (Conv : Iir;
                                    Res_Type : Iir; Param_Type : Iir)
                                   return Iir
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

      case Get_Kind (Func) is
         when Iir_Kinds_Function_Declaration =>
            Res := Create_Iir (Iir_Kind_Function_Call);
            Location_Copy (Res, Conv);
            Set_Implementation (Res, Func);
            Set_Base_Name (Res, Res);
            Set_Parameter_Association_Chain (Res, Null_Iir);
            Set_Type (Res, Get_Return_Type (Func));
            Set_Expr_Staticness (Res, None);
            Set_Use_Flag (Func, True);
         when Iir_Kind_Subtype_Declaration
           | Iir_Kind_Type_Declaration =>
            Res := Create_Iir (Iir_Kind_Type_Conversion);
            Location_Copy (Res, Conv);
            Set_Type_Mark (Res, Func);
            Set_Type (Res, Get_Type (Func));
            Set_Expression (Res, Null_Iir);
            Set_Expr_Staticness (Res, None);
         when others =>
            Error_Kind ("extract_out_conversion", Res);
      end case;
      Set_Named_Entity (Conv, Res);
      Xrefs.Xref_Name (Conv);
      return Res;
   end Extract_Out_Conversion;


   --  Associate ASSOC with interface INTERFACE
   --  This sets RES.
   procedure Sem_Association
     (Assoc : Iir;
      Inter : Iir;
      Finish : Boolean;
      Match : out Boolean)
   is
      Formal : Iir;
      Formal_Type : Iir;
      Actual: Iir;
      Actual_Types : Iir;
      Out_Conv, In_Conv : Iir;
      Expr : Iir;
      Res_Type : Iir;
      Assoc_Kind : Param_Assoc_Type;
   begin
      Formal := Get_Formal (Assoc);

      --  Handle open association.
      if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
         if Formal /= Null_Iir then
            Assoc_Kind := Sem_Formal (Formal, Inter);
            if Assoc_Kind = None then
               Match := False;
               return;
            end if;
            Set_Whole_Association_Flag (Assoc, Assoc_Kind = Whole);
            if Finish then
               Set_Type (Formal, Null_Iir);
               Sem_Name (Formal, False);
               Expr := Get_Named_Entity (Formal);
               if Get_Kind (Expr) = Iir_Kind_Error then
                  Match := False;
                  return;
               end if;
               --  LRM 4.3.3.2  Associations lists
               --  It is an error if an actual of open is associated with a
               --  formal that is associated individually.
               if Assoc_Kind = Individual then
                  Error_Msg_Sem ("cannot associate individually with open",
                                 Assoc);
               end if;

               Xrefs.Xref_Name (Formal);
               Set_Formal (Assoc, Expr);
            end if;
         else
            Set_Whole_Association_Flag (Assoc, True);
         end if;
         Match := True;
         return;
      end if;

      --  Pre-semantize formal and extract out conversion.
      if Formal /= Null_Iir then
         Assoc_Kind := Sem_Formal (Formal, Inter);
         if Assoc_Kind = None then
            Match := False;
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
      Actual := Get_Actual (Assoc);
      Actual_Types := Get_Type (Actual);
      In_Conv := Null_Iir;
      if Actual_Types = Null_Iir then
         Match := False;
         return;
      end if;
      if Get_Kind (Inter) /= Iir_Kind_Constant_Interface_Declaration then
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
         Actual_Types := Get_Type (Actual);
      end if;

      --  4 cases: F:out_conv, G:in_conv.
      --    A  => B     type of A = type of B
      --  F(A) => B     type of B = type of F
      --    A  => G(B)  type of A = type of G
      --  F(A) => G(B)  type of B = type of F, type of A = type of G
      if Out_Conv = Null_Iir and then In_Conv = Null_Iir then
         Match := Compatibility_Types (Formal_Type, Actual_Types);
      else
         Match := True;
         if In_Conv /= Null_Iir then
            if not Compatibility_Types (Formal_Type, Get_Type (In_Conv)) then
               Match := False;
            end if;
         end if;
         if Out_Conv /= Null_Iir then
            if not Compatibility_Types (Get_Type (Out_Conv), Actual_Types) then
               Match := False;
            end if;
         end if;
      end if;

      if not Match then
         if Finish then
            Error_Msg_Sem
              ("can't associate " & Disp_Node (Actual) & " with "
               & Disp_Node (Inter), Assoc);
            Error_Msg_Sem
              ("(type of " & Disp_Node (Actual) & " is "
               & Disp_Type_Of (Actual) & ")", Assoc);
            Error_Msg_Sem
              ("(type of " & Disp_Node (Inter) & " is "
               & Disp_Type_Of (Inter) & ")", Inter);
         end if;
         return;
      end if;

      if not Finish then
         return;
      end if;

      if Out_Conv = Null_Iir and then In_Conv = Null_Iir then
         Res_Type := Formal_Type;
      else
         if Out_Conv /= Null_Iir then
            Res_Type := Search_Compatible_Type (Get_Type (Out_Conv),
                                                Actual_Types);
         else
            Res_Type := Actual_Types;
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
         Match := False;
         return;
      end if;

      --  Semantize formal.
      if Get_Formal (Assoc) /= Null_Iir then
         Set_Type (Formal, Null_Iir);
         Sem_Name (Formal, False);
         Expr := Get_Named_Entity (Formal);
         if Get_Kind (Expr) = Iir_Kind_Error then
            return;
         end if;
         Xrefs.Xref_Name (Formal);
         Free_Name (Formal);
         Set_Formal (Assoc, Expr);
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
           ("can't use an out conversion for an in interface", Assoc);
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
           ("can't use an in conversion for an out/buffer interface", Assoc);
      end if;

      --  FIXME: LRM refs
      --  This is somewhat wrong.  A missing conversion is not an error but
      --  may result in a type mismatch.
      if Get_Mode (Inter) = Iir_Inout_Mode then
         if In_Conv = Null_Iir and then Out_Conv /= Null_Iir then
            Error_Msg_Sem
              ("out conversion without corresponding in conversion", Assoc);
         elsif In_Conv /= Null_Iir and then Out_Conv = Null_Iir then
            Error_Msg_Sem
              ("in conversion without corresponding out conversion", Assoc);
         end if;
      end if;
      Set_Actual (Assoc, Actual);

      --  Semantize actual.
      Expr := Sem_Expression (Actual, Res_Type);
      if Expr /= Null_Iir then
         Expr := Eval_Expr_Check_If_Static (Expr, Res_Type);
         Set_Actual (Assoc, Expr);
         if In_Conv = Null_Iir and then Out_Conv = Null_Iir then
            if not Check_Implicit_Conversion (Formal_Type, Expr) then
               Error_Msg_Sem ("actual length does not match formal length",
                              Assoc);
            end if;
         end if;
      end if;
   end Sem_Association;

   procedure Sem_Association_Chain
     (Interface_Chain : Iir;
      Assoc_Chain: in out Iir;
      Finish: Boolean;
      Missing : Missing_Type;
      Loc : Iir;
      Match : out Boolean)
   is
      --  Set POS and INTERFACE to *the* matching interface if any of ASSOC.
      procedure Search_Interface (Assoc : Iir;
                                  Inter : out Iir;
                                  Pos : out Integer)
      is
         I_Match : Boolean;
      begin
         Inter := Interface_Chain;
         Pos := 0;
         while Inter /= Null_Iir loop
            -- Formal assoc is not necessarily a simple name, it may
            -- be a conversion function, or even an indexed or
            -- selected name.
            Sem_Association (Assoc, Inter, False, I_Match);
            if I_Match then
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
      Match := True;
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
               Match := False;
               return;
            end if;
            -- Try to match actual of ASSOC with the interface.
            if Inter = Null_Iir then
               if Finish then
                  Error_Msg_Sem
                    ("too many arguments for " & Disp_Node (Loc), Assoc);
               end if;
               Match := False;
               return;
            end if;
            Sem_Association (Assoc, Inter, Finish, Match);
            if not Match then
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
                          (Disp_Node (Inter) & " already associated", Assoc);
                        Match := False;
                        return;
                     end if;
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
                          ("non consecutive individual association for "
                           & Disp_Node (Inter), Assoc);
                        Match := False;
                        return;
                     end if;
                     Last_Individual := Inter;
                     Arg_Matched (Pos) := Individual;
                  else
                     if Finish then
                        Error_Msg_Sem
                          (Disp_Node (Inter) & " already associated", Assoc);
                        Match := False;
                        return;
                     end if;
                  end if;
               end if;
               if Finish then
                  Sem_Association (Assoc, Inter, True, Match);
                  --  MATCH can be false du to errors.
               end if;
            else
               -- Not found.
               if Finish then
                  --  FIXME: display the name of subprg or component/entity.
                  --  FIXME: fetch the interface (for parenthesis_name).
                  Error_Msg_Sem
                    ("no interface for " & Disp_Node (Get_Formal (Assoc))
                     & " in association", Assoc);
               end if;
               Match := False;
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

      --  LRM 8.6 Procedure Call Statement
      --  For each formal parameter of a procedure, a procedure call must
      --  specify exactly one corresponding actual parameter.
      --  This actual parameter is specified either explicitly, by an
      --  association element (other than the actual OPEN) in the association
      --  list, or in the absence of such an association element, by a default
      --  expression (see Section 4.3.3.2).

      --  LRM 7.3.3 Function Calls
      --  For each formal parameter of a function, a function call must
      --  specify exactly one corresponding actual parameter.
      --  This actual parameter is specified either explicitly, by an
      --  association element (other than the actual OPEN) in the association
      --  list, or in the absence of such an association element, by a default
      --  expression (see Section 4.3.3.2).

      --  LRM 1.1.1.2
      --  A port of mode IN may be unconnected or unassociated only if its
      --  declaration includes a default expression.
      --  It is an error if a port of any mode other than IN is unconnected
      --  or unassociated and its type is an unconstrained array type.

      Inter := Interface_Chain;
      Pos := 0;
      while Inter /= Null_Iir loop
         if Arg_Matched (Pos) <= Open
           and then Get_Default_Value (Inter) = Null_Iir
         then
            case Missing is
               when Missing_Parameter
                 | Missing_Generic =>
                  if Finish then
                     Error_Msg_Sem ("no actual for " & Disp_Node (Inter), Loc);
                  end if;
                  Match := False;
                  return;
               when Missing_Port =>
                  case Get_Mode (Inter) is
                     when Iir_In_Mode =>
                        if not Finish then
                           raise Internal_Error;
                        end if;
                        Error_Msg_Sem (Disp_Node (Inter)
                                       & " of mode IN must be connected", Loc);
                        Match := False;
                        return;
                     when Iir_Out_Mode
                       | Iir_Linkage_Mode
                       | Iir_Inout_Mode
                       | Iir_Buffer_Mode =>
                        if not Finish then
                           raise Internal_Error;
                        end if;
                        if not Is_Fully_Constrained_Type (Get_Type (Inter))
                        then
                           Error_Msg_Sem
                             ("unconstrained " & Disp_Node (Inter)
                              & " must be connected", Loc);
                           Match := False;
                           return;
                        end if;
                     when Iir_Unknown_Mode =>
                        raise Internal_Error;
                  end case;
               when Missing_Allowed =>
                  null;
            end case;
         end if;
         Inter := Get_Chain (Inter);
         Pos := Pos + 1;
      end loop;
      return;
   end Sem_Association_Chain;
end Sem_Assocs;
