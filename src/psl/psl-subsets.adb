--  PSL - Simple subset
--  Copyright (C) 2002-2016 Tristan Gingold
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

with Types; use Types;

with PSL.Types; use PSL.Types;
with PSL.Errors; use PSL.Errors;

package body PSL.Subsets is
   procedure Check_Simple (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Not_Bool =>
            if Get_Psl_Type (Get_Boolean (N)) /= Type_Boolean then
               Error_Msg_Sem
                 ("operand of a negation operator must be a boolean", N);
            end if;
         when N_Never =>
            case Get_Psl_Type (Get_Property (N)) is
               when Type_Sequence | Type_Boolean =>
                  null;
               when others =>
                  Error_Msg_Sem ("operand of a 'never' operator must be "
                                   & "a boolean or a sequence", N);
            end case;
         when N_Eventually =>
            case Get_Psl_Type (Get_Property (N)) is
               when Type_Sequence | Type_Boolean =>
                  null;
               when others =>
                  Error_Msg_Sem ("operand of an 'eventually!' operator must be"
                                   & " a boolean or a sequence", N);
            end case;
         when N_And_Bool =>
            if Get_Psl_Type (Get_Left (N)) /= Type_Boolean then
               Error_Msg_Sem ("left-hand side operand of logical 'and' must be"
                                & " a boolean", N);
            end if;
         when N_Or_Bool =>
            if Get_Psl_Type (Get_Left (N)) /= Type_Boolean then
               Error_Msg_Sem ("left-hand side operand of logical 'or' must be"
                                & " a boolean", N);
            end if;
         when N_Log_Imp_Prop =>
            if Get_Psl_Type (Get_Left (N)) /= Type_Boolean then
               Error_Msg_Sem ("left-hand side operand of logical '->' must be"
                                & " a boolean", N);
            end if;
            --  FIXME: <->
         when N_Until =>
            if not Get_Inclusive_Flag (N) then
               if Get_Psl_Type (Get_Right (N)) /= Type_Boolean then
                  Error_Msg_Sem ("right-hand side of a non-overlapping "
                                   & "'until*' operator must be a boolean", N);
               end if;
            else
               if Get_Psl_Type (Get_Right (N)) /= Type_Boolean
                 or else Get_Psl_Type (Get_Left (N)) /= Type_Boolean
               then
                  Error_Msg_Sem ("both operands of an overlapping 'until*'"
                                   & " operator are boolean", N);
               end if;
            end if;
         when N_Before =>
            if Get_Psl_Type (Get_Right (N)) /= Type_Boolean
              or else Get_Psl_Type (Get_Left (N)) /= Type_Boolean
            then
               Error_Msg_Sem ("both operands of a 'before*'"
                                & " operator are boolean", N);
            end if;
         when others =>
            null;
      end case;

      --  Recursion.
      case Get_Kind (N) is
         when N_Error =>
            null;
         when N_Hdl_Mod_Name =>
            null;
         when N_Vunit
            | N_Vmode
            | N_Vprop =>
            declare
               Item : Node;
            begin
               Item := Get_Item_Chain (N);
               while Item /= Null_Node loop
                  Check_Simple (Item);
                  Item := Get_Chain (Item);
               end loop;
            end;
         when N_Name_Decl =>
            null;
         when N_Assert_Directive
            | N_Property_Declaration =>
            Check_Simple (Get_Property (N));
         when N_Endpoint_Declaration
            | N_Sequence_Declaration =>
            Check_Simple (Get_Sequence (N));
         when N_Clock_Event =>
            Check_Simple (Get_Property (N));
            Check_Simple (Get_Boolean (N));
         when N_Always
            | N_Never
            | N_Eventually
            | N_Strong =>
            Check_Simple (Get_Property (N));
         when N_Braced_SERE
            | N_Clocked_SERE =>
            Check_Simple (Get_SERE (N));
         when N_Concat_SERE
            | N_Fusion_SERE
            | N_Within_SERE =>
            Check_Simple (Get_Left (N));
            Check_Simple (Get_Right (N));
         when N_Name =>
            null;
         when N_Star_Repeat_Seq
            | N_Plus_Repeat_Seq =>
            declare
               N2 : constant Node := Get_Sequence (N);
            begin
               if N2 /= Null_Node then
                  Check_Simple (N2);
               end if;
            end;
         when N_Goto_Repeat_Seq
            | N_Equal_Repeat_Seq =>
            null;
         when N_Match_And_Seq
            | N_And_Seq
            | N_Or_Seq =>
            Check_Simple (Get_Left (N));
            Check_Simple (Get_Right (N));
         when N_Imp_Seq
            | N_Overlap_Imp_Seq =>
            Check_Simple (Get_Sequence (N));
            Check_Simple (Get_Property (N));
         when N_Log_Imp_Prop
            | N_Log_Equiv_Prop
            | N_Until
            | N_Before
            | N_Or_Prop
            | N_And_Prop
            | N_And_Bool
            | N_Or_Bool
            | N_Imp_Bool
            | N_Equiv_Bool =>
            Check_Simple (Get_Left (N));
            Check_Simple (Get_Right (N));
         when N_Next
            | N_Next_A
            | N_Next_E
            | N_Paren_Prop =>
            Check_Simple (Get_Property (N));
         when N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Abort
           | N_Async_Abort
           | N_Sync_Abort =>
            Check_Simple (Get_Boolean (N));
            Check_Simple (Get_Property (N));
         when N_Not_Bool
           | N_Paren_Bool =>
            Check_Simple (Get_Boolean (N));
         when N_Const_Parameter
           | N_Sequence_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter =>
            null;
         when N_Actual =>
            null;
         when N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Property_Instance =>
            null;
         when N_True
           | N_False
           | N_Number
           | N_Inf
           | N_EOS
           | N_HDL_Expr
           | N_HDL_Bool =>
            null;
      end case;
   end Check_Simple;

   function Is_Async_Abort (N : Node) return Boolean is
   begin
      case Get_Kind (N) is
         when N_Async_Abort
            | N_Abort =>
            return True;
         when N_Sync_Abort =>
            return False;
         when others =>
            raise Internal_Error;
      end case;
   end Is_Async_Abort;

end PSL.Subsets;
