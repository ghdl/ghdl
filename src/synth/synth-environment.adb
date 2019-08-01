--  Environment definition for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Netlists.Builders; use Netlists.Builders;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates; use Netlists.Gates;
with Errorout; use Errorout;
with Synth.Inference;
with Synth.Errors; use Synth.Errors;
with Vhdl.Nodes;
with Vhdl.Errors; use Vhdl.Errors;

package body Synth.Environment is
   procedure Set_Wire_Mark (Wid : Wire_Id; Mark : Boolean := True) is
   begin
      Wire_Id_Table.Table (Wid).Mark_Flag := Mark;
   end Set_Wire_Mark;

   function Get_Wire_Mark (Wid : Wire_Id) return Boolean is
   begin
      return Wire_Id_Table.Table (Wid).Mark_Flag;
   end Get_Wire_Mark;

   function Alloc_Wire (Kind : Wire_Kind; Obj : Source.Syn_Src)
                       return Wire_Id is
   begin
      Wire_Id_Table.Append ((Kind => Kind,
                             Mark_Flag => False,
                             Decl => Obj,
                             Gate => No_Net,
                             Cur_Assign => No_Seq_Assign,
                             Final_Assign => No_Conc_Assign,
                             Nbr_Final_Assign => 0));
      return Wire_Id_Table.Last;
   end Alloc_Wire;

   procedure Set_Wire_Gate (Wid : Wire_Id; Gate : Net) is
   begin
      --  Cannot override a gate.
      pragma Assert (Wire_Id_Table.Table (Wid).Gate = No_Net);

      Wire_Id_Table.Table (Wid).Gate := Gate;
   end Set_Wire_Gate;

   function Get_Wire_Id (W : Seq_Assign) return Wire_Id is
   begin
      return Assign_Table.Table (W).Id;
   end Get_Wire_Id;

   function Get_Assign_Prev (Asgn : Seq_Assign) return Seq_Assign is
   begin
      return Assign_Table.Table (Asgn).Prev;
   end Get_Assign_Prev;

   function Get_Assign_Chain (Asgn : Seq_Assign) return Seq_Assign is
   begin
      return Assign_Table.Table (Asgn).Chain;
   end Get_Assign_Chain;

   procedure Set_Assign_Chain (Asgn : Seq_Assign; Chain : Seq_Assign) is
   begin
      Assign_Table.Table (Asgn).Chain := Chain;
   end Set_Assign_Chain;

   function Current_Phi return Phi_Id is
   begin
      return Phis_Table.Last;
   end Current_Phi;

   procedure Push_Phi is
   begin
      Phis_Table.Append ((First => No_Seq_Assign,
                          Nbr => 0));
   end Push_Phi;

   --  Get list of assignments for this current block.
   procedure Pop_Phi (Phi : out Phi_Type)
   is
      Cur_Phi : constant Phi_Id := Current_Phi;
      Asgn : Seq_Assign;
   begin
      --  Pop.
      Phi := Phis_Table.Table (Cur_Phi);
      Phis_Table.Decrement_Last;

      --  Point to previous wires.  The current values are the ones before
      --  the block.
      Asgn := Phi.First;
      while Asgn /= No_Seq_Assign loop
         pragma Assert (Assign_Table.Table (Asgn).Phi = Cur_Phi);
         Wire_Id_Table.Table (Get_Wire_Id (Asgn)).Cur_Assign :=
           Get_Assign_Prev (Asgn);
         Asgn := Get_Assign_Chain (Asgn);
      end loop;
   end Pop_Phi;

   function Get_Conc_Offset (Asgn : Conc_Assign) return Uns32 is
   begin
      return Conc_Assign_Table.Table (Asgn).Offset;
   end Get_Conc_Offset;

   function Get_Conc_Value (Asgn : Conc_Assign) return Net is
   begin
      return Conc_Assign_Table.Table (Asgn).Value;
   end Get_Conc_Value;

   function Get_Conc_Chain (Asgn : Conc_Assign) return Conc_Assign is
   begin
      return Conc_Assign_Table.Table (Asgn).Next;
   end Get_Conc_Chain;

   procedure Set_Conc_Chain (Asgn : Conc_Assign; Chain : Conc_Assign) is
   begin
      Conc_Assign_Table.Table (Asgn).Next := Chain;
   end Set_Conc_Chain;

   procedure Add_Conc_Assign_Partial
     (Wid : Wire_Id; Val : Net; Off : Uns32; Stmt : Source.Syn_Src)
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      Conc_Assign_Table.Append ((Next => Wire_Rec.Final_Assign,
                                 Value => Val,
                                 Offset => Off,
                                 Stmt => Stmt));
      Wire_Rec.Final_Assign := Conc_Assign_Table.Last;
      Wire_Rec.Nbr_Final_Assign := Wire_Rec.Nbr_Final_Assign + 1;
   end Add_Conc_Assign_Partial;

   function Is_Partial_Assignment (Val : Net; Prev_Val : Net) return Boolean
   is
      Inst : Instance;
      V : Net;
   begin
      if Val = Prev_Val then
         --  This particular case is a loop.
         return False;
      end if;

      V := Val;
      loop
         Inst := Get_Parent (V);
         if Get_Id (Inst) = Id_Insert then
            V := Get_Input_Net (Inst, 0);
         else
            return V = Prev_Val;
         end if;
      end loop;
   end Is_Partial_Assignment;

   procedure Add_Conc_Assign_Comb
     (Wid : Wire_Id; Val : Net; Stmt : Source.Syn_Src)
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      --  Check for partial assignment.
      if Is_Partial_Assignment (Val, Wire_Rec.Gate) then
         declare
            Wd : constant Width := Get_Width (Val);
            Idx : Uns32;
            Len : Width;
            Inst : Instance;
            V : Net;
            Ins_Idx : Uns32;
            Ins_Inp : Net;
            Ins_Wd : Width;
         begin
            --  Sweep all the bits.
            Idx := 0;
            while Idx < Wd loop
               --  We are interested in bits from Idx to the end.
               Len := Wd - Idx;
               V := Val;
               loop
                  Inst := Get_Parent (V);
                  if Get_Id (Inst) = Id_Insert then
                     Ins_Idx := Get_Param_Uns32 (Inst, 0);
                     Ins_Inp := Get_Input_Net (Inst, 1);
                     Ins_Wd := Get_Width (Ins_Inp);
                     if Idx < Ins_Idx then
                        --  Consider bits before this insert; continue.
                        Len := Ins_Idx - Idx;
                     elsif Idx >= Ins_Idx + Ins_Wd then
                        --  Already handled; continue.
                        null;
                     else
                        --  Partially handled.
                        Len := Ins_Idx + Ins_Wd - Idx;
                        if Len = Ins_Wd and then Idx = Ins_Idx then
                           --  Fully convered by this insert.
                           Add_Conc_Assign_Partial (Wid, Ins_Inp, Idx, Stmt);
                        else
                           --  TODO: extract bits from ins_inp.
                           raise Internal_Error;
                        end if;
                        Idx := Idx + Len;
                        exit;
                     end if;
                     --  Check with next insert gate.
                     V := Get_Input_Net (Inst, 0);
                  else
                     --  Not assigned.
                     pragma Assert (V = Wire_Rec.Gate);
                     Idx := Idx + Len;
                     exit;
                  end if;
               end loop;
            end loop;
         end;
      else
         Add_Conc_Assign_Partial (Wid, Val, 0, Stmt);
      end if;
   end Add_Conc_Assign_Comb;

   procedure Add_Conc_Assign
     (Wid : Wire_Id; Val : Net; Stmt : Source.Syn_Src) is
   begin
      Add_Conc_Assign_Partial (Wid, Val, 0, Stmt);
   end Add_Conc_Assign;

   --  This procedure is called after each concurrent statement to assign
   --  values to signals.
   procedure Pop_And_Merge_Phi (Ctxt : Builders.Context_Acc;
                                Stmt : Source.Syn_Src)
   is
      Phi : Phi_Type;
      Asgn : Seq_Assign;
   begin
      Pop_Phi (Phi);

      Asgn := Phi.First;
      while Asgn /= No_Seq_Assign loop
         declare
            Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
            Wid : constant Wire_Id := Asgn_Rec.Id;
            Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
            Outport : constant Net := Wire_Rec.Gate;
            --  Must be connected to an Id_Output or Id_Signal
            pragma Assert (Outport /= No_Net);
            Gate_Inst : Instance;
            Gate_In : Input;
            Drv : Net;
         begin
            Gate_Inst := Get_Parent (Outport);
            Gate_In := Get_Input (Gate_Inst, 0);
            Drv := Get_Driver (Gate_In);

            case Wire_Rec.Kind is
               when Wire_Output
                 | Wire_Signal
                 | Wire_Variable =>
                  if Drv /= No_Net then
                     --  Output already assigned
                     raise Internal_Error;
                  end if;

                  Inference.Infere (Ctxt, Wid, Asgn_Rec.Value, Outport, Stmt);
               when others =>
                  raise Internal_Error;
            end case;

            Asgn := Asgn_Rec.Chain;
         end;
      end loop;
      --  FIXME: free wires.
   end Pop_And_Merge_Phi;

   --  Merge sort of conc_assign by offset.
   procedure Sort_Conc_Assign (Chain : Conc_Assign;
                               Len : Natural;
                               First : out Conc_Assign;
                               Next : out Conc_Assign)
   is
      Left, Right : Conc_Assign;
      Last : Conc_Assign;
      El : Conc_Assign;
   begin
      if Len = 0 then
         First := No_Conc_Assign;
         Next := Chain;
      elsif Len = 1 then
         First := Chain;
         Next := Get_Conc_Chain (Chain);
         Set_Conc_Chain (Chain, No_Conc_Assign);
      else
         --  Divide.
         Sort_Conc_Assign (Chain, Len / 2, Left, Right);
         Sort_Conc_Assign (Right, Len - Len / 2, Right, Next);

         First := No_Conc_Assign;
         Last := No_Conc_Assign;
         for I in 1 .. Len loop
            if Left /= No_Conc_Assign
              and then
              (Right = No_Conc_Assign
                 or else Get_Conc_Offset (Left) <= Get_Conc_Offset (Right))
            then
               El := Left;
               Left := Get_Conc_Chain (Left);
            else
               pragma Assert (Right /= No_Conc_Assign);
               El := Right;
               Right := Get_Conc_Chain (Right);
            end if;
            --  Append
            if First = No_Conc_Assign then
               First := El;
            else
               Set_Conc_Chain (Last, El);
            end if;
            Last := El;
         end loop;
         Set_Conc_Chain (Last, No_Conc_Assign);
      end if;
   end Sort_Conc_Assign;

   procedure Finalize_Complex_Assignment (Ctxt : Builders.Context_Acc;
                                          Wire_Rec : Wire_Id_Record;
                                          Value : out Net)
   is
      First_Assign : Conc_Assign;
      Asgn : Conc_Assign;
      Last_Asgn : Conc_Assign;
      New_Asgn : Conc_Assign;
      Next_Off : Uns32;
      Expected_Off : Uns32;
      Last_Off : Uns32;
      Nbr_Assign : Natural;
   begin
      Nbr_Assign := Wire_Rec.Nbr_Final_Assign;
      --  Sort assignments by offset.
      Asgn := Wire_Rec.Final_Assign;
      Sort_Conc_Assign (Asgn, Nbr_Assign, Asgn, Last_Asgn);
      First_Assign := Asgn;

      --  Report overlaps and holes, count number of inputs
      Last_Asgn := No_Conc_Assign;
      Expected_Off := 0;
      Last_Off := Get_Width (Wire_Rec.Gate);
      while (Expected_Off < Last_Off) or Asgn /= No_Conc_Assign loop
         if Asgn /= No_Conc_Assign then
            Next_Off := Get_Conc_Offset (Asgn);
         else
            Next_Off := Last_Off;
         end if;
         if Next_Off = Expected_Off then
            --  Normal case.
            pragma Assert (Asgn /= No_Conc_Assign);
            Expected_Off := Expected_Off + Get_Width (Get_Conc_Value (Asgn));
            Last_Asgn := Asgn;
            Asgn := Get_Conc_Chain (Asgn);
         elsif Next_Off > Expected_Off then
            if Next_Off = Expected_Off + 1 then
               Warning_Msg_Synth
                 (+Wire_Rec.Decl, "no assignment for offset %v",
                  (1 => +Expected_Off));
            else
               Warning_Msg_Synth
                 (+Wire_Rec.Decl, "no assignment for offsets %v:%v",
                  (+Expected_Off, +(Next_Off - 1)));
            end if;

            --  Insert conc_assign with initial value.
            --  FIXME: handle initial values.
            Conc_Assign_Table.Append
              ((Next => Asgn,
                Value => Build_Const_Z (Ctxt, Next_Off - Expected_Off),
                Offset => Expected_Off,
                Stmt => Source.No_Syn_Src));
            New_Asgn := Conc_Assign_Table.Last;
            if Last_Asgn = No_Conc_Assign then
               First_Assign := New_Asgn;
            else
               Set_Conc_Chain (Last_Asgn, New_Asgn);
            end if;
            Last_Asgn := New_Asgn;
            Nbr_Assign := Nbr_Assign + 1;

            Expected_Off := Next_Off;
         else
            pragma Assert (Next_Off < Expected_Off);
            Error_Msg_Synth
              (+Wire_Rec.Decl, "multiple assignments for offsets %v:%v",
               (+Next_Off, +(Expected_Off - 1)));
            --  TODO: insert resolver
            pragma Assert (Asgn /= No_Conc_Assign);
            Last_Asgn := Asgn;
            Asgn := Get_Conc_Chain (Asgn);
         end if;
      end loop;

      --  Create concat
      --  Set concat inputs
      if Nbr_Assign = 1 then
         Value := Get_Conc_Value (First_Assign);
      elsif Nbr_Assign = 2 then
         Value := Build_Concat2 (Ctxt,
                                 Get_Conc_Value (Last_Asgn),
                                 Get_Conc_Value (First_Assign));
      else
         Value := Build_Concatn (Ctxt, Last_Off, Uns32 (Nbr_Assign));
         declare
            Inst : constant Instance := Get_Parent (Value);
         begin
            Asgn := First_Assign;
            for I in reverse 0 .. Nbr_Assign - 1 loop
               Connect (Get_Input (Inst, Port_Idx (I)), Get_Conc_Value (Asgn));
               Asgn := Get_Conc_Chain (Asgn);
            end loop;
         end;
      end if;
   end Finalize_Complex_Assignment;

   procedure Finalize_Assignment
     (Ctxt : Builders.Context_Acc; Wire_Rec : Wire_Id_Record)
   is
      use Vhdl.Nodes;
      Gate_Inst : constant Instance := Get_Parent (Wire_Rec.Gate);
      Inp : constant Input := Get_Input (Gate_Inst, 0);
      Value : Net;
   begin
      case Wire_Rec.Nbr_Final_Assign is
         when 0 =>
            --  TODO: use initial value ?
            if Wire_Rec.Decl /= Null_Node
              and then Wire_Rec.Kind = Wire_Output
            then
               Error_Msg_Synth
                 (+Wire_Rec.Decl, "no assignment for %n", +Wire_Rec.Decl);
            end if;
            return;
         when 1 =>
            declare
               Conc_Asgn : Conc_Assign_Record renames
                 Conc_Assign_Table.Table (Wire_Rec.Final_Assign);
            begin
               if Conc_Asgn.Offset = 0
                 and then (Get_Width (Conc_Asgn.Value)
                             = Get_Width (Wire_Rec.Gate))
               then
                  --  Single and full assignment.
                  Value := Conc_Asgn.Value;
               else
                  --  Partial or multiple assignments.
                  Finalize_Complex_Assignment (Ctxt, Wire_Rec, Value);
               end if;
            end;
         when others =>
            Finalize_Complex_Assignment (Ctxt, Wire_Rec, Value);
      end case;

      Connect (Inp, Value);
   end Finalize_Assignment;

   procedure Finalize_Assignments (Ctxt : Builders.Context_Acc) is
   begin
      pragma Assert (Phis_Table.Last = No_Phi_Id);
      --  pragma Assert (Assign_Table.Last = No_Seq_Assign);

      for Wid in Wire_Id_Table.First + 1 .. Wire_Id_Table.Last loop
         declare
            Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
         begin
            pragma Assert (Wire_Rec.Cur_Assign = No_Seq_Assign);
            Finalize_Assignment (Ctxt, Wire_Rec);
         end;
      end loop;

      Wire_Id_Table.Set_Last (No_Wire_Id);
   end Finalize_Assignments;

   --  Sort the LEN first wires of chain W (linked by Chain) in Id increasing
   --  values.  The result is assigned to FIRST and the first non-sorted wire
   --  (the one after LEN) is assigned to NEXT.  The chain headed by FIRST
   --  is truncated to LEN elements.
   --  Use a merge sort.
   procedure Sort_Wires (Asgn : Seq_Assign;
                         Len : Uns32;
                         First : out Seq_Assign;
                         Next : out Seq_Assign)
   is
      Left, Right : Seq_Assign;
      Last : Seq_Assign;
      El : Seq_Assign;
   begin
      if Len = 0 then
         --  Empty chain.
         First := No_Seq_Assign;
         Next := Asgn;
         return;
      elsif Len = 1 then
         --  Chain with one element.
         First := Asgn;
         Next := Get_Assign_Chain (First);
         Set_Assign_Chain (First, No_Seq_Assign);
         return;
      else
         --  Divide.
         Sort_Wires (Asgn, Len / 2, Left, Right);
         Sort_Wires (Right, Len - Len / 2, Right, Next);

         --  Conquer: merge.
         First := No_Seq_Assign;
         Last := No_Seq_Assign;
         for I in 1 .. Len loop
            if Left /= No_Seq_Assign
              and then (Right = No_Seq_Assign
                          or else Get_Wire_Id (Left) <= Get_Wire_Id (Right))
            then
               El := Left;
               Left := Get_Assign_Chain (Left);
            else
               pragma Assert (Right /= No_Seq_Assign);
               El := Right;
               Right := Get_Assign_Chain (Right);
            end if;

            --  Append
            if First = No_Seq_Assign then
               First := El;
            else
               Set_Assign_Chain (Last, El);
            end if;
            Last := El;
         end loop;
         Set_Assign_Chain (Last, No_Seq_Assign);
      end if;
   end Sort_Wires;

   function Sort_Phi (P : Phi_Type) return Seq_Assign
   is
      Res, Last : Seq_Assign;
   begin
      Sort_Wires (P.First, P.Nbr, Res, Last);
      pragma Assert (Last = No_Seq_Assign);
      return Res;
   end Sort_Phi;

   function Get_Assign_Value (Asgn : Seq_Assign) return Net
   is
      Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
   begin
      case Wire_Id_Table.Table (Asgn_Rec.Id).Kind is
         when Wire_Signal | Wire_Output | Wire_Inout | Wire_Variable =>
            return Asgn_Rec.Value;
         when Wire_Input | Wire_None =>
            raise Internal_Error;
      end case;
   end Get_Assign_Value;

   function Get_Current_Value (Wid : Wire_Id) return Net
   is
      Wid_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      case Wid_Rec.Kind is
         when Wire_Variable =>
            if Wid_Rec.Cur_Assign = No_Seq_Assign then
               return Wid_Rec.Gate;
            else
               return Assign_Table.Table (Wid_Rec.Cur_Assign).Value;
            end if;
         when Wire_Signal | Wire_Output | Wire_Inout | Wire_Input =>
            return Wid_Rec.Gate;
         when Wire_None =>
            raise Internal_Error;
      end case;
   end Get_Current_Value;

   function Get_Last_Assigned_Value (Wid : Wire_Id) return Net
   is
      Wid_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      if Wid_Rec.Cur_Assign = No_Seq_Assign then
         return Wid_Rec.Gate;
      else
         return Get_Assign_Value (Wid_Rec.Cur_Assign);
      end if;
   end Get_Last_Assigned_Value;

   --  Add muxes for two lists T and F of assignments.
   procedure Merge_Phis (Ctxt : Builders.Context_Acc;
                         Sel : Net;
                         T, F : Phi_Type)
   is
      T_Asgns : Seq_Assign;
      F_Asgns : Seq_Assign;
      W : Wire_Id;
      Te, Fe : Net;
      Res : Net;
   begin
      T_Asgns := Sort_Phi (T);
      F_Asgns := Sort_Phi (F);

      while T_Asgns /= No_Seq_Assign or F_Asgns /= No_Seq_Assign loop
         --  Extract a wire.
         if T_Asgns = No_Seq_Assign
           or else (F_Asgns /= No_Seq_Assign
                      and then Get_Wire_Id (F_Asgns) < Get_Wire_Id (T_Asgns))
         then
            --  Has an assignment only for the false branch.
            W := Get_Wire_Id (F_Asgns);
            Te := Get_Last_Assigned_Value (W);
            Fe := Get_Assign_Value (F_Asgns);
            F_Asgns := Get_Assign_Chain (F_Asgns);
         elsif F_Asgns = No_Seq_Assign
           or else (T_Asgns /= No_Seq_Assign
                      and then Get_Wire_Id (T_Asgns) < Get_Wire_Id (F_Asgns))
         then
            --  Has an assignment only for the true branch.
            W := Get_Wire_Id (T_Asgns);
            Te := Get_Assign_Value (T_Asgns);
            Fe := Get_Last_Assigned_Value (W);
            T_Asgns := Get_Assign_Chain (T_Asgns);
         else
            --  Has assignments for both the true and the false branch.
            pragma Assert (Get_Wire_Id (F_Asgns) = Get_Wire_Id (T_Asgns));
            W := Get_Wire_Id (F_Asgns);
            Te := Get_Assign_Value (T_Asgns);
            Fe := Get_Assign_Value (F_Asgns);
            T_Asgns := Get_Assign_Chain (T_Asgns);
            F_Asgns := Get_Assign_Chain (F_Asgns);
         end if;
         Res := Netlists.Builders.Build_Mux2 (Ctxt, Sel, Fe, Te);
         Phi_Assign (W, Res);
      end loop;
   end Merge_Phis;

   procedure Phi_Insert_Assign (Asgn : Seq_Assign)
   is
      pragma Assert (Asgn /= No_Seq_Assign);
      Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
      pragma Assert (Asgn_Rec.Phi = Current_Phi);
      pragma Assert (Asgn_Rec.Chain = No_Seq_Assign);
      P : Phi_Type renames Phis_Table.Table (Phis_Table.Last);
   begin
      --  Chain assignment in the current sequence.
      Asgn_Rec.Chain := P.First;
      P.First := Asgn;
      P.Nbr := P.Nbr + 1;
   end Phi_Insert_Assign;

   procedure Phi_Assign (Dest : Wire_Id; Val : Net)
   is
      Cur_Asgn : constant Seq_Assign := Wire_Id_Table.Table (Dest).Cur_Assign;
   begin
      if Cur_Asgn = No_Seq_Assign
        or else Assign_Table.Table (Cur_Asgn).Phi < Current_Phi
      then
         --  Never assigned, or first assignment in that level
         Assign_Table.Append ((Phi => Current_Phi,
                              Id => Dest,
                              Prev => Cur_Asgn,
                              Chain => No_Seq_Assign,
                              Value => Val));
         Wire_Id_Table.Table (Dest).Cur_Assign := Assign_Table.Last;
         Phi_Insert_Assign (Assign_Table.Last);
      else
         --  Overwrite.
         --  FIXME: may need to merge in case of partial assignment.
         Assign_Table.Table (Cur_Asgn).Value := Val;
      end if;
   end Phi_Assign;
begin
   Wire_Id_Table.Append ((Kind => Wire_None,
                          Mark_Flag => False,
                          Decl => Source.No_Syn_Src,
                          Gate => No_Net,
                          Cur_Assign => No_Seq_Assign,
                          Final_Assign => No_Conc_Assign,
                          Nbr_Final_Assign => 0));
   pragma Assert (Wire_Id_Table.Last = No_Wire_Id);

   Assign_Table.Append ((Phi => No_Phi_Id,
                        Id => No_Wire_Id,
                        Prev => No_Seq_Assign,
                        Chain => No_Seq_Assign,
                        Value => No_Net));
   pragma Assert (Assign_Table.Last = No_Seq_Assign);

   Phis_Table.Append ((First => No_Seq_Assign,
                       Nbr => 0));
   pragma Assert (Phis_Table.Last = No_Phi_Id);

   Conc_Assign_Table.Append ((Next => No_Conc_Assign,
                              Value => No_Net,
                              Offset => 0,
                              Stmt => Source.No_Syn_Src));
   pragma Assert (Conc_Assign_Table.Last = No_Conc_Assign);
end Synth.Environment;
