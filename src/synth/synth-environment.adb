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

   function Get_Assign_Partial (Asgn : Seq_Assign) return Partial_Assign is
   begin
      return Assign_Table.Table (Asgn).Asgns;
   end Get_Assign_Partial;

   function Get_Partial_Offset (Asgn : Partial_Assign) return Uns32 is
   begin
      return Partial_Assign_Table.Table (Asgn).Offset;
   end Get_Partial_Offset;

   function Get_Partial_Value (Asgn : Partial_Assign) return Net is
   begin
      return Partial_Assign_Table.Table (Asgn).Value;
   end Get_Partial_Value;

   function Get_Partial_Next (Asgn : Partial_Assign) return Partial_Assign is
   begin
      return Partial_Assign_Table.Table (Asgn).Next;
   end Get_Partial_Next;

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

   procedure Add_Conc_Assign
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
            P : Partial_Assign;
         begin
            case Wire_Rec.Kind is
               when Wire_Output
                 | Wire_Signal
                 | Wire_Variable =>
                  --  Check output is not already assigned.
                  pragma Assert
                    (Get_Input_Net (Get_Parent (Outport), 0) = No_Net);

               when others =>
                  raise Internal_Error;
            end case;

            P := Asgn_Rec.Asgns;
            pragma Assert (P /= No_Partial_Assign);
            while P /= No_Partial_Assign loop
               declare
                  Pa : Partial_Assign_Record renames
                    Partial_Assign_Table.Table (P);
               begin
                  Inference.Infere
                    (Ctxt, Wid, Pa.Value, Pa.Offset, Outport, Stmt);
                  P := Pa.Next;
               end;
            end loop;

            Asgn := Asgn_Rec.Chain;
         end;
      end loop;
      --  FIXME: free wires.
   end Pop_And_Merge_Phi;

   --  Merge sort of conc_assign by offset.
   function Le_Conc_Assign (Left, Right : Conc_Assign) return Boolean is
   begin
      if Get_Conc_Offset (Left) < Get_Conc_Offset (Right) then
         return True;
      end if;
      if Get_Conc_Offset (Left) = Get_Conc_Offset (Right) then
         return (Get_Width (Get_Conc_Value (Left))
                   < Get_Width (Get_Conc_Value (Right)));
      else
         return False;
      end if;
   end Le_Conc_Assign;

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
            pragma Assert (not (Left = No_Conc_Assign
                                  and Right = No_Conc_Assign));
            if Right = No_Conc_Assign
              or else
              (Left /= No_Conc_Assign and then Le_Conc_Assign (Left, Right))
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
            --  There is an hole.
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
            Expected_Off := Expected_Off + Get_Width (Get_Conc_Value (Asgn));
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

   function Get_Assign_Value (Ctxt : Builders.Context_Acc; Asgn : Seq_Assign)
                             return Net
   is
      Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
      Wid_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Asgn_Rec.Id);
      W : constant Width := Get_Width (Wid_Rec.Gate);
   begin
      case Wid_Rec.Kind is
         when Wire_Signal | Wire_Output | Wire_Inout | Wire_Variable =>
            null;
         when Wire_Input | Wire_None =>
            raise Internal_Error;
      end case;

      --  Cannot be empty.
      pragma Assert (Asgn_Rec.Asgns /= No_Partial_Assign);

      --  Simple case: fully assigned.
      declare
         Pasgn : Partial_Assign_Record renames
           Partial_Assign_Table.Table (Asgn_Rec.Asgns);
      begin
         if Pasgn.Offset = 0 and then Get_Width (Pasgn.Value) = W then
            return Pasgn.Value;
         end if;
      end;

      return Get_Current_Assign_Value (Ctxt, Asgn_Rec.Id, 0, W);
   end Get_Assign_Value;

   function Get_Current_Value (Ctxt : Builders.Context_Acc; Wid : Wire_Id)
                              return Net
   is
      Wid_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      case Wid_Rec.Kind is
         when Wire_Variable =>
            if Wid_Rec.Cur_Assign = No_Seq_Assign then
               --  The variable was never assigned, so the variable value is
               --  the initial value.
               --  FIXME: use initial value directly ?
               return Wid_Rec.Gate;
            else
               return Get_Assign_Value (Ctxt, Wid_Rec.Cur_Assign);
            end if;
         when Wire_Signal | Wire_Output | Wire_Inout | Wire_Input =>
            --  For signals, always read the previous value.
            return Wid_Rec.Gate;
         when Wire_None =>
            raise Internal_Error;
      end case;
   end Get_Current_Value;

   function Get_Last_Assigned_Value
     (Ctxt : Builders.Context_Acc; Wid : Wire_Id) return Net
   is
      Wid_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      if Wid_Rec.Cur_Assign = No_Seq_Assign then
         return Wid_Rec.Gate;
      else
         return Get_Assign_Value (Ctxt, Wid_Rec.Cur_Assign);
      end if;
   end Get_Last_Assigned_Value;

   --  Get the current value of W for WD bits at offset OFF.
   function Get_Current_Assign_Value
     (Ctxt : Builders.Context_Acc; Wid : Wire_Id; Off : Uns32; Wd : Width)
     return Net
   is
      Wire : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
      First_Seq : Seq_Assign;
   begin
      --  Latest seq assign
      First_Seq := Wire.Cur_Assign;

      --  If no seq assign, return current value.
      if First_Seq = No_Seq_Assign then
         if Off = 0 and then Wd = Get_Width (Wire.Gate) then
            return Wire.Gate;
         else
            return Build_Extract (Ctxt, Wire.Gate, Off, Wd);
         end if;
      end if;

      --  If the range is the same as the seq assign, return the value.
      declare
         P : constant Partial_Assign := Get_Assign_Partial (First_Seq);
         V : Net;
      begin
         if Get_Partial_Offset (P) = Off then
            V := Get_Partial_Value (P);
            if Get_Width (V) = Wd then
               return V;
            end if;
         end if;
      end;

      --  Build a vector
      declare
         Vec : Net_Tables.Instance;
         Seq : Seq_Assign;
         P : Partial_Assign;
         Cur_Off : Uns32;
         Cur_Wd : Width;

         Last : Int32;
         Inst : Instance;
         Res : Net;
      begin
         Net_Tables.Init (Vec);
         Cur_Off := Off;
         Cur_Wd := Wd;
         pragma Assert (Wd > 0);
         loop
            --  Find value at CUR_OFF from assignment.
            Seq := First_Seq;
            P := Get_Assign_Partial (Seq);
            loop
               pragma Assert (P /= No_Partial_Assign);
               declare
                  Pr : Partial_Assign_Record renames
                    Partial_Assign_Table.Table (P);
                  Pw : constant Width := Get_Width (Pr.Value);
               begin
                  if Pr.Offset <= Cur_Off
                    and then Pr.Offset + Pw > Cur_Off
                  then
                     --  Found.
                     if Pr.Offset = Cur_Off and then Pw = Cur_Wd then
                        --  No need to extract.
                        Net_Tables.Append (Vec, Pr.Value);
                     else
                        Cur_Wd := Width'Min
                          (Cur_Wd, Pw - (Cur_Off - Pr.Offset));
                        Net_Tables.Append
                          (Vec, Build_Extract (Ctxt, Pr.Value,
                                               Cur_Off - Pr.Offset, Cur_Wd));
                     end if;
                     exit;
                  end if;
                  if Pr.Offset + Pw < Cur_Off then
                     --  Next partial;
                     P := Pr.Next;
                  elsif Pr.Offset > Cur_Off
                    and then Pr.Offset < Cur_Off + Cur_Wd
                  then
                     --  Reduce WD and continue to search in previous;
                     Cur_Wd := Pr.Offset - Cur_Off;
                     P := No_Partial_Assign;
                  else
                     --  Continue to search in previous.
                     P := No_Partial_Assign;
                  end if;
                  if P = No_Partial_Assign then
                     Seq := Get_Assign_Prev (Seq);
                     if Seq = No_Seq_Assign then
                        --  Extract from gate.
                        Net_Tables.Append
                          (Vec, Build_Extract (Ctxt, Wire.Gate,
                                               Cur_Off, Cur_Wd));
                        exit;
                     end if;
                  end if;
               end;
            end loop;

            Cur_Off := Cur_Off + Cur_Wd;
            Cur_Wd := Wd - (Cur_Off - Off);
            exit when Cur_Off = Off + Wd;
         end loop;

         --  Concat
         Last := Net_Tables.Last (Vec);
         case Last is
            when Int32'First .. 0 =>
               raise Internal_Error;
            when 1 =>
               Res := Vec.Table (1);
            when 2 =>
               Res := Build_Concat2 (Ctxt, Vec.Table (1), Vec.Table (2));
            when 3 =>
               Res := Build_Concat3
                 (Ctxt, Vec.Table (1), Vec.Table (2), Vec.Table (3));
            when 4 =>
               Res := Build_Concat4
                 (Ctxt,
                  Vec.Table (1), Vec.Table (2), Vec.Table (3), Vec.Table (4));
            when 5 .. Int32'Last =>
               Res := Build_Concatn (Ctxt, Wd, Uns32 (Last));
               Inst := Get_Parent (Res);
               for I in Net_Tables.First .. Last loop
                  Connect (Get_Input (Inst, Port_Idx (I - 1)), Vec.Table (I));
               end loop;
         end case;
         --  Free the vector and return it.
         Net_Tables.Free (Vec);
         return Res;
      end;
   end Get_Current_Assign_Value;

   procedure Merge_Assigns (Ctxt : Builders.Context_Acc;
                            W : Wire_Id;
                            Sel : Net;
                            F_Asgns : Partial_Assign;
                            T_Asgns : Partial_Assign)
   is
      P : Partial_Assign_Array (0 .. 1);
      N : Net_Array (0 .. 1);
      Min_Off : Uns32;
      Off : Uns32;
      Wd : Width;
      Res : Net;
   begin
      P := (0 => F_Asgns, 1 => T_Asgns);

      Min_Off := 0;
      loop
         --  Look for the partial assign with the least offset (but still
         --  greather than Min_Off).  Also extract the least width.
         Off := Uns32'Last;
         Wd := Width'Last;
         for I in P'Range loop
            if P (I) /= No_Partial_Assign then
               declare
                  Pa : Partial_Assign_Record
                    renames Partial_Assign_Table.Table (P (I));
               begin
                  if Pa.Offset <= Off then
                     Off := Uns32'Max (Pa.Offset, Min_Off);
                     Wd := Width'Min
                       (Wd, Get_Width (Pa.Value) - (Off - Pa.Offset));
                  end if;
               end;
            end if;
         end loop;

         --  No more assignments.
         if Off = Uns32'Last and Wd = Width'Last then
            return;
         end if;

         --  Get the values for that offset/width.  Update lists.
         for I in P'Range loop
            if P (I) /= No_Partial_Assign
              and then Get_Partial_Offset (P (I)) <= Off
            then
               declare
                  Val : constant Net := Get_Partial_Value (P (I));
                  P_W : constant Width := Get_Width (Val);
                  P_Off : constant Uns32 := Get_Partial_Offset (P (I));
               begin
                  --  There is a partial assignment.
                  if P_Off = Off and then P_W = Wd then
                     --  Full covered.
                     N (I) := Val;
                     P (I) := Get_Partial_Next (P (I));
                  else
                     N (I) := Build_Extract (Ctxt, Val, Off - P_Off, Wd);
                     if P_Off + P_W = Off + Wd then
                        P (I) := Get_Partial_Next (P (I));
                     end if;
                  end if;
               end;
            else
               --  No partial assignment.  Get extract previous value.
               N (I) := Get_Current_Assign_Value (Ctxt, W, Off, Wd);
            end if;
         end loop;

         --  Build mux.
         Res := Netlists.Builders.Build_Mux2 (Ctxt, Sel, N (0), N (1));
         Phi_Assign (Ctxt, W, Res, Off);

         Min_Off := Off + Wd;
      end loop;
   end Merge_Assigns;

   --  Add muxes for two lists T and F of assignments.
   procedure Merge_Phis (Ctxt : Builders.Context_Acc;
                         Sel : Net;
                         T, F : Phi_Type)
   is
      T_Asgns : Seq_Assign;
      F_Asgns : Seq_Assign;
      W : Wire_Id;
      Tp, Fp : Partial_Assign;
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
            Fp := Get_Assign_Partial (F_Asgns);
            Tp := No_Partial_Assign;
            F_Asgns := Get_Assign_Chain (F_Asgns);
         elsif F_Asgns = No_Seq_Assign
           or else (T_Asgns /= No_Seq_Assign
                      and then Get_Wire_Id (T_Asgns) < Get_Wire_Id (F_Asgns))
         then
            --  Has an assignment only for the true branch.
            W := Get_Wire_Id (T_Asgns);
            Fp := No_Partial_Assign;
            Tp := Get_Assign_Partial (T_Asgns);
            T_Asgns := Get_Assign_Chain (T_Asgns);
         else
            --  Has assignments for both the true and the false branch.
            pragma Assert (Get_Wire_Id (F_Asgns) = Get_Wire_Id (T_Asgns));
            W := Get_Wire_Id (F_Asgns);
            Fp := Get_Assign_Partial (F_Asgns);
            Tp := Get_Assign_Partial (T_Asgns);
            T_Asgns := Get_Assign_Chain (T_Asgns);
            F_Asgns := Get_Assign_Chain (F_Asgns);
         end if;
         Merge_Assigns (Ctxt, W, Sel, Fp, Tp);

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

   --  Check consistency:
   --  - ordered.
   --  - no overlaps.
   procedure Check (Seq : Seq_Assign)
   is
      Seq_Asgn : Seq_Assign_Record renames Assign_Table.Table (Seq);
      Prev_El : Partial_Assign;
   begin
      Prev_El := Seq_Asgn.Asgns;
      if Prev_El = No_Partial_Assign then
         --  It's empty!
         return;
      end if;
      loop
         declare
            Prev : Partial_Assign_Record
              renames Partial_Assign_Table.Table (Prev_El);
            El : constant Partial_Assign := Prev.Next;
         begin
            if El = No_Partial_Assign then
               --  Done.
               exit;
            end if;
            declare
               Cur : Partial_Assign_Record
                 renames Partial_Assign_Table.Table (El);
            begin
               --  Check no overlap.
               if Cur.Offset < Prev.Offset + Get_Width (Prev.Value) then
                  raise Internal_Error;
               end if;
            end;
            Prev_El := El;
         end;
      end loop;
   end Check;

   --  Insert partial assignment ASGN to list SEQ.
   --  Deal with overrides.  Place it correctly.
   procedure Insert_Partial_Assign
     (Ctxt : Builders.Context_Acc; Seq : Seq_Assign; Asgn : Partial_Assign)
   is
      V : Partial_Assign_Record renames Partial_Assign_Table.Table (Asgn);
      V_Next : constant Uns32 := V.Offset + Get_Width (V.Value);
      Seq_Asgn : Seq_Assign_Record renames Assign_Table.Table (Seq);
      El, Last_El : Partial_Assign;
      Inserted : Boolean;
   begin
      Inserted := False;
      Last_El := No_Partial_Assign;
      El := Seq_Asgn.Asgns;
      while El /= No_Partial_Assign loop
         declare
            P : Partial_Assign_Record renames Partial_Assign_Table.Table (El);
            P_Next : constant Uns32 := P.Offset + Get_Width (P.Value);
         begin
            if V.Offset < P_Next and then V_Next > P.Offset then
               --  Override.
               if V.Offset <= P.Offset and then V_Next >= P_Next then
                  --  Full override:
                  --     V.Off               V.Next
                  --     |------------------||
                  --           |----------||
                  --          P.Off        P.Next
                  --  Remove it.
                  --  FIXME: free it.
                  if not Inserted then
                     if Last_El /= No_Partial_Assign then
                        Partial_Assign_Table.Table (Last_El).Next := Asgn;
                     else
                        Seq_Asgn.Asgns := Asgn;
                     end if;
                     V.Next := P.Next;
                     Inserted := True;
                     Last_El := Asgn;
                  else
                     pragma Assert (Last_El /= No_Partial_Assign);
                     Partial_Assign_Table.Table (Last_El).Next := P.Next;
                  end if;
               elsif V.Offset <= P.Offset and then V_Next < P_Next then
                  --  Overrides the beginning of EL.
                  --     V.Off           V.Next
                  --     |--------------||
                  --           |----------||
                  --          P.Off        P.Next
                  --  Shrink EL.
                  P.Value := Build_Extract (Ctxt, P.Value,
                                            Off => V_Next - P.Offset,
                                            W => P_Next - V_Next);
                  P.Offset := V_Next;
                  if not Inserted then
                     if Last_El /= No_Partial_Assign then
                        Partial_Assign_Table.Table (Last_El).Next := Asgn;
                     else
                        Seq_Asgn.Asgns := Asgn;
                     end if;
                     V.Next := El;
                     Inserted := True;
                  end if;
                  --  No more possible overlaps.
                  exit;
               elsif V.Offset > P.Offset and then P_Next <= V_Next then
                  --  Overrides the end of EL.
                  --             V.Off               V.Next
                  --             |------------------||
                  --           |----------||
                  --          P.Off        P.Next
                  --  Shrink EL.
                  P.Value := Build_Extract (Ctxt, P.Value,
                                            Off => 0,
                                            W => V.Offset - P.Offset);
                  pragma Assert (not Inserted);
                  V.Next := P.Next;
                  P.Next := Asgn;
                  Last_El := Asgn;
                  Inserted := True;
               elsif V.Offset > P.Offset and then V_Next < P_Next then
                  --  Contained within EL.
                  --             V.Off       V.Next
                  --             |----------||
                  --           |---------------||
                  --          P.Off             P.Next
                  --  Split EL.
                  pragma Assert (not Inserted);
                  Partial_Assign_Table.Append
                    ((Next => P.Next,
                      Value => Build_Extract (Ctxt, P.Value,
                                              Off => V_Next - P.Offset,
                                              W => P_Next - V_Next),
                      Offset => V_Next));
                  V.Next := Partial_Assign_Table.Last;
                  P.Value := Build_Extract (Ctxt, P.Value,
                                            Off => 0,
                                            W => V.Offset - P.Offset);
                  P.Next := Asgn;
                  Inserted := True;
                  --  No more possible overlaps.
                  exit;
               else
                  --  No other case.
                  raise Internal_Error;
               end if;
            else
               if V.Offset < P.Offset then
                  --  Insert before P (if not already inserted).
                  if not Inserted then
                     if Last_El /= No_Partial_Assign then
                        Partial_Assign_Table.Table (Last_El).Next := Asgn;
                     else
                        Seq_Asgn.Asgns := Asgn;
                     end if;
                     V.Next := El;
                     Inserted := True;
                  end if;
                  exit;
               elsif P.Next = No_Partial_Assign then
                  if not Inserted then
                     --  Insert after P.
                     P.Next := Asgn;
                     Inserted := True;
                  end if;
                  exit;
               else
                  Last_El := El;
               end if;
            end if;

            El := P.Next;
         end;
      end loop;
      pragma Assert (Inserted);
      pragma Debug (Check (Seq));
   end Insert_Partial_Assign;

   procedure Phi_Assign
     (Ctxt : Builders.Context_Acc; Dest : Wire_Id; Val : Net; Offset : Uns32)
   is
      Cur_Asgn : constant Seq_Assign := Wire_Id_Table.Table (Dest).Cur_Assign;
      Pasgn : Partial_Assign;
   begin
      Partial_Assign_Table.Append ((Next => No_Partial_Assign,
                                    Value => Val,
                                    Offset => Offset));
      Pasgn := Partial_Assign_Table.Last;

      if Cur_Asgn = No_Seq_Assign
        or else Assign_Table.Table (Cur_Asgn).Phi < Current_Phi
      then
         --  Never assigned, or first assignment in that level
         Assign_Table.Append ((Phi => Current_Phi,
                               Id => Dest,
                               Prev => Cur_Asgn,
                               Chain => No_Seq_Assign,
                               Asgns => Pasgn));
         Wire_Id_Table.Table (Dest).Cur_Assign := Assign_Table.Last;
         Phi_Insert_Assign (Assign_Table.Last);
      else
         --  Overwrite.
         Insert_Partial_Assign (Ctxt, Cur_Asgn, Pasgn);
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
                        Asgns => No_Partial_Assign));
   pragma Assert (Assign_Table.Last = No_Seq_Assign);

   Partial_Assign_Table.Append ((Next => No_Partial_Assign,
                                 Value => No_Net,
                                 Offset => 0));
   pragma Assert (Partial_Assign_Table.Last = No_Partial_Assign);

   Phis_Table.Append ((First => No_Seq_Assign,
                       Nbr => 0));
   pragma Assert (Phis_Table.Last = No_Phi_Id);

   Conc_Assign_Table.Append ((Next => No_Conc_Assign,
                              Value => No_Net,
                              Offset => 0,
                              Stmt => Source.No_Syn_Src));
   pragma Assert (Conc_Assign_Table.Last = No_Conc_Assign);
end Synth.Environment;
