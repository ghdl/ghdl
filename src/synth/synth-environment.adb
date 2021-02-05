--  Environment definition for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Netlists.Builders; use Netlists.Builders;
with Netlists.Concats;
with Netlists.Gates;
with Netlists.Gates_Ports;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Inference;

with Errorout; use Errorout;
with Name_Table;

with Synth.Flags;
with Synth.Errors; use Synth.Errors;
with Synth.Source; use Synth.Source;
with Synth.Context;

with Vhdl.Nodes;
with Vhdl.Utils;

package body Synth.Environment is
   procedure Phi_Assign
     (Ctxt : Builders.Context_Acc; Dest : Wire_Id; Pasgn : Partial_Assign);

   procedure Set_Wire_Mark (Wid : Wire_Id; Mark : Boolean := True) is
   begin
      Wire_Id_Table.Table (Wid).Mark_Flag := Mark;
   end Set_Wire_Mark;

   function Get_Wire_Mark (Wid : Wire_Id) return Boolean is
   begin
      return Wire_Id_Table.Table (Wid).Mark_Flag;
   end Get_Wire_Mark;

   function Alloc_Wire (Kind : Wire_Kind; Typ : Type_Acc; Obj : Source.Syn_Src)
                       return Wire_Id
   is
      Res : Wire_Id;
   begin
      Wire_Id_Table.Append ((Kind => Kind,
                             Mark_Flag => False,
                             Decl => Obj,
                             Typ => Typ,
                             Gate => No_Net,
                             Cur_Assign => No_Seq_Assign,
                             Final_Assign => No_Conc_Assign,
                             Nbr_Final_Assign => 0));
      Res := Wire_Id_Table.Last;
      return Res;
   end Alloc_Wire;

   procedure Free_Wire (Wid : Wire_Id)
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      --  Check the wire was not already free.
      pragma Assert (Wire_Rec.Kind /= Wire_None);

      --  All the assignments have been handled.
      pragma Assert (Wire_Rec.Cur_Assign = No_Seq_Assign);

      Wire_Rec.Kind := Wire_None;
   end Free_Wire;

   procedure Set_Wire_Gate (Wid : Wire_Id; Gate : Net) is
   begin
      --  Cannot override a gate.
      pragma Assert (Wire_Id_Table.Table (Wid).Gate = No_Net);

      Wire_Id_Table.Table (Wid).Gate := Gate;
   end Set_Wire_Gate;

   function Get_Wire_Gate (Wid : Wire_Id) return Net is
   begin
      return Wire_Id_Table.Table (Wid).Gate;
   end Get_Wire_Gate;

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

   function Get_Assign_Is_Static (Asgn : Seq_Assign) return Boolean is
   begin
      return Assign_Table.Table (Asgn).Val.Is_Static = True;
   end Get_Assign_Is_Static;

   function Get_Assign_Static_Val (Asgn : Seq_Assign) return Memtyp is
   begin
      return Assign_Table.Table (Asgn).Val.Val;
   end Get_Assign_Static_Val;

   function Get_Assign_Partial (Asgn : Seq_Assign) return Partial_Assign is
   begin
      --  Note: fails if the value is static.
      --  Use Get_Assign_Partial_Force if you want to automatically convert
      --  the value to a Partial_Assign (a net).
      return Assign_Table.Table (Asgn).Val.Asgns;
   end Get_Assign_Partial;

   function Get_Seq_Assign_Value (Asgn : Seq_Assign) return Seq_Assign_Value is
   begin
      return Assign_Table.Table (Asgn).Val;
   end Get_Seq_Assign_Value;

   function New_Partial_Assign (Val : Net; Offset : Uns32)
                               return Partial_Assign is
   begin
      Partial_Assign_Table.Append ((Next => No_Partial_Assign,
                                    Value => Val,
                                    Offset => Offset));
      return Partial_Assign_Table.Last;
   end New_Partial_Assign;

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

   procedure Set_Partial_Next (Asgn : Partial_Assign;
                               Chain : Partial_Assign) is
   begin
      Partial_Assign_Table.Table (Asgn).Next := Chain;
   end Set_Partial_Next;

   function Current_Phi return Phi_Id is
   begin
      return Phis_Table.Last;
   end Current_Phi;

   procedure Push_Phi is
   begin
      Phis_Table.Append ((First => No_Seq_Assign,
                          Last => No_Seq_Assign,
                          Nbr => 0,
                          En => No_Wire_Id));
   end Push_Phi;

   procedure Mark (M : out Wire_Id) is
   begin
      M := Wire_Id_Table.Last;
   end Mark;

   procedure Release (M : in out Wire_Id)
   is
      Last : Wire_Id;
   begin
      --  Check all wires to be released are free.
      Last := M;
      for I in M + 1 .. Wire_Id_Table.Last loop
         declare
            Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (I);
            Asgn : Seq_Assign;
         begin
            case Wire_Rec.Kind is
               when Wire_None =>
                  null;
               when Wire_Enable =>
                  --  Keep.  This renames the wire, but the only references
                  --  must be in the wire.
                  Last := Last + 1;
                  if Last /= I then
                     --  Renames.
                     Asgn := Wire_Rec.Cur_Assign;
                     while Asgn /= No_Seq_Assign loop
                        Assign_Table.Table (Asgn).Id := Last;
                        Asgn := Get_Assign_Prev (Asgn);
                     end loop;
                     Wire_Id_Table.Table (Last) := Wire_Rec;
                  end if;
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end loop;

      --  Release.
      Wire_Id_Table.Set_Last (Last);

      M := No_Wire_Id;
   end Release;

   procedure All_Released is
   begin
      if Wire_Id_Table.Last /= No_Wire_Id then
         raise Internal_Error;
      end if;
   end All_Released;

   --  Concatenate when possible partial assignments of HEAD.
   procedure Merge_Partial_Assignments
     (Ctxt : Context_Acc; Head : Seq_Assign_Value)
   is
      use Netlists.Concats;
      First : Partial_Assign;
      Next : Partial_Assign;
      Concat : Concat_Type;
      Expected_Next_Off : Uns32;
      Next_Off : Uns32;
      Next_Val : Net;
   begin
      if Head.Is_Static /= False then
         return;
      end if;

      First := Head.Asgns;
      loop
         exit when First = No_Partial_Assign;

         Next := Get_Partial_Next (First);
         exit when Next = No_Partial_Assign;
         Expected_Next_Off := Get_Partial_Offset (First)
           + Get_Width (Get_Partial_Value (First));
         Next_Off := Get_Partial_Offset (Next);
         if Expected_Next_Off = Next_Off then
            --  Merge First and Next.
            Next_Val := Get_Partial_Value (Next);
            Append (Concat, Get_Partial_Value (First));
            Append (Concat, Next_Val);
            Expected_Next_Off := Next_Off + Get_Width (Next_Val);
            --  Merge as long as possible.
            loop
               Next := Get_Partial_Next (Next);
               exit when Next = No_Partial_Assign;

               Next_Off := Get_Partial_Offset (Next);
               Next_Val := Get_Partial_Value (Next);
               exit when  Next_Off /= Expected_Next_Off;
               Append (Concat, Next_Val);
               Expected_Next_Off := Next_Off + Get_Width (Next_Val);
            end loop;

            --  Replace.
            declare
               First_Record : Partial_Assign_Record renames
                 Partial_Assign_Table.Table (First);
            begin
               Build (Ctxt, Concat, First_Record.Value);
               First_Record.Next := Next;

            end;
         end if;
         First := Next;
      end loop;
   end Merge_Partial_Assignments;

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

   procedure Phi_Discard_Wires (Wid1 : Wire_Id; Wid2 : Wire_Id)
   is
      Phi : Phi_Type renames Phis_Table.Table (Current_Phi);
      Asgn, Next_Asgn : Seq_Assign;
      Wid : Wire_Id;
   begin
      Asgn := Phi.First;
      Phi := (First => No_Seq_Assign,
              Last => No_Seq_Assign,
              Nbr => 0,
              En => No_Wire_Id);
      while Asgn /= No_Seq_Assign loop
         pragma Assert (Assign_Table.Table (Asgn).Phi = Current_Phi);
         Next_Asgn := Get_Assign_Chain (Asgn);
         Set_Assign_Chain (Asgn, No_Seq_Assign);

         Wid := Get_Wire_Id (Asgn);
         if Wid = Wid1 or Wid = Wid2 then
            --  Discard.
            pragma Assert (Wid /= No_Wire_Id);
            Wire_Id_Table.Table (Wid).Cur_Assign := No_Seq_Assign;
         else
            --  Append.
            if Phi.First = No_Seq_Assign then
               Phi.First := Asgn;
            else
               Set_Assign_Chain (Phi.Last, Asgn);
            end if;
            Phi.Nbr := Phi.Nbr + 1;
            Phi.Last := Asgn;
         end if;
         Asgn := Next_Asgn;
      end loop;
   end Phi_Discard_Wires;

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
      pragma Assert (Wire_Rec.Kind /= Wire_None);
      Conc_Assign_Table.Append ((Next => Wire_Rec.Final_Assign,
                                 Value => Val,
                                 Offset => Off,
                                 Stmt => Stmt));
      Wire_Rec.Final_Assign := Conc_Assign_Table.Last;
      Wire_Rec.Nbr_Final_Assign := Wire_Rec.Nbr_Final_Assign + 1;
   end Add_Conc_Assign;

   procedure Pop_And_Merge_Phi_Wire (Ctxt : Builders.Context_Acc;
                                     Asgn_Rec : Seq_Assign_Record;
                                     Stmt : Source.Syn_Src)
   is
      Wid : constant Wire_Id := Asgn_Rec.Id;
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
      Outport : constant Net := Wire_Rec.Gate;
      --  Must be connected to an Id_Output or Id_Signal
      pragma Assert (Outport /= No_Net);
      P : Partial_Assign;
      Res : Net;
   begin
      --  Check output is not already assigned.
      pragma Assert (Get_Input_Net (Get_Net_Parent (Outport), 0) = No_Net);

      case Asgn_Rec.Val.Is_Static is
         when Unknown =>
            raise Internal_Error;
         when True =>
            --  Create a net.  No inference to do.
            Res := Synth.Context.Get_Memtyp_Net (Ctxt, Asgn_Rec.Val.Val);
            if Wire_Rec.Kind = Wire_Enable then
               Connect (Get_Input (Get_Net_Parent (Outport), 0), Res);
            else
               Add_Conc_Assign (Wid, Res, 0, Stmt);
            end if;
         when False =>
            P := Asgn_Rec.Val.Asgns;
            pragma Assert (P /= No_Partial_Assign);
            while P /= No_Partial_Assign loop
               declare
                  Pa : Partial_Assign_Record renames
                    Partial_Assign_Table.Table (P);
               begin
                  if Synth.Flags.Flag_Debug_Noinference then
                     Res := Pa.Value;
                  elsif Wire_Rec.Kind = Wire_Enable then
                     --  Possibly infere a idff/iadff.
                     pragma Assert (Pa.Offset = 0);
                     pragma Assert (Pa.Next = No_Partial_Assign);
                     Res := Inference.Infere_Assert
                       (Ctxt, Pa.Value, Outport, Stmt);
                     Connect (Get_Input (Get_Net_Parent (Outport), 0), Res);
                  else
                     --  Note: lifetime is currently based on the kind of the
                     --   wire (variable -> not reused beyond this process).
                     --   This is OK for vhdl but not general.
                     Res := Inference.Infere
                       (Ctxt, Pa.Value, Pa.Offset, Outport, Stmt,
                        Wire_Rec.Kind = Wire_Variable);
                     Add_Conc_Assign (Wid, Res, Pa.Offset, Stmt);
                  end if;
                  P := Pa.Next;
               end;
            end loop;
      end case;
   end Pop_And_Merge_Phi_Wire;

   --  This procedure is called after each concurrent statement to assign
   --  values to signals.
   procedure Pop_And_Merge_Phi (Ctxt : Builders.Context_Acc;
                                Stmt : Source.Syn_Src)
   is
      Phi : Phi_Type;
      Asgn : Seq_Assign;
   begin
      Pop_Phi (Phi);
      pragma Assert (Phis_Table.Last = No_Phi_Id);

      --  It is possible that the same value is assigned to different targets.
      --  Example:
      --    if rising_edge(clk) then
      --      a := c;
      --    end if;
      --    b := a;
      --  Because the assignment is not yet done, only the net is stored in
      --  the partial assign.  When the net for variable A is infered and
      --  changed to a dff, it is not known that it will also be assigned to
      --  variable B.
      --
      --  Mark gates that will be infered.  And if already marked, insert
      --  a nop.
      Asgn := Phi.First;
      while Asgn /= No_Seq_Assign loop
         declare
            Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
            P : Partial_Assign;
         begin
            if Asgn_Rec.Val.Is_Static = False then
               P := Asgn_Rec.Val.Asgns;
               pragma Assert (P /= No_Partial_Assign);
               while P /= No_Partial_Assign loop
                  declare
                     Pa : Partial_Assign_Record
                       renames Partial_Assign_Table.Table (P);
                     Res_Inst : constant Instance := Get_Net_Parent (Pa.Value);
                  begin
                     if Get_Mark_Flag (Res_Inst)
                       and then Get_Id (Res_Inst) = Gates.Id_Mux2
                     then
                        --  A nop is needed iff the value is reused and will be
                        --  inferred (which is only possible for Id_Mux2).
                        Pa.Value := Build_Nop (Ctxt, Pa.Value);
                     else
                        Set_Mark_Flag (Res_Inst, True);
                     end if;

                     P := Pa.Next;
                  end;
               end loop;
            end if;
            Asgn := Asgn_Rec.Chain;
         end;
      end loop;

      --  Clear mark flag.
      Asgn := Phi.First;
      while Asgn /= No_Seq_Assign loop
         declare
            Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
            P : Partial_Assign;
         begin
            if Asgn_Rec.Val.Is_Static = False then
               P := Asgn_Rec.Val.Asgns;
               pragma Assert (P /= No_Partial_Assign);
               while P /= No_Partial_Assign loop
                  declare
                     Pa : Partial_Assign_Record
                       renames Partial_Assign_Table.Table (P);
                     Res_Inst : constant Instance := Get_Net_Parent (Pa.Value);
                  begin
                     Set_Mark_Flag (Res_Inst, False);

                     P := Pa.Next;
                  end;
               end loop;
            end if;
            Asgn := Asgn_Rec.Chain;
         end;
      end loop;

      Asgn := Phi.First;
      while Asgn /= No_Seq_Assign loop
         declare
            Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
         begin
            Pop_And_Merge_Phi_Wire (Ctxt, Asgn_Rec, Stmt);
            Asgn := Asgn_Rec.Chain;
         end;
      end loop;
   end Pop_And_Merge_Phi;

   procedure Propagate_Phi_Until_Mark (Ctxt : Builders.Context_Acc;
                                       Phi : Phi_Type;
                                       Mark : Wire_Id)
   is
      Asgn, Next_Asgn : Seq_Assign;
   begin
      Asgn := Phi.First;
      while Asgn /= No_Seq_Assign loop
         declare
            Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
            Wid : constant Wire_Id := Asgn_Rec.Id;
            Pasgn, Next_Pasgn : Partial_Assign;
         begin
            --  FIXME: Asgn_Rec may become invalid due to allocation by
            --  Phi_Assign.  So we read what is needed before calling
            --  Phi_Assign.
            Next_Asgn := Asgn_Rec.Chain;
            if Wid <= Mark then
               case Asgn_Rec.Val.Is_Static is
                  when Unknown =>
                     raise Internal_Error;
                  when True =>
                     Phi_Assign_Static (Wid, Asgn_Rec.Val.Val);
                  when False =>
                     Pasgn := Asgn_Rec.Val.Asgns;
                     while Pasgn /= No_Partial_Assign loop
                        Next_Pasgn := Get_Partial_Next (Pasgn);
                        Set_Partial_Next (Pasgn, No_Partial_Assign);
                        Phi_Assign (Ctxt, Wid, Pasgn);
                        Pasgn := Next_Pasgn;
                     end loop;
               end case;
            end if;
            Asgn := Next_Asgn;
         end;
      end loop;
   end Propagate_Phi_Until_Mark;

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

   --  Return True iff PREV and NEXT are two concurrent assignments for
   --  a multiport memory.
   function Is_Finalize_Assignment_Multiport (Prev, Next : Conc_Assign)
                                             return Boolean
   is
      use Netlists.Gates;
      P_Val : Net;
      N_Val : Net;
   begin
      --  The assignemnts must fully overlap (same offset and same width).
      if Get_Conc_Offset (Prev) /= Get_Conc_Offset (Next) then
         return False;
      end if;
      P_Val := Get_Conc_Value (Prev);
      N_Val := Get_Conc_Value (Next);
      if Get_Width (P_Val) /= Get_Width (N_Val) then
         return False;
      end if;

      --  Both assignments must be a dff.
      case Get_Id (Get_Net_Parent (P_Val)) is
         when Id_Dyn_Insert_En =>
            null;
         when others =>
            return False;
      end case;
      case Get_Id (Get_Net_Parent (N_Val)) is
         when Id_Dyn_Insert_En =>
            null;
         when others =>
            return False;
      end case;

      return True;
   end Is_Finalize_Assignment_Multiport;

   function Is_Tribuf_Net (N : Net) return Boolean
   is
      use Netlists.Gates;
   begin
      case Get_Id (Get_Net_Parent (N)) is
         when Id_Tri
           | Id_Resolver
           | Id_Port =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Tribuf_Net;

   function Is_Tribuf_Assignment (Prev, Next : Conc_Assign) return Boolean
   is
      P_Val : Net;
      N_Val : Net;
   begin
      --  The assignemnts must fully overlap (same offset and same width).
      if Get_Conc_Offset (Prev) /= Get_Conc_Offset (Next) then
         return False;
      end if;
      P_Val := Get_Conc_Value (Prev);
      N_Val := Get_Conc_Value (Next);
      if Get_Width (P_Val) /= Get_Width (N_Val) then
         return False;
      end if;

      --  Both assignments must be a tri or a resolver.
      return Is_Tribuf_Net (P_Val)
        and then Is_Tribuf_Net (N_Val);
   end Is_Tribuf_Assignment;

   function Info_Subrange_Vhdl (Off : Width; Wd : Width; Bnd: Bound_Type)
                               return String
   is
      function Image (V : Int32) return String
      is
         Res : constant String := Int32'Image (V);
      begin
         if V >= 0 then
            return Res (2 .. Res'Last);
         else
            return Res;
         end if;
      end Image;
   begin
      case Bnd.Dir is
         when Dir_To =>
            if Wd = 1 then
               return Image (Bnd.Right - Int32 (Off));
            else
               return Image (Bnd.Left + Int32 (Bnd.Len - (Off + Wd)))
                 & " to "
                 & Image (Bnd.Right - Int32 (Off));
            end if;
         when Dir_Downto =>
            if Wd = 1 then
               return Image (Bnd.Right + Int32 (Off));
            else
               return Image (Bnd.Left - Int32 (Bnd.Len - (Off + Wd)))
                 & " downto "
                 & Image (Bnd.Right + Int32 (Off));
            end if;
      end case;
   end Info_Subrange_Vhdl;

   procedure Info_Subnet_Vhdl (Loc    : Location_Type;
                               Prefix : String;
                               Otype  : Vhdl.Nodes.Node;
                               Typ    : Type_Acc;
                               Off    : Width;
                               Wd     : Width) is
   begin
      case Typ.Kind is
         when Type_Bit
            | Type_Logic
            | Type_Discrete
            | Type_Float =>
            pragma Assert (Wd = Typ.W);
            pragma Assert (Off = 0);
            Info_Msg_Synth (+Loc, "  " & Prefix);
         when Type_File
            | Type_Protected
            | Type_Access
            | Type_Unbounded_Array
            | Type_Unbounded_Record
            | Type_Unbounded_Vector =>
            raise Internal_Error;
         when Type_Vector =>
            pragma Assert (Wd <= Typ.W);
            if Off = 0 and Wd = Typ.W then
               Info_Msg_Synth (+Loc, "  " & Prefix);
            else
               Info_Msg_Synth
                 (+Loc,
                  "  " & Prefix
                    & "(" & Info_Subrange_Vhdl (Off, Wd, Typ.Vbound) & ")");
            end if;
         when Type_Slice
            | Type_Array =>
            Info_Msg_Synth (+Loc, "  " & Prefix & "(??)");
         when Type_Record =>
            declare
               use Vhdl.Nodes;
               Els : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Otype);
            begin
               for I in Typ.Rec.E'Range loop
                  declare
                     El : Rec_El_Type renames Typ.Rec.E (I);
                     Field : constant Vhdl.Nodes.Node :=
                       Get_Nth_Element (Els, Natural (I - 1));
                     Sub_Off : Uns32;
                     Sub_Wd : Width;
                  begin
                     if Off + Wd <= El.Boff then
                        --  Not covered anymore.
                        exit;
                     elsif Off >= El.Boff + El.Typ.W then
                        --  Not yet covered.
                        null;
                     elsif Off <= El.Boff
                       and then Off + Wd >= El.Boff + El.Typ.W
                     then
                        --  Fully covered.
                        Info_Msg_Synth
                          (+Loc,
                           "  " & Prefix & '.'
                             & Vhdl.Utils.Image_Identifier (Field));
                     else
                        --  Partially covered.
                        if Off < El.Boff then
                           Sub_Off := 0;
                           Sub_Wd := Wd - (El.Boff - Off);
                           Sub_Wd := Width'Min (Sub_Wd, El.Typ.W);
                        else
                           Sub_Off := Off - El.Boff;
                           Sub_Wd := El.Typ.W - (Off - El.Boff);
                           Sub_Wd := Width'Min (Sub_Wd, Wd);
                        end if;
                        Info_Subnet_Vhdl
                          (+Loc,
                           Prefix & '.' & Vhdl.Utils.Image_Identifier (Field),
                           Get_Type (Field), El.Typ, Sub_Off, Sub_Wd);
                     end if;
                  end;
               end loop;
            end;
      end case;
   end Info_Subnet_Vhdl;

   procedure Info_Subnet
     (Decl : Vhdl.Nodes.Node; Typ : Type_Acc; Off : Width; Wd : Width)
   is
      Loc : Location_Type;
   begin
      if Typ = null then
         --  Type is unknown, cannot display more infos.
         return;
      end if;

      if Off = 0 and Wd = Typ.W then
         --  Whole object, no need to give details.
         --  TODO: just say it ?
         return;
      end if;

      Loc := Vhdl.Nodes.Get_Location (Decl);
      Info_Msg_Synth (+Loc, " this concerns these parts of the signal:");
      Info_Subnet_Vhdl (Loc,
                        Name_Table.Image (Vhdl.Nodes.Get_Identifier (Decl)),
                        Vhdl.Nodes.Get_Type (Decl),
                        Typ, Off, Wd);
   end Info_Subnet;

   --  Compute the VALUE to be assigned to WIRE_REC.  Handle partial
   --  assignment, multiple assignments and error cases.
   procedure Finalize_Complex_Assignment (Ctxt : Builders.Context_Acc;
                                          Wire_Rec : Wire_Id_Record;
                                          Value : out Net)
   is
      Wire_Width : constant Width := Get_Width (Wire_Rec.Gate);
      First_Assign : Conc_Assign;
      Asgn : Conc_Assign;
      Last_Asgn : Conc_Assign;
      New_Asgn : Conc_Assign;
      Next_Off : Uns32;
      Expected_Off : Uns32;
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
      while (Expected_Off < Wire_Width) or Asgn /= No_Conc_Assign loop
         --  NEXT_OFF is the offset of the next assignment.
         --  EXPECTED_OFF is the offset just after the previous assignment.
         if Asgn /= No_Conc_Assign then
            Next_Off := Get_Conc_Offset (Asgn);
         else
            --  If there is no more assignment, simulate a hole until the end.
            Next_Off := Wire_Width;
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
                 (+Wire_Rec.Decl, "no assignment for offset %v of %n",
                  (1 => +Expected_Off, 2 => +Wire_Rec.Decl));
            else
               Warning_Msg_Synth
                 (+Wire_Rec.Decl, "no assignment for offsets %v:%v of %n",
                  (+Expected_Off, +(Next_Off - 1), +Wire_Rec.Decl));
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
            --  Overlap.
            pragma Assert (Next_Off < Expected_Off);
            pragma Assert (Asgn /= No_Conc_Assign);

            if Wire_Rec.Kind = Wire_Variable
              and then Is_Finalize_Assignment_Multiport (Last_Asgn, Asgn)
            then
               --  Insert a multiport (for shared variable).
               declare
                  Last_Asgn_Rec : Conc_Assign_Record renames
                    Conc_Assign_Table.Table (Last_Asgn);
               begin
                  Last_Asgn_Rec.Value := Build_Mem_Multiport
                    (Ctxt, Last_Asgn_Rec.Value, Get_Conc_Value (Asgn));
               end;
               --  Remove this assignment.
               Nbr_Assign := Nbr_Assign - 1;
               Set_Conc_Chain (Last_Asgn, Get_Conc_Chain (Asgn));
            elsif Is_Tribuf_Assignment (Last_Asgn, Asgn) then
               --  Insert a resolver.
               declare
                  Last_Asgn_Rec : Conc_Assign_Record renames
                    Conc_Assign_Table.Table (Last_Asgn);
                  V : constant Net := Last_Asgn_Rec.Value;
               begin
                  Last_Asgn_Rec.Value := Build_Resolver
                    (Ctxt, V, Get_Conc_Value (Asgn));
                  Copy_Location (Last_Asgn_Rec.Value, V);
               end;
               --  Remove this assignment.
               Nbr_Assign := Nbr_Assign - 1;
               Set_Conc_Chain (Last_Asgn, Get_Conc_Chain (Asgn));
            else
               declare
                  Asgn_Wd : constant Width :=
                    Get_Width (Get_Conc_Value (Asgn));
                  Overlap_Wd : Width;
               begin
                  Overlap_Wd := Asgn_Wd;
                  if Next_Off + Overlap_Wd > Expected_Off then
                     Overlap_Wd := Expected_Off - Next_Off;
                  end if;

                  Error_Msg_Synth
                    (+Wire_Rec.Decl,
                     "multiple assignments for %i offsets %v:%v",
                     (+Wire_Rec.Decl,
                      +Next_Off, +(Next_Off + Overlap_Wd - 1)));
                  Info_Subnet (Wire_Rec.Decl, Wire_Rec.Typ,
                               Next_Off, Overlap_Wd);

                  if Next_Off + Asgn_Wd < Expected_Off then
                     --  Remove this assignment
                     Nbr_Assign := Nbr_Assign - 1;
                     Set_Conc_Chain (Last_Asgn, Get_Conc_Chain (Asgn));
                  else
                     Expected_Off := Next_Off + Asgn_Wd;
                     Last_Asgn := Asgn;
                  end if;
               end;
            end if;
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
         Value := Build_Concatn (Ctxt, Wire_Width, Uns32 (Nbr_Assign));
         declare
            Inst : constant Instance := Get_Net_Parent (Value);
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
     (Ctxt : Builders.Context_Acc; Wid : Wire_Id)
   is
      use Vhdl.Nodes;
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
      Gate_Inst : constant Instance := Get_Net_Parent (Wire_Rec.Gate);
      Inp : constant Input := Get_Input (Gate_Inst, 0);
      Value : Net;
   begin
      case Wire_Rec.Nbr_Final_Assign is
         when 0 =>
            --  TODO: use initial value ?
            --  TODO: fix that in synth-decls.finalize_object.
            if Wire_Rec.Decl /= Null_Node
              and then Wire_Rec.Kind = Wire_Output
            then
               Warning_Msg_Synth
                 (+Wire_Rec.Decl, "no assignment for %n", +Wire_Rec.Decl);
               if Get_Id (Gate_Inst) = Gates.Id_Iinout then
                  Value := Get_Input_Net (Gate_Inst, 1);
               else
                  Value := Build_Const_Z (Ctxt, Get_Width (Wire_Rec.Gate));
               end if;
            else
               return;
            end if;
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
                  --  Partial assignment.
                  Finalize_Complex_Assignment (Ctxt, Wire_Rec, Value);
               end if;
            end;
            Wire_Rec.Final_Assign := No_Conc_Assign;
         when others =>
            --  Multiple assignments.
            Finalize_Complex_Assignment (Ctxt, Wire_Rec, Value);
            Wire_Rec.Final_Assign := No_Conc_Assign;
      end case;

      Connect (Inp, Value);
   end Finalize_Assignment;

   procedure Finalize_Wires is
   begin
      pragma Assert (Phis_Table.Last = No_Phi_Id);
      --  pragma Assert (Assign_Table.Last = No_Seq_Assign);

      for Wid in Wire_Id_Table.First + 1 .. Wire_Id_Table.Last loop
         declare
            Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
         begin
            pragma Assert (Wire_Rec.Kind = Wire_None
                             or Wire_Rec.Kind = Wire_Enable);
            pragma Assert (Wire_Rec.Final_Assign = No_Conc_Assign);
            null;
         end;
      end loop;

      Wire_Id_Table.Set_Last (No_Wire_Id);
   end Finalize_Wires;

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
         when Wire_Signal | Wire_Output | Wire_Inout
           | Wire_Variable =>
            null;
         when Wire_Input | Wire_Enable | Wire_None =>
            raise Internal_Error;
      end case;

      if Asgn_Rec.Val.Is_Static = True then
         return Synth.Context.Get_Memtyp_Net (Ctxt, Asgn_Rec.Val.Val);
      end if;

      --  Cannot be empty.
      pragma Assert (Asgn_Rec.Val.Asgns /= No_Partial_Assign);

      --  Simple case: fully assigned.
      declare
         Pasgn : Partial_Assign_Record renames
           Partial_Assign_Table.Table (Asgn_Rec.Val.Asgns);
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
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
      pragma Assert (Wire_Rec.Kind /= Wire_None);
   begin
      case Wire_Rec.Kind is
         when Wire_Variable =>
            if Wire_Rec.Cur_Assign = No_Seq_Assign then
               --  The variable was never assigned, so the variable value is
               --  the initial value.
               --  FIXME: use initial value directly ?
               return Wire_Rec.Gate;
            else
               return Get_Assign_Value (Ctxt, Wire_Rec.Cur_Assign);
            end if;
         when Wire_Signal | Wire_Output | Wire_Inout | Wire_Input
           | Wire_Enable =>
            --  For signals, always read the previous value.
            return Wire_Rec.Gate;
         when Wire_None =>
            raise Internal_Error;
      end case;
   end Get_Current_Value;

   --  Get the current value of W for WD bits at offset OFF.
   function Get_Current_Assign_Value
     (Ctxt : Context_Acc; Wid : Wire_Id; Off : Uns32; Wd : Width)
     return Net
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
      pragma Assert (Wire_Rec.Kind /= Wire_None);
      First_Seq : Seq_Assign;
   begin
      --  Latest seq assign
      First_Seq := Wire_Rec.Cur_Assign;

      --  If no seq assign, return current value.
      if First_Seq = No_Seq_Assign then
         return Build2_Extract_Push (Ctxt, Wire_Rec.Gate, Off, Wd);
      end if;

      --  If the current value is static, just return it.
      if Get_Assign_Is_Static (First_Seq) then
         return Context.Get_Partial_Memtyp_Net
           (Ctxt, Get_Assign_Static_Val (First_Seq), Off, Wd);
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
         use Netlists.Concats;
         Vec : Concat_Type;
         Seq : Seq_Assign;
         P : Partial_Assign;
         Cur_Off : Uns32;
         Cur_Wd : Width;

         Res : Net;
      begin
         Cur_Off := Off;
         Cur_Wd := Wd;
         pragma Assert (Wd > 0);
         loop
            --  Find value at CUR_OFF from assignment.  Start at the top
            --  phi (which is not a static value).
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
                     if Pr.Offset = Cur_Off and then Pw <= Cur_Wd then
                        --  No need to extract.
                        Append (Vec, Pr.Value);
                        Cur_Wd := Pw;
                     else
                        Cur_Wd := Width'Min
                          (Cur_Wd, Pw - (Cur_Off - Pr.Offset));
                        Append
                          (Vec,
                           Build2_Extract_Push (Ctxt, Pr.Value,
                                                Cur_Off - Pr.Offset, Cur_Wd));
                     end if;
                     exit;
                  end if;
                  if Pr.Offset + Pw <= Cur_Off then
                     --  Skip this partial, it is before what we are searching.
                     P := Pr.Next;
                  elsif Pr.Offset > Cur_Off
                    and then Pr.Offset < Cur_Off + Cur_Wd
                  then
                     --  There is a partial assignment that should be
                     --  considered, but first we need some values before it.
                     --  Reduce WD and continue to search in previous;
                     Cur_Wd := Pr.Offset - Cur_Off;
                     P := No_Partial_Assign;
                  else
                     --  The next partial assignment is beyond what we are
                     --  searching.
                     --  Continue to search in previous.
                     P := No_Partial_Assign;
                  end if;
                  if P = No_Partial_Assign then
                     Seq := Get_Assign_Prev (Seq);
                     if Seq = No_Seq_Assign then
                        --  Extract from gate.
                        Append (Vec, Build2_Extract_Push (Ctxt, Wire_Rec.Gate,
                                                          Cur_Off, Cur_Wd));
                        exit;
                     end if;
                     if Get_Assign_Is_Static (Seq) then
                        --  Extract from static value.
                        Append (Vec, Context.Get_Partial_Memtyp_Net
                                  (Ctxt, Get_Assign_Static_Val (Seq),
                                   Cur_Off, Cur_Wd));
                        exit;
                     end if;
                     P := Get_Assign_Partial (Seq);
                  end if;
               end;
            end loop;

            Cur_Off := Cur_Off + Cur_Wd;
            Cur_Wd := Wd - (Cur_Off - Off);
            exit when Cur_Off = Off + Wd;
         end loop;

         --  Concat
         Build (Ctxt, Vec, Res);
         return Res;
      end;
   end Get_Current_Assign_Value;

   --  P is an array of Partial_Assign.  Each element is a list
   --  of partial assign from a different basic block.
   --  Extract the value to nets N of the maximal partial assignment starting
   --  at offset OFF for all partial assignments.  Fully handled partial
   --  assignments are poped.  Set the offset and width to OFF and WD of the
   --  result.
   procedure Extract_Merge_Partial_Assigns (Ctxt : Builders.Context_Acc;
                                            P : in out Seq_Assign_Value_Array;
                                            N : out Net_Array;
                                            Off : in out Uns32;
                                            Wd : out Width)
   is
      Min_Off : Uns32;
   begin
      Min_Off := Off;

      --  Look for the partial assign with the least offset (but still
      --  greather than Min_Off).  Also extract the least width.
      Off := Uns32'Last;
      Wd := Width'Last;
      for I in P'Range loop
         case P (I).Is_Static is
            when Unknown =>
               --  No assignment.
               null;
            when True =>
               declare
                  P_Wd : constant Width := P (I).Val.Typ.W;
               begin
                  if Min_Off >= P_Wd then
                     --  No net can be beyond the width.
                     pragma Assert (Off = Uns32'Last);
                     pragma Assert (Wd = Width'Last);
                     return;
                  end if;

                  if Off > Min_Off and then Off < P_Wd then
                     --  There is already an assignment for an offset after
                     --  the minimum.  Stick to the min!
                     Wd := Off - Min_Off;
                     Off := Min_Off;
                  else
                     --  Either no assignment, or an assignment at Min_Off.
                     Off := Min_Off;
                     Wd := Width'Min (Wd, P_Wd - Min_Off);
                  end if;
               end;
            when False =>
               declare
                  pragma Assert (P (I).Asgns /= No_Partial_Assign);
                  Pa : Partial_Assign_Record
                    renames Partial_Assign_Table.Table (P (I).Asgns);
                  N_Wd : Width;
                  N_Off : Uns32;
               begin
                  if Pa.Offset < Off and then Min_Off < Off then
                     --  There is an assignment for an offset before the
                     --  current one.  Handle it.
                     pragma Assert (Off >= Min_Off);
                     N_Off := Uns32'Max (Pa.Offset, Min_Off);
                     N_Wd := Get_Width (Pa.Value) - (N_Off - Pa.Offset);
                     Wd := Width'Min (N_Wd, Off - N_Off);
                     Off := N_Off;
                  elsif Pa.Offset = Off
                    or else (Off = Min_Off and then Pa.Offset < Off)
                  then
                     --  Reduce the width if the assignment is shorter.
                     Wd := Width'Min
                       (Wd, Get_Width (Pa.Value) - (Off - Pa.Offset));
                  elsif Pa.Offset < Off + Wd then
                     --  Reduce the width when there is an assignment after
                     --  the current offset.
                     Wd := Pa.Offset - Off;
                  end if;
               end;
         end case;
      end loop;

      --  No more assignments.
      if Off = Uns32'Last and Wd = Width'Last then
         return;
      end if;

      --  Get the values for that offset/width.  Update lists.
      for I in P'Range loop
         --  Default: no partial assignment.  Get extract previous value.
         N (I) := No_Net;

         case P (I).Is_Static is
            when Unknown =>
               null;
            when True =>
               N (I) := Context.Get_Partial_Memtyp_Net
                 (Ctxt, P (I).Val, Off, Wd);
            when False =>
               if Get_Partial_Offset (P (I).Asgns) <= Off then
                  declare
                     Asgn : constant Partial_Assign := P (I).Asgns;
                     Val : constant Net := Get_Partial_Value (Asgn);
                     P_W : constant Width := Get_Width (Val);
                     P_Off : constant Uns32 := Get_Partial_Offset (Asgn);
                  begin
                     --  There is a partial assignment.
                     if P_Off = Off and then P_W = Wd then
                        --  Full covered.
                        N (I) := Val;
                        P (I).Asgns := Get_Partial_Next (Asgn);
                     else
                        N (I) := Build_Extract (Ctxt, Val, Off - P_Off, Wd);
                        if P_Off + P_W = Off + Wd then
                           P (I).Asgns := Get_Partial_Next (Asgn);
                        end if;
                     end if;
                  end;
                  if P (I).Asgns = No_Partial_Assign then
                     P (I) := No_Seq_Assign_Value;
                  end if;
               end if;
         end case;
      end loop;
   end Extract_Merge_Partial_Assigns;

   function Is_Assign_Value_Array_Static
     (Wid : Wire_Id; Arr : Seq_Assign_Value_Array) return Memtyp
   is
      Res : Memtyp;
      Prev_Val : Memtyp;
   begin
      Prev_Val := Null_Memtyp;
      for I in Arr'Range loop
         case Arr (I).Is_Static is
            when False =>
               --  A value is not static.
               return Null_Memtyp;
            when Unknown =>
               if Prev_Val = Null_Memtyp then
                  --  First use of previous value.
                  if not Is_Static_Wire (Wid) then
                     --  The previous value is not static.
                     return Null_Memtyp;
                  end if;
                  Prev_Val := Get_Static_Wire (Wid);
                  if Res /= Null_Memtyp then
                     --  There is already a result.
                     if not Is_Equal (Res, Prev_Val) then
                        --  The previous value is different from the result.
                        return Null_Memtyp;
                     end if;
                  else
                     Res := Prev_Val;
                  end if;
               end if;
            when True =>
               if Res = Null_Memtyp then
                  --  First value.  Keep it.
                  Res := Arr (I).Val;
               else
                  if not Is_Equal (Res, Arr (I).Val) then
                     --  Value is different.
                     return  Null_Memtyp;
                  end if;
               end if;
         end case;
      end loop;
      return Res;
   end Is_Assign_Value_Array_Static;

   procedure Partial_Assign_Init (List : out Partial_Assign_List) is
   begin
      List := (First | Last => No_Partial_Assign);
   end Partial_Assign_Init;

   procedure Partial_Assign_Append (List : in out Partial_Assign_List;
                                    Pasgn : Partial_Assign) is
   begin
      if List.First = No_Partial_Assign then
         List.First := Pasgn;
      else
         Set_Partial_Next (List.Last, Pasgn);
      end if;
      List.Last := Pasgn;
   end Partial_Assign_Append;

   procedure Merge_Partial_Assigns (Ctxt : Builders.Context_Acc;
                                    Wid : Wire_Id;
                                    List : in out Partial_Assign_List)
   is
      Pasgn : Partial_Assign;
   begin
      while List.First /= No_Partial_Assign loop
         Pasgn := Get_Partial_Next (List.First);
         Set_Partial_Next (List.First, No_Partial_Assign);
         Phi_Assign (Ctxt, Wid, List.First);
         List.First := Pasgn;
      end loop;
   end Merge_Partial_Assigns;

   procedure Merge_Assigns (Ctxt : Builders.Context_Acc;
                            Wid : Wire_Id;
                            Sel : Net;
                            F_Asgns : Seq_Assign_Value;
                            T_Asgns : Seq_Assign_Value;
                            Stmt : Source.Syn_Src)
   is
      use Netlists.Gates;
      use Netlists.Gates_Ports;
      P : Seq_Assign_Value_Array (0 .. 1);
      N : Net_Array (0 .. 1);
      Min_Off : Uns32;
      Off : Uns32;
      Wd : Width;
      Res : Net;
      List : Partial_Assign_List;
      Pasgn : Partial_Assign;
      N1_Inst : Instance;
   begin
      P := (0 => F_Asgns, 1 => T_Asgns);
      Partial_Assign_Init (List);

      Min_Off := 0;
      loop
         Off := Min_Off;
         Extract_Merge_Partial_Assigns (Ctxt, P, N, Off, Wd);

         --  No more assignments.
         exit when Off = Uns32'Last and Wd = Width'Last;

         for I in N'Range loop
            if N (I) = No_Net then
               --  No partial assignment.  Get extract previous value.
               N (I) := Get_Current_Assign_Value (Ctxt, Wid, Off, Wd);
            end if;
         end loop;

         --  Possible optimizations:
         --  if C1 then            _          _                 _
         --    if C2 then      R0-|0\     R0-|0\           R0 -|0\
         --      R := V;   ==>    |  |--+    |  |- R   ==>     |  |- R
         --    end if;          V-|_/   +----|_/             V-|_/
         --  end if;               C1        C2                C1.C2
         --
         --  This really helps inference as the net R0 doesn't have to be
         --  walked twice (in absence of memoization).

         --  Build mux.
         N1_Inst := Get_Net_Parent (N (1));
         if Get_Id (N1_Inst) = Id_Mux2
           and then Same_Net (Get_Driver (Get_Mux2_I0 (N1_Inst)), N (0))
         then
            declare
               N1_Net : Net;
               N1_Sel : Input;
               N1_Sel_Net : Net;
            begin
               N1_Net := Get_Output (N1_Inst, 0);
               N1_Sel := Get_Input (N1_Inst, 0);
               N1_Sel_Net := Get_Driver (N1_Sel);
               if not Is_Connected (N1_Net) then
                  --  If the previous mux2 is not used, just modify it.
                  Res := N1_Net;
                  Disconnect (N1_Sel);
                  N1_Sel_Net := Build_Dyadic (Ctxt, Id_And, Sel, N1_Sel_Net);
                  Set_Location (N1_Sel_Net, Stmt);
                  Connect (N1_Sel, N1_Sel_Net);
               else
                  Res := Build_Dyadic (Ctxt, Id_And, Sel, N1_Sel_Net);
                  Set_Location (Res, Stmt);
                  Res := Build_Mux2
                    (Ctxt, Res, N (0), Get_Driver (Get_Mux2_I1 (N1_Inst)));
               end if;
            end;
         elsif N (0) = N (1) then
            --  Minor optimization: no need to add a mux if both sides are
            --  equal.  But this is important for the control wires.
            Res := N (0);
         else
            Res := Build_Mux2 (Ctxt, Sel, N (0), N (1));
         end if;
         Set_Location (Res, Stmt);

         --  Keep the result in a list.
         Pasgn := New_Partial_Assign (Res, Off);
         Partial_Assign_Append (List, Pasgn);

         Min_Off := Off + Wd;
      end loop;

      --  Do the assignments from the result list.
      --  It cannot be done before because the assignments will overwrite the
      --  last assignments which are read to create a partial assignment.
      Merge_Partial_Assigns (Ctxt, Wid, List);
   end Merge_Assigns;

   function Merge_Static_Assigns (Wid : Wire_Id; Tv, Fv : Seq_Assign_Value)
                                 return Boolean
   is
      Prev : Memtyp;
   begin
      --  First case: both TV and FV are static.
      if Tv.Is_Static = True and then Fv.Is_Static = True then
         if Is_Equal (Tv.Val, Fv.Val) then
            Phi_Assign_Static (Wid, Tv.Val);
            return True;
         else
            return False;
         end if;
      end if;

      --  If either TV or FV are nets, they cannot be merged.
      if Tv.Is_Static = False or else Fv.Is_Static = False then
         return False;
      end if;

      --  Get the previous value.
      declare
         Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
         pragma Assert (Wire_Rec.Kind /= Wire_None);
         First_Seq : Seq_Assign;
      begin
         --  Latest seq assign
         First_Seq := Wire_Rec.Cur_Assign;

         --  If no seq assign, fails.
         if First_Seq = No_Seq_Assign then
            return False;
         end if;

         if not Get_Assign_Is_Static (First_Seq) then
            return False;
         end if;
         Prev := Get_Assign_Static_Val (First_Seq);
      end;

      if Tv.Is_Static = True then
         pragma Assert (Fv = No_Seq_Assign_Value);
         return Is_Equal (Tv.Val, Prev);
      else
         pragma Assert (Fv.Is_Static = True);
         pragma Assert (Tv = No_Seq_Assign_Value);
         return Is_Equal (Fv.Val, Prev);
      end if;
   end Merge_Static_Assigns;

   --  Add muxes for two lists T and F of assignments.
   procedure Merge_Phis (Ctxt : Builders.Context_Acc;
                         Sel : Net;
                         T, F : Phi_Type;
                         Stmt : Source.Syn_Src)
   is
      T_Asgns : Seq_Assign;
      F_Asgns : Seq_Assign;
      W : Wire_Id;
      Tv, Fv : Seq_Assign_Value;
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
            Fv := Get_Seq_Assign_Value (F_Asgns);
            Tv := No_Seq_Assign_Value;
            F_Asgns := Get_Assign_Chain (F_Asgns);
         elsif F_Asgns = No_Seq_Assign
           or else (T_Asgns /= No_Seq_Assign
                      and then Get_Wire_Id (T_Asgns) < Get_Wire_Id (F_Asgns))
         then
            --  Has an assignment only for the true branch.
            W := Get_Wire_Id (T_Asgns);
            Fv := No_Seq_Assign_Value;
            Tv := Get_Seq_Assign_Value (T_Asgns);
            T_Asgns := Get_Assign_Chain (T_Asgns);
         else
            --  Has assignments for both the true and the false branch.
            pragma Assert (Get_Wire_Id (F_Asgns) = Get_Wire_Id (T_Asgns));
            W := Get_Wire_Id (F_Asgns);
            Fv := Get_Seq_Assign_Value (F_Asgns);
            Tv := Get_Seq_Assign_Value (T_Asgns);
            T_Asgns := Get_Assign_Chain (T_Asgns);
            F_Asgns := Get_Assign_Chain (F_Asgns);
         end if;
         --  Merge partial assigns as much as possible.  This reduce
         --  propagation of splits.
         Merge_Partial_Assignments (Ctxt, Fv);
         Merge_Partial_Assignments (Ctxt, Tv);
         if not Merge_Static_Assigns (W, Tv, Fv) then
            Merge_Assigns (Ctxt, W, Sel, Fv, Tv, Stmt);
         end if;

      end loop;
   end Merge_Phis;

   procedure Phi_Append_Assign (P : in out Phi_Type; Asgn : Seq_Assign) is
   begin
      --  Chain assignment in the current sequence.
      if P.First = No_Seq_Assign then
         P.First := Asgn;
      else
         Set_Assign_Chain (P.Last, Asgn);
      end if;
      P.Last := Asgn;
      P.Nbr := P.Nbr + 1;
   end Phi_Append_Assign;

   procedure Phi_Append_Assign (Asgn : Seq_Assign)
   is
      pragma Assert (Asgn /= No_Seq_Assign);
      Asgn_Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
      pragma Assert (Asgn_Rec.Phi = Current_Phi);
      pragma Assert (Asgn_Rec.Chain = No_Seq_Assign);
   begin
      Phi_Append_Assign (Phis_Table.Table (Phis_Table.Last), Asgn);
   end Phi_Append_Assign;

   function Phi_Enable (Ctxt : Builders.Context_Acc; Loc : Source.Syn_Src)
                       return Net
   is
      Last : constant Phi_Id := Phis_Table.Last;
      Wid : Wire_Id;
      N : Net;
      Asgn : Seq_Assign;
   begin
      if Last = No_Phi_Id then
         --  Can be called only when a phi is created.
         raise Internal_Error;
      end if;
      if Last = No_Phi_Id + 1 then
         --  That's the first phi, which is always enabled.
         return No_Net;
      end if;

      --  Cached value.
      Wid := Phis_Table.Table (Last).En;
      if Wid = No_Wire_Id then
         Wid := Alloc_Wire (Wire_Enable, Bit_Type, Loc);
         Phis_Table.Table (Last).En := Wid;

         --  Create the Enable gate.
         N := Build_Enable (Ctxt);
         Set_Location (N, Loc);
         Set_Wire_Gate (Wid, N);

         --  Initialize to '0'.
         --  This is really cheating, as it is like assigning in the first
         --  phi.
         Assign_Table.Append ((Phi => No_Phi_Id + 1,
                               Id => Wid,
                               Prev => No_Seq_Assign,
                               Chain => No_Seq_Assign,
                               Val => (Is_Static => True, Val => Bit0)));
         Asgn := Assign_Table.Last;
         Wire_Id_Table.Table (Wid).Cur_Assign := Asgn;
         Phi_Append_Assign (Phis_Table.Table (No_Phi_Id + 1), Asgn);

         --  Assign to '1'.
         Phi_Assign_Static (Wid, Bit1);
         return N;
      else
         return Get_Current_Value (Ctxt, Wid);
      end if;
   end Phi_Enable;

   --  Check consistency:
   --  - ordered.
   --  - no overlaps.
   procedure Check (Seq : Seq_Assign)
   is
      Seq_Asgn : Seq_Assign_Record renames Assign_Table.Table (Seq);
      Prev_El : Partial_Assign;
   begin
      Prev_El := Seq_Asgn.Val.Asgns;
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
      El := Seq_Asgn.Val.Asgns;
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
                        Seq_Asgn.Val.Asgns := Asgn;
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
                  P.Value := Build2_Extract_Push (Ctxt, P.Value,
                                                  Off => V_Next - P.Offset,
                                                  W => P_Next - V_Next);
                  P.Offset := V_Next;
                  if not Inserted then
                     if Last_El /= No_Partial_Assign then
                        Partial_Assign_Table.Table (Last_El).Next := Asgn;
                     else
                        Seq_Asgn.Val.Asgns := Asgn;
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
                  P.Value := Build2_Extract_Push (Ctxt, P.Value,
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
                      Value => Build2_Extract_Push (Ctxt, P.Value,
                                                    Off => V_Next - P.Offset,
                                                    W => P_Next - V_Next),
                      Offset => V_Next));
                  V.Next := Partial_Assign_Table.Last;
                  P.Value := Build2_Extract_Push (Ctxt, P.Value,
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
                        Seq_Asgn.Val.Asgns := Asgn;
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
     (Ctxt : Builders.Context_Acc; Dest : Wire_Id; Pasgn : Partial_Assign)
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Dest);
      pragma Assert (Wire_Rec.Kind /= Wire_None);
      Cur_Asgn : constant Seq_Assign := Wire_Rec.Cur_Assign;
   begin
      if Cur_Asgn = No_Seq_Assign
        or else Assign_Table.Table (Cur_Asgn).Phi < Current_Phi
      then
         --  Never assigned, or first assignment in that level
         Assign_Table.Append ((Phi => Current_Phi,
                               Id => Dest,
                               Prev => Cur_Asgn,
                               Chain => No_Seq_Assign,
                               Val => (Is_Static => False, Asgns => Pasgn)));
         Wire_Rec.Cur_Assign := Assign_Table.Last;
         Phi_Append_Assign (Assign_Table.Last);
      else
         --  Overwrite.
         if Get_Assign_Is_Static (Cur_Asgn) then
            --  Force seq_assign to be a net.
            declare
               Asgn_Rec : Seq_Assign_Record renames
                 Assign_Table.Table (Cur_Asgn);
               N : Net;
               Pa : Partial_Assign;
            begin
               N := Synth.Context.Get_Memtyp_Net (Ctxt, Asgn_Rec.Val.Val);
               Pa := New_Partial_Assign (N, 0);
               Asgn_Rec.Val := (Is_Static => False, Asgns => Pa);
            end;
         end if;

         Insert_Partial_Assign (Ctxt, Cur_Asgn, Pasgn);
      end if;
   end Phi_Assign;

   procedure Phi_Assign_Net
     (Ctxt : Builders.Context_Acc; Dest : Wire_Id; Val : Net; Offset : Uns32)
   is
      Pasgn : Partial_Assign;
   begin
      Pasgn := New_Partial_Assign (Val, Offset);

      Phi_Assign (Ctxt, Dest, Pasgn);
   end Phi_Assign_Net;

   procedure Phi_Assign_Static (Dest : Wire_Id; Val : Memtyp)
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Dest);
      pragma Assert (Wire_Rec.Kind /= Wire_None);
      Cur_Asgn : constant Seq_Assign := Wire_Rec.Cur_Assign;
   begin
      if Cur_Asgn = No_Seq_Assign
        or else Assign_Table.Table (Cur_Asgn).Phi < Current_Phi
      then
         --  Never assigned, or first assignment in that level
         Assign_Table.Append ((Phi => Current_Phi,
                               Id => Dest,
                               Prev => Cur_Asgn,
                               Chain => No_Seq_Assign,
                               Val => (Is_Static => True, Val => Val)));
         Wire_Rec.Cur_Assign := Assign_Table.Last;
         Phi_Append_Assign (Assign_Table.Last);
      else
         Assign_Table.Table (Cur_Asgn).Val := (Is_Static => True, Val => Val);
      end if;
   end Phi_Assign_Static;

   --  Return the net driving WID when it is known to be possibly constant.
   --  Return No_Net is not constant.
   function Is_Static_Wire (Wid : Wire_Id) return Boolean
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      if Wire_Rec.Kind /= Wire_Variable then
         return False;
      end if;
      if Wire_Rec.Cur_Assign = No_Seq_Assign then
         return False;
      end if;
      return Get_Assign_Is_Static (Wire_Rec.Cur_Assign);
   end Is_Static_Wire;

   function Get_Static_Wire (Wid : Wire_Id) return Memtyp
   is
      Wire_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      return Get_Assign_Static_Val (Wire_Rec.Cur_Assign);
   end Get_Static_Wire;
begin
   Wire_Id_Table.Append ((Kind => Wire_None,
                          Mark_Flag => False,
                          Decl => Source.No_Syn_Src,
                          Typ => null,
                          Gate => No_Net,
                          Cur_Assign => No_Seq_Assign,
                          Final_Assign => No_Conc_Assign,
                          Nbr_Final_Assign => 0));
   pragma Assert (Wire_Id_Table.Last = No_Wire_Id);

   Assign_Table.Append ((Phi => No_Phi_Id,
                         Id => No_Wire_Id,
                         Prev => No_Seq_Assign,
                         Chain => No_Seq_Assign,
                         Val => (Is_Static => False,
                                 Asgns => No_Partial_Assign)));
   pragma Assert (Assign_Table.Last = No_Seq_Assign);

   Partial_Assign_Table.Append ((Next => No_Partial_Assign,
                                 Value => No_Net,
                                 Offset => 0));
   pragma Assert (Partial_Assign_Table.Last = No_Partial_Assign);

   Phis_Table.Append ((First => No_Seq_Assign,
                       Last => No_Seq_Assign,
                       Nbr => 0,
                       En => No_Wire_Id));
   pragma Assert (Phis_Table.Last = No_Phi_Id);

   Conc_Assign_Table.Append ((Next => No_Conc_Assign,
                              Value => No_Net,
                              Offset => 0,
                              Stmt => Source.No_Syn_Src));
   pragma Assert (Conc_Assign_Table.Last = No_Conc_Assign);
end Synth.Environment;
