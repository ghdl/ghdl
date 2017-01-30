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

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;
with Synth.Inference;

package body Synth.Environment is
   function Get_Wire_Id (W : Assign) return Wire_Id is
   begin
      return Assign_Table.Table (W).Id;
   end Get_Wire_Id;

   function Get_Assign_Prev (Asgn : Assign) return Assign is
   begin
      return Assign_Table.Table (Asgn).Prev;
   end Get_Assign_Prev;

   function Get_Assign_Chain (Asgn : Assign) return Assign is
   begin
      return Assign_Table.Table (Asgn).Chain;
   end Get_Assign_Chain;

   procedure Set_Assign_Chain (Asgn : Assign; Chain : Assign) is
   begin
      Assign_Table.Table (Asgn).Chain := Chain;
   end Set_Assign_Chain;

   procedure Push_Phi is
   begin
      Phis_Table.Append ((First => No_Assign,
                          Nbr => 0));
   end Push_Phi;

   procedure Pop_Phi (Phi : out Phi_Type)
   is
      Cur_Phi : constant Phi_Id := Current_Phi;
      Asgn : Assign;
   begin
      Phi := Phis_Table.Table (Cur_Phi);
      Phis_Table.Decrement_Last;

      --  Point to previous wires.
      Asgn := Phi.First;
      while Asgn /= No_Assign loop
         pragma Assert (Assign_Table.Table (Asgn).Phi = Cur_Phi);
         Wire_Id_Table.Table (Get_Wire_Id (Asgn)).Cur_Assign :=
           Get_Assign_Prev (Asgn);
         Asgn := Get_Assign_Chain (Asgn);
      end loop;
   end Pop_Phi;

   procedure Pop_And_Merge_Phi (Ctxt : Builders.Context_Acc)
   is
      Phi : Phi_Type;
      Asgn : Assign;
   begin
      Pop_Phi (Phi);
      Asgn := Phi.First;
      while Asgn /= No_Assign loop
         declare
            Asgn_Rec : Assign_Record renames Assign_Table.Table (Asgn);
            Outport : constant Net := Wire_Id_Table.Table (Asgn_Rec.Id).Gate;
            --  Must be connected to an Id_Output or Id_Signal
            pragma Assert (Outport /= No_Net);
            Gate_Inst : Instance;
            Gate_In : Input;
            Drv : Net;
            New_Sig : Net;
         begin
            Gate_Inst := Get_Parent (Outport);
            Gate_In := Get_Input (Gate_Inst, 0);
            Drv := Get_Driver (Gate_In);

            case Wire_Id_Table.Table (Asgn_Rec.Id).Kind is
               when Wire_Output
                 | Wire_Signal
                 | Wire_Variable =>
                  if Drv /= No_Net then
                     --  Output already assigned
                     raise Internal_Error;
                  else
                     Drv := Inference.Infere (Ctxt, Asgn_Rec.Value, Outport);

                     if Get_Id (Gate_Inst) = Id_Isignal
                       and then Get_Driver (Get_Input (Gate_Inst, 1)) = No_Net
                     then
                        --  Mutate Isignal to signal.
                        New_Sig := Build_Signal
                          (Ctxt, Get_Name (Gate_Inst), Get_Width (Outport));
                        Connect (Get_Input (Get_Parent (New_Sig), 0), Drv);
                        Redirect_Inputs (Outport, New_Sig);
                        Wire_Id_Table.Table (Asgn_Rec.Id).Gate := New_Sig;
                        Free_Instance (Gate_Inst);
                     else
                        Connect (Gate_In, Drv);
                     end if;
                  end if;
               when others =>
                  raise Internal_Error;
            end case;

            Asgn := Asgn_Rec.Chain;
         end;
      end loop;
      --  FIXME: free wires.
   end Pop_And_Merge_Phi;

   --  Sort the LEN first wires of chain W (linked by Chain) in Id increasing
   --  values.  The result is assigned to FIRST and the first non-sorted wire
   --  (the one after LEN) is assigned to NEXT.  The chain headed by FIRST
   --  is truncated to LEN elements.
   --  Use a merge sort.
   procedure Sort_Wires
     (Asgn : Assign; Len : Uns32; First : out Assign; Next : out Assign)
   is
      Left, Right : Assign;
      Last : Assign;
      El : Assign;
   begin
      if Len = 0 then
         --  Empty chain.
         First := No_Assign;
         Next := Asgn;
         return;
      elsif Len = 1 then
         --  Chain with one element.
         First := Asgn;
         Next := Get_Assign_Chain (First);
         Set_Assign_Chain (First, No_Assign);
         return;
      else
         --  Divide.
         Sort_Wires (Asgn, Len / 2, Left, Right);
         Sort_Wires (Right, Len - Len / 2, Right, Next);

         --  Conquer: merge.
         First := No_Assign;
         Last := No_Assign;
         for I in 1 .. Len loop
            if Left /= No_Assign
              and then (Right = No_Assign
                          or else Get_Wire_Id (Left) <= Get_Wire_Id (Right))
            then
               El := Left;
               Left := Get_Assign_Chain (Left);
            else
               pragma Assert (Right /= No_Assign);
               El := Right;
               Right := Get_Assign_Chain (Right);
            end if;

            --  Append
            if First = No_Assign then
               First := El;
            else
               Set_Assign_Chain (Last, El);
            end if;
            Last := El;
         end loop;
         Set_Assign_Chain (Last, No_Assign);
      end if;
   end Sort_Wires;

   function Sort_Phi (P : Phi_Type) return Assign
   is
      Res, Last : Assign;
   begin
      Sort_Wires (P.First, P.Nbr, Res, Last);
      pragma Assert (Last = No_Assign);
      return Res;
   end Sort_Phi;

   function Get_Assign_Value (Asgn : Assign) return Net
   is
      Asgn_Rec : Assign_Record renames Assign_Table.Table (Asgn);
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
            if Wid_Rec.Cur_Assign = No_Assign then
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
      if Wid_Rec.Cur_Assign = No_Assign then
         return Wid_Rec.Gate;
      else
         return Get_Assign_Value (Wid_Rec.Cur_Assign);
      end if;
   end Get_Last_Assigned_Value;

   procedure Merge_Phis (Ctxt : Builders.Context_Acc;
                         Sel : Net;
                         T, F : Phi_Type)
   is
      T_Asgns : Assign;
      F_Asgns : Assign;
      W : Wire_Id;
      Te, Fe : Net;
      Res : Net;
   begin
      T_Asgns := Sort_Phi (T);
      F_Asgns := Sort_Phi (F);

      while T_Asgns /= No_Assign or F_Asgns /= No_Assign loop
         --  Extract a wire.
         if T_Asgns = No_Assign
           or else (F_Asgns /= No_Assign
                      and then Get_Wire_Id (F_Asgns) < Get_Wire_Id (T_Asgns))
         then
            W := Get_Wire_Id (F_Asgns);
            Te := Get_Last_Assigned_Value (W);
            Fe := Get_Assign_Value (F_Asgns);
            F_Asgns := Get_Assign_Chain (F_Asgns);
         elsif F_Asgns = No_Assign
           or else (T_Asgns /= No_Assign
                      and then Get_Wire_Id (T_Asgns) < Get_Wire_Id (F_Asgns))
         then
            W := Get_Wire_Id (T_Asgns);
            Te := Get_Assign_Value (T_Asgns);
            Fe := Get_Last_Assigned_Value (W);
            T_Asgns := Get_Assign_Chain (T_Asgns);
         else
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

   procedure Phi_Insert_Assign (Asgn : Assign)
   is
      pragma Assert (Asgn /= No_Assign);
      Asgn_Rec : Assign_Record renames Assign_Table.Table (Asgn);
      pragma Assert (Asgn_Rec.Phi = Current_Phi);
      pragma Assert (Asgn_Rec.Chain = No_Assign);
      P : Phi_Type renames Phis_Table.Table (Phis_Table.Last);
   begin
      Asgn_Rec.Chain := P.First;
      P.First := Asgn;
      P.Nbr := P.Nbr + 1;
   end Phi_Insert_Assign;

   procedure Phi_Assign (Dest : Wire_Id; Val : Net)
   is
      Cur_Asgn : constant Assign := Wire_Id_Table.Table (Dest).Cur_Assign;
   begin
      if Cur_Asgn = No_Assign
        or else Assign_Table.Table (Cur_Asgn).Phi < Current_Phi
      then
         --  Never assigned, or first assignment in that level
         Assign_Table.Append ((Phi => Current_Phi,
                              Id => Dest,
                              Prev => Cur_Asgn,
                              Chain => No_Assign,
                              Value => Val));
         Wire_Id_Table.Table (Dest).Cur_Assign := Assign_Table.Last;
         Phi_Insert_Assign (Assign_Table.Last);
      else
         --  Overwrite.
         --  FIXME: may need to merge in case of partial assignment.
         Assign_Table.Table (Cur_Asgn).Value := Val;
      end if;
   end Phi_Assign;

   function Current_Phi return Phi_Id is
   begin
      return Phis_Table.Last;
   end Current_Phi;
begin
   Wire_Id_Table.Append ((Kind => Wire_None,
                          Mark_Flag => False,
                          Decl => Source.No_Syn_Src,
                          Gate => No_Net,
                          Cur_Assign => No_Assign));
   pragma Assert (Wire_Id_Table.Last = No_Wire_Id);

   Assign_Table.Append ((Phi => No_Phi_Id,
                        Id => No_Wire_Id,
                        Prev => No_Assign,
                        Chain => No_Assign,
                        Value => No_Net));
   pragma Assert (Assign_Table.Last = No_Assign);

   Phis_Table.Append ((First => No_Assign,
                       Nbr => 0));
   pragma Assert (Phis_Table.Last = No_Phi_Id);
end Synth.Environment;
