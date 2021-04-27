--  Debug utilities for synthesis environment.
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

with Ada.Text_IO; use Ada.Text_IO;
with Netlists.Dump; use Netlists.Dump;

--  with Synth.Values.Debug; use Synth.Values.Debug;

package body Synth.Environment.Debug is
   procedure Put_Wire_Id (Wid : Wire_Id) is
   begin
      Put (Wire_Id'Image (Wid));
   end Put_Wire_Id;

   procedure Debug_Wire (Wid : Wire_Id)
   is
      W_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Wid);
   begin
      Put ("Wire:");
      Put_Wire_Id (Wid);
      Put_Line ("  kind: " & Wire_Kind'Image (W_Rec.Kind));
--      Put_Line (" decl:" & Source.Syn_Src'Image (W_Rec.Decl));
      Put (" gate: ");
      Dump_Net_Name (W_Rec.Gate, True);
      New_Line;
      Put_Line (" cur_assign:" & Seq_Assign'Image (W_Rec.Cur_Assign));
      Put_Line (" conc_assign:" & Conc_Assign'Image(W_Rec.Final_Assign));
   end Debug_Wire;

   procedure Dump_Partial_Assign (Pasgn : Partial_Assign)
   is
      procedure Dump_Value (N : Net) is
      begin
         if N /= No_Net then
            Dump_Net_Name (N, True);
            Put (" := ");
            Disp_Instance (Get_Net_Parent (N), False, 0);
         else
            Put ("unassigned");
         end if;
      end Dump_Value;
      P : Partial_Assign;
   begin
      P := Pasgn;
      while P /= No_Partial_Assign loop
         declare
            Pasgn : Partial_Assign_Record renames
              Partial_Assign_Table.Table (P);
         begin
            Put (" off:" & Uns32'Image (Pasgn.Offset));
            Put (", ");
            Dump_Value (Pasgn.Value);
            New_Line;
            P := Pasgn.Next;
         end;
      end loop;
   end Dump_Partial_Assign;

   procedure Debug_Assign (Asgn : Seq_Assign)
   is
      Rec : Seq_Assign_Record renames Assign_Table.Table (Asgn);
   begin
      Put ("Assign" & Seq_Assign'Image (Asgn));
      Put (" Wire Id:" & Wire_Id'Image (Rec.Id));
      Put (", prev_assign:" & Seq_Assign'Image (Rec.Prev));
      Put (", phi:" & Phi_Id'Image (Rec.Phi));
      Put (", chain:" & Seq_Assign'Image (Rec.Chain));
      New_Line;
      declare
         W_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Rec.Id);
      begin
--         Put_Line (" wire decl:" & Source.Syn_Src'Image (W_Rec.Decl));
         Put (" wire gate: ");
         Dump_Net_Name (W_Rec.Gate, True);
         New_Line;
      end;
      Put_Line (" value:");
      case Rec.Val.Is_Static is
         when Unknown =>
            Put_Line ("   ??? (unknown)");
         when True =>
            Put_Line ("   static:");
--            Debug_Memtyp (Rec.Val.Val);
         when False =>
            Dump_Partial_Assign (Rec.Val.Asgns);
      end case;
   end Debug_Assign;

   procedure Debug_Phi (Id : Phi_Id)
   is
      Phi : Phi_Type renames Phis_Table.Table (Id);
      Asgn : Seq_Assign;
   begin
      Put ("phi_id:" & Phi_Id'Image (Id) & ", nbr:" & Uns32'Image (Phi.Nbr));
      New_Line;
      Asgn := Phi.First;
      while Asgn /= No_Seq_Assign loop
         Debug_Assign (Asgn);
         Asgn := Get_Assign_Chain (Asgn);
      end loop;
   end Debug_Phi;

   procedure Debug_Conc_Assigns (First : Conc_Assign)
   is
      Asgn : Conc_Assign;
   begin
      Asgn := First;
      while Asgn /= No_Conc_Assign loop
         Put ("conc_assign" & Conc_Assign'Image (Asgn));
         declare
            Arec : Conc_Assign_Record renames Conc_Assign_Table.Table (Asgn);
         begin
            Put (" off:" & Uns32'Image (Arec.Offset));
            Put (", width:" & Width'Image (Get_Width (Arec.Value)));
            New_Line;
            Put ("  value: ");
            Disp_Instance (Get_Net_Parent (Arec.Value), False, 0);
            Asgn := Arec.Next;
         end;
         New_Line;
      end loop;
   end Debug_Conc_Assigns;
end Synth.Environment.Debug;
