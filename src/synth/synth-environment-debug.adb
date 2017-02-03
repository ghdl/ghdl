--  Debug utilities for synthesis environment.
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

with Ada.Text_IO; use Ada.Text_IO;
with Netlists.Dump; use Netlists.Dump;

package body Synth.Environment.Debug is
   procedure Dump_Wire_Id (Id : Wire_Id)
   is
      W_Rec : Wire_Id_Record renames Wire_Id_Table.Table (Id);
   begin
      Put ("Wire:" & Wire_Id'Image (Id));
      Put_Line ("  kind: " & Wire_Kind'Image (W_Rec.Kind));
      Put_Line (" decl:" & Source.Syn_Src'Image (W_Rec.Decl));
      Put (" Init: ");
      Dump_Net_Name (W_Rec.Gate);
      New_Line;
      Put_Line (" cur_assign:" & Assign'Image (W_Rec.Cur_Assign));
   end Dump_Wire_Id;

   procedure Dump_Assign (Asgn : Assign)
   is
      procedure Dump_Value (N : Net) is
      begin
         if N /= No_Net then
            Dump_Net_Name (N);
            Put (" := ");
            Disp_Instance (Get_Parent (N), False);
         else
            Put ("unassigned");
         end if;
      end Dump_Value;
      Rec : Assign_Record renames Assign_Table.Table (Asgn);
   begin
      Put ("Assign" & Assign'Image (Asgn));
      Put (" Id:" & Wire_Id'Image (Rec.Id));
      Put (", prev_assign:" & Assign'Image (Rec.Prev));
      Put (", phi:" & Phi_Id'Image (Rec.Phi));
      Put (", chain:" & Assign'Image (Rec.Chain));
      New_Line;
      Put (" value: ");
      Dump_Value (Rec.Value);
      New_Line;
   end Dump_Assign;

   procedure Dump_Phi (Id : Phi_Id)
   is
      Phi : Phi_Type renames Phis_Table.Table (Id);
      Asgn : Assign;
   begin
      Put ("phi_id:" & Phi_Id'Image (Id) & ", nbr:" & Uns32'Image (Phi.Nbr));
      New_Line;
      Asgn := Phi.First;
      while Asgn /= No_Assign loop
         Dump_Assign (Asgn);
         Asgn := Get_Assign_Chain (Asgn);
      end loop;
   end Dump_Phi;
end Synth.Environment.Debug;
