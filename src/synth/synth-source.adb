--  Source/origin of synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Netlists; use Netlists;
with Netlists.Locations; use Netlists.Locations;

package body Synth.Source is
   procedure Set_Location2 (N : Net; Loc : Node) is
   begin
      Set_Location (Get_Net_Parent (N), Get_Location (Loc));
   end Set_Location2;

   procedure Set_Location2 (Inst : Instance; Loc : Node) is
   begin
      Set_Location (Inst, Get_Location (Loc));
   end Set_Location2;

   procedure Set_Location (N : Net; Src : Syn_Src) is
   begin
      --  Short and compact code as it is inlined.
      if Flag_Locations then
         Set_Location2 (N, Src);
      end if;
   end Set_Location;

   procedure Set_Location (Inst : Instance; Src : Syn_Src) is
   begin
      --  Short and compact code as it is inlined.
      if Flag_Locations then
         Set_Location2 (Inst, Src);
      end if;
   end Set_Location;

   procedure Set_Location_Maybe2 (Inst : Netlists.Instance; Src : Syn_Src) is
   begin
      if Get_Location (Inst) /= No_Location then
         return;
      end if;
      Set_Location2 (Inst, Src);
   end Set_Location_Maybe2;

   procedure Set_Location_Maybe (Inst : Netlists.Instance; Src : Syn_Src) is
   begin
      if Flag_Locations then
         Set_Location_Maybe2 (Inst, Src);
      end if;
   end Set_Location_Maybe;

   procedure Set_Location_Maybe2 (N : Netlists.Net; Src : Syn_Src) is
   begin
      Set_Location_Maybe2 (Get_Net_Parent (N), Src);
   end Set_Location_Maybe2;

   procedure Set_Location_Maybe (N : Netlists.Net; Src : Syn_Src) is
   begin
      if Flag_Locations then
         Set_Location_Maybe2 (N, Src);
      end if;
   end Set_Location_Maybe;
end Synth.Source;
