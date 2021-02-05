--  Build utilities
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

with Netlists.Locations; use Netlists.Locations;

package body Netlists.Butils is
   procedure Synth_Case (Ctxt : Context_Acc;
                         Sel : Net;
                         Els : in out Case_Element_Array;
                         Default : Net;
                         Res : out Net;
                         Sel_Loc : Location_Type)
   is
      Wd : constant Width := Get_Width (Sel);
      Mask : Uns64;
      Sub_Sel : Net;
      Lels : Natural;
      Iels : Natural;
      Oels : Natural;
   begin
      Lels := Els'Last;
      Iels := Els'First;

      if Lels < Iels then
         --  No choices
         Res := Default;
         return;
      end if;

      --  Handle SEL bits by 2, so group case_element by 4.
      for I in 1 .. Natural (Wd / 2) loop
         Mask := Shift_Left (not 0, Natural (2 * I));
         Iels := Els'First;
         Oels := Els'First;
         while Iels <= Lels loop
            declare
               type Net4 is array (0 .. 3) of Net;
               G : Net4;
               S_Group : constant Uns64 := Els (Iels).Sel and Mask;
               S_El : Uns64;
               El_Idx : Natural;
               Rsel : Net;
            begin
               --  Extract 2 bits from the selector.
               G := (others => Default);
               for K in 0 .. 3 loop
                  exit when Iels > Lels;
                  S_El := Els (Iels).Sel;
                  exit when (S_El and Mask) /= S_Group;
                  El_Idx := Natural
                    (Shift_Right (S_El, Natural (2 * (I - 1))) and 3);
                  G (El_Idx) := Els (Iels).Val;
                  Iels := Iels + 1;
               end loop;
               if G (0) /= No_Net
                 and G (1) /= No_Net
                 and G (2) /= No_Net
                 and G (3) /= No_Net
               then
                  --  The 4 choices are available.
                  if G (0) = G (1) and G (0) = G (2) and G (0) = G (3) then
                     --  But they are the same: no need to choose!
                     Rsel := G (0);
                  else
                     Sub_Sel := Build_Extract (Ctxt,
                                               Sel, Width (2 * (I - 1)), 2);
                     Set_Location (Sub_Sel, Sel_Loc);

                     Rsel := Build_Mux4 (Ctxt,
                                         Sub_Sel, G (0), G (1), G (2), G (3));
                     Set_Location (Rsel, Sel_Loc);
                  end if;
               else
                  for K in 0 .. 1 loop
                     if G (2 * K) /= No_Net and G (2 * K + 1) /= No_Net then
                        G (K) := Build_Mux2
                          (Ctxt,
                           Build_Extract_Bit
                             (Ctxt, Sel, Width (2 * (I - 1))),
                           G (2 * K), G (2 * K + 1));
                        Set_Location (G (K), Sel_Loc);
                     else
                        if G (2 * K) /= No_Net then
                           G (K) := G (2 * K);
                        else
                           G (K) := G (2 * K + 1);
                        end if;
                     end if;
                  end loop;
                  if G (0) /= No_Net and G (1) /= No_Net then
                     Rsel := Build_Mux2
                       (Ctxt,
                        Build_Extract_Bit (Ctxt, Sel,
                                           Width (2 * (I - 1) + 1)),
                        G (0), G (1));
                     Set_Location (Rsel, Sel_Loc);
                  else
                     if G (0) /= No_Net then
                        Rsel := G (0);
                     else
                        Rsel := G (1);
                     end if;
                  end if;
               end if;
               Els (Oels) := (Sel => S_Group, Val => Rsel);
               Oels := Oels + 1;
            end;
         end loop;
         Lels := Oels - 1;
      end loop;

      --  If the width is not a multiple of 2, handle the last level.
      if Wd mod 2 = 1 then
         if Wd = 1 then
            Sub_Sel := Sel;
         else
            Sub_Sel := Build_Extract_Bit (Ctxt, Sel, Wd - 1);
            Set_Location (Sub_Sel, Sel_Loc);
         end if;
         Iels := Els'First;
         Oels := Els'First;
         while Iels <= Lels loop
            declare
               type Net2 is array (0 .. 1) of Net;
               G : Net2;
               S_Group : constant Uns64 := Els (Iels).Sel and Mask;
               S_El : Uns64;
               El_Idx : Natural;
               Rsel : Net;
            begin
               G := (others => Default);
               for K in 0 .. 1 loop
                  exit when Iels > Lels;
                  S_El := Els (Iels).Sel;
                  El_Idx := Natural
                    (Shift_Right (S_El, Natural (Wd - 1)) and 1);
                  G (El_Idx) := Els (Iels).Val;
                  Iels := Iels + 1;
               end loop;
               Rsel := Build_Mux2 (Ctxt, Sub_Sel, G (0), G (1));
               Set_Location (Rsel, Sel_Loc);
               Els (Oels) := (Sel => S_Group, Val => Rsel);
               Oels := Oels + 1;
            end;
         end loop;
         Lels := Oels - 1;
      end if;
      pragma Assert (Lels = Els'First);
      Res := Els (Els'First).Val;
   end Synth_Case;
end Netlists.Butils;
