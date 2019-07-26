--  Types synthesis.
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

with Types; use Types;
with Mutils; use Mutils;
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Utils; use Vhdl.Utils;

package body Synth.Types is
   function Is_Bit_Type (Atype : Node) return Boolean
   is
      Btype : Node;
   begin
      if Atype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type
        or else Atype = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Type
        or else Atype = Vhdl.Std_Package.Boolean_Type_Definition
        or else Atype = Vhdl.Std_Package.Bit_Type_Definition
      then
         return True;
      end if;
      Btype := Get_Base_Type (Atype);
      if Btype = Atype then
         return False;
      else
         return Is_Bit_Type (Btype);
      end if;
   end Is_Bit_Type;

   function Is_Vector_Type (Atype : Node) return Boolean is
   begin
      return Is_Bit_Type (Get_Element_Subtype (Atype))
        and then Get_Nbr_Dimensions (Atype) = 1;
   end Is_Vector_Type;

   function Get_Range_Width (Rng : Value_Range_Type) return Width
   is
      Low : Int64;
      High : Int64;
   begin
      case Rng.Dir is
         when Iir_To =>
            Low := Rng.Left;
            High := Rng.Right;
         when Iir_Downto =>
            Low := Rng.Right;
            High := Rng.Left;
      end case;
      if Low > High then
         return 0;
      end if;

      if Low >= 0 then
         --  Positive.
         return Width (Clog2 (Uns64 (High)));
      elsif High < 0 then
         --  Negative only.
         --  FIXME: overflow.
         return Width (Clog2 (Uns64 (-Low))) + 1;
      else
         declare
            --  FIXME: overflow.
            L : constant Width := Width (Clog2 (Uns64 (-Low)));
            H : constant Width := Width (Clog2 (Uns64 (-High)));
         begin
            return Width'Max (L, H) + 1;
         end;
      end if;
   end Get_Range_Width;
end Synth.Types;
