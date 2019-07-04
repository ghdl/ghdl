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
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors; use Vhdl.Errors;

with Synth.Values; use Synth.Values;
with Synth.Expr;
with Vhdl.Annotations; use Vhdl.Annotations;

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

   function Get_Width (Syn_Inst : Synth_Instance_Acc; Atype : Node)
                      return Width
   is
      Btype : constant Node := Get_Base_Type (Atype);
   begin
      case Get_Kind (Atype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            return Width (Get_Info (Atype).Width);
         when Iir_Kind_Enumeration_Subtype_Definition =>
            --  Tail call
            return Get_Width (Syn_Inst, Btype);
         when Iir_Kind_Array_Subtype_Definition =>
            if Is_Vector_Type (Btype) then
               declare
                  Bnd : Value_Bound_Acc;
               begin
                  Bnd := Expr.Synth_Array_Bounds (Syn_Inst, Atype, 0);
                  return Bnd.Len;
               end;
            else
               raise Internal_Error;
            end if;
         when others =>
            Error_Kind ("get_width", Atype);
      end case;
   end Get_Width;

end Synth.Types;
