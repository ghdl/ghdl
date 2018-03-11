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
with Std_Package;
with Ieee.Std_Logic_1164;
with Iirs_Utils; use Iirs_Utils;

with Simul.Environments; use Simul.Environments;
with Simul.Execution;
with Errorout; use Errorout;

package body Synth.Types is
   function Is_Bit_Type (Atype : Iir) return Boolean is
   begin
      return Atype = Ieee.Std_Logic_1164.Std_Ulogic_Type
        or else Atype = Ieee.Std_Logic_1164.Std_Logic_Type
        or else Atype = Std_Package.Boolean_Type_Definition
        or else Atype = Std_Package.Bit_Type_Definition;
   end Is_Bit_Type;

   function Is_Vector_Type (Atype : Iir) return Boolean is
   begin
      return Is_Bit_Type (Get_Element_Subtype (Atype))
        and then Get_Nbr_Dimensions (Atype) = 1;
   end Is_Vector_Type;

   function Get_Width (Syn_Inst : Synth_Instance_Acc; Atype : Iir)
                      return Width
   is
      Btype : constant Iir := Get_Base_Type (Atype);
   begin
      case Get_Kind (Atype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            if Is_Bit_Type (Atype) then
               return 1;
            else
               raise Internal_Error;
            end if;
         when Iir_Kind_Enumeration_Subtype_Definition =>
            --  Tail call
            return Get_Width (Syn_Inst, Btype);
         when Iir_Kind_Array_Subtype_Definition =>
            if Is_Vector_Type (Btype) then
               declare
                  Bnd : Iir_Value_Literal_Acc;
               begin
                  Bnd := Simul.Execution.Execute_Bounds
                    (Syn_Inst.Sim,
                     Get_Nth_Element (Get_Index_Subtype_List (Atype), 0));
                  return Width (Bnd.Length);
               end;
            else
               raise Internal_Error;
            end if;
         when others =>
            Error_Kind ("get_width", Atype);
      end case;
   end Get_Width;

end Synth.Types;
