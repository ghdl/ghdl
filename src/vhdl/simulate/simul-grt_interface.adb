--  Interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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

with Types; use Types;

with Vhdl.Nodes; use Vhdl.Nodes;

package body Simul.Grt_Interface is
   To_Dir : constant array (Direction_Type) of Ghdl_Dir_Type :=
     (Dir_To => Dir_To, Dir_Downto => Dir_Downto);

   function Build_Bound (Arr : Iir_Value_Literal_Acc) return Std_String_Bound
   is
      Rng : constant Iir_Value_Literal_Acc := Arr.Bounds.D (1);
   begin
      return (Dim_1 => (Left => Std_Integer (Rng.Left.I64),
                        Right => Std_Integer (Rng.Right.I64),
                        Dir => To_Dir (Rng.Dir),
                        Length => Ghdl_Index_Type (Rng.Length)));
   end Build_Bound;

   procedure Set_Std_String_From_Iir_Value (Str : Std_String;
                                            Val : Iir_Value_Literal_Acc) is
   begin
      for I in Val.Val_Array.V'Range loop
         Str.Base (Ghdl_Index_Type (I - 1)) :=
           Character'Val (Val.Val_Array.V (I).E8);
      end loop;
   end Set_Std_String_From_Iir_Value;
end Simul.Grt_Interface;
