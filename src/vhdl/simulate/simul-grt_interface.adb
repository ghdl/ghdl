--  Interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Iirs; use Iirs;
with Types; use Types;

package body Simul.Grt_Interface is
   To_Dir : constant array (Iir_Direction) of Ghdl_Dir_Type :=
     (Iir_To => Dir_To, Iir_Downto => Dir_Downto);

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
