--  std_logic_1164
--  Copyright (C) 2020 Tristan Gingold
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

package body Synth.Ieee.Std_Logic_1164 is
   function Read_Std_Logic (M : Memory_Ptr; Off : Uns32) return Std_Ulogic is
   begin
      return Std_Ulogic'Val (Read_U8 (M + Size_Type (Off)));
   end Read_Std_Logic;

   procedure Write_Std_Logic (M : Memory_Ptr; Off : Uns32; Val : Std_Ulogic) is
   begin
      Write_U8 (M + Size_Type (Off), Std_Ulogic'Pos (Val));
   end Write_Std_Logic;
end Synth.Ieee.Std_Logic_1164;
