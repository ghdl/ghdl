--  Verilog dynamic arrays
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Ada.Unchecked_Deallocation;

package body Verilog.Sv_Arrays is
   procedure Delete (Arr : in out Sv_Dyn_Array_Ptr)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Sv_Dyn_Array_Type, Sv_Dyn_Array_Ptr);
   begin
      Free (Arr);
   end Delete;
end Verilog.Sv_Arrays;
