--  Bignums storage
--  Copyright (C) 2023 Tristan Gingold
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

with Dyn_Tables;
with Types; use Types;
with Verilog.Types; use Verilog.Types;

package Verilog.Bn_Tables is
   package Bignum_Table is new Dyn_Tables
     (Table_Component_Type => Logic_32,
      Table_Index_Type => Bn_Index,
      Table_Low_Bound => No_Bn_Index + 1);

   Bn_Table : Bignum_Table.Instance;
end Verilog.Bn_Tables;
