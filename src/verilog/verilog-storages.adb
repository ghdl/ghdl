--  Verilog data storage
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

package body Verilog.Storages is
   function "+" (Data : Data_Ptr; Off : Storage_Index) return Data_Ptr is
   begin
      return To_Storage (Data)(Off)'Address;
   end "+";

   function Is_Null (Data : Data_Ptr) return Boolean
   is
      use System;
   begin
      return Data = Null_Address;
   end Is_Null;
end Verilog.Storages;
