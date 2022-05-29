--  Nodes recognizer for std.env.
--  Copyright (C) 2022 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Std_Env is
   Std_Env_Pkg : Iir_Package_Declaration := Null_Iir;

   --  Extract declarations from PKG (std_env).
   procedure Extract_Declarations (Pkg : Iir_Package_Declaration);
end Vhdl.Std_Env;
