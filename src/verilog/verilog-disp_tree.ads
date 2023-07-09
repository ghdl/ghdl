--  Verilog tree dump
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

with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Disp_Tree is
   function Image_Binary_Ops (Op : Binary_Ops) return String;

   procedure Disp_Tree
     (N : Node; Indent : Natural := 0; Depth : Natural := 20);
end Verilog.Disp_Tree;
