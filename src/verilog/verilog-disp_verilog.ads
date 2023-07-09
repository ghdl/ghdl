--  Verilog disp source
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

package Verilog.Disp_Verilog is
   --  Disp implicit cast.
   Flag_Disp_Implicit_Cast : Boolean := False;

   --  Disp implicit types as if it were explicit.
   Flag_Disp_Implicit_Type : Boolean := False;

   --  Disp omitted part in port declarations (direction, data_type, kind).
   Flag_Disp_Port_Omitted : Boolean := False;

   procedure Disp_Item (Item : Node);
   procedure Disp_Module (M : Node; Indent : Natural := 0);
   procedure Disp_Source (Source : Node);

   procedure Disp_One_Net_Declaration (Indent : Natural; Net : Node);
   procedure Disp_Expression (Expr : Node);
   procedure Disp_Control (Ctrl : Node);
   procedure Disp_If_Header (Stmt : Node);
   procedure Disp_While_Header (Stmt : Node);
   procedure Disp_For_Header (Stmt : Node);
   procedure Disp_Case_Header (Stmt : Node);
   procedure Disp_Blocking_Assignment (Stmt : Node);
   procedure Disp_Continuous_Assignment (Stmt : Node);
   procedure Disp_Non_Blocking_Assignment (Stmt : Node);
   procedure Disp_Gate_Kind (Gate : Node);
end Verilog.Disp_Verilog;
