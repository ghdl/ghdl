--  Canonicalization pass for PSL.
--  Copyright (C) 2009 Tristan Gingold
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

with PSL.Nodes; use PSL.Nodes;
with PSL.Errors; use PSL.Errors;
with Canon; use Canon;
with Iirs_Utils; use Iirs_Utils;

package body Canon_PSL is
   --  Version of Canon.Canon_Extract_Sensitivity for PSL nodes.
   procedure Canon_Extract_Sensitivity
     (Expr: PSL_Node; Sensitivity_List: Iir_List)
   is
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr =>
            Canon_Extract_Sensitivity (Get_HDL_Node (Expr), Sensitivity_List);
         when N_And_Bool
           | N_Or_Bool =>
            Canon_Extract_Sensitivity (Get_Left (Expr), Sensitivity_List);
            Canon_Extract_Sensitivity (Get_Right (Expr), Sensitivity_List);
         when N_Not_Bool =>
            Canon_Extract_Sensitivity (Get_Boolean (Expr), Sensitivity_List);
         when others =>
            Error_Kind ("PSL.Canon_extract_Sensitivity", Expr);
      end case;
   end Canon_Extract_Sensitivity;
end Canon_PSL;
