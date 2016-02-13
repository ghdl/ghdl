--  VHDL regeneration from internal nodes.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Types; use Types;
with Iirs; use Iirs;

package Disp_Vhdl is
   -- General procedure to display a node.
   -- Mainly used to dispatch to other functions according to the kind of
   -- the node.
   procedure Disp_Vhdl (An_Iir: Iir);

   procedure Disp_PSL_NFA (N : PSL_NFA);

   procedure Disp_Expression (Expr: Iir);
   --  Display an expression.

   -- Disp an iir_int64, without the leading blank.
   procedure Disp_Int64 (Val: Iir_Int64);

   -- Disp an iir_int32, without the leading blank.
   procedure Disp_Int32 (Val: Iir_Int32);

   -- Disp an iir_Fp64, without the leading blank.
   procedure Disp_Fp64 (Val: Iir_Fp64);
end Disp_Vhdl;
