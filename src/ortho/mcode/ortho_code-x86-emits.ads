--  Mcode back-end for ortho - Binary X86 instructions generator.
--  Copyright (C) 2006 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Binary_File; use Binary_File;
with Ortho_Code.Exprs; use Ortho_Code.Exprs;

package Ortho_Code.X86.Emits is
   procedure Init;
   procedure Finish;

   procedure Emit_Subprg (Subprg : Subprogram_Data_Acc);

   procedure Emit_Var_Decl (Decl : O_Dnode);
   procedure Emit_Var_Zero (Decl : O_Dnode);

   procedure Emit_Const_Decl (Decl : O_Dnode);
   procedure Emit_Init_Value (Decl : O_Dnode; Val : O_Cnode);

   type Intrinsic_Symbols_Map is array (Intrinsics_X86) of Symbol;
   Intrinsics_Symbol : Intrinsic_Symbols_Map;

   Mcount_Symbol : Symbol;
   Chkstk_Symbol : Symbol;
end Ortho_Code.X86.Emits;
