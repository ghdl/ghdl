--  Mcode back-end for ortho - mcode to X86 instructions.
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
package Ortho_Code.X86.Insns is
   function Reg_Used (Reg : Regs_R32) return Boolean;

   --  Split enodes of SUBPRG into instructions.
   procedure Gen_Subprg_Insns (Subprg : Subprogram_Data_Acc);

end Ortho_Code.X86.Insns;

