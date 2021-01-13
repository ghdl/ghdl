--  Mcode back-end for ortho - mcode to X86 instructions.
--  Copyright (C) 2006 Tristan Gingold
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
with Ortho_Code.Exprs; use Ortho_Code.Exprs;

package Ortho_Code.X86.Insns is
   --  Return True iff OBJ is in a different module.
   --  This applies to x86-64 only as in that case RIP relative addressing
   --  cannot be used.
   function Is_External_Object (Obj : O_Dnode) return Boolean;

   function Reg_Used (Reg : Regs_R64) return Boolean;

   --  Split enodes of SUBPRG into instructions.
   procedure Gen_Subprg_Insns (Subprg : Subprogram_Data_Acc);

   --  Convert a KIND to a reg.
   function Ekind_Unsigned_To_Cc (Kind : OE_Kind_Cmp) return O_Reg;
   function Ekind_Signed_To_Cc (Kind : OE_Kind_Cmp) return O_Reg;
end Ortho_Code.X86.Insns;
