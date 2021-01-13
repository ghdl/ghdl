--  Mcode back-end for ortho - X86 common definitions.
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
package body Ortho_Code.X86 is
   function Inverse_Cc (R : O_Reg) return O_Reg is
   begin
      case R is
         when R_Ult =>
            return R_Uge;
         when R_Uge =>
            return R_Ult;
         when R_Eq =>
            return R_Ne;
         when R_Ne =>
            return R_Eq;
         when R_Ule =>
            return R_Ugt;
         when R_Ugt =>
            return R_Ule;
         when R_Slt =>
            return R_Sge;
         when R_Sge =>
            return R_Slt;
         when R_Sle =>
            return R_Sgt;
         when R_Sgt =>
            return R_Sle;
         when others =>
            raise Program_Error;
      end case;
   end Inverse_Cc;

   function Get_Pair_High (Reg : Regs_Pair) return Regs_R32 is
   begin
      case Reg is
         when R_Edx_Eax =>
            return R_Dx;
         when R_Ebx_Ecx =>
            return R_Bx;
         when R_Esi_Edi =>
            return R_Si;
      end case;
   end Get_Pair_High;

   function Get_Pair_Low (Reg : Regs_Pair) return Regs_R32 is
   begin
      case Reg is
         when R_Edx_Eax =>
            return R_Ax;
         when R_Ebx_Ecx =>
            return R_Cx;
         when R_Esi_Edi =>
            return R_Di;
      end case;
   end Get_Pair_Low;

end Ortho_Code.X86;
