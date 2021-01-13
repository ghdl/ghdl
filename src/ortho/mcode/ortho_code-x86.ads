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

package Ortho_Code.X86 is
   --  Registers.
   R_Nil : constant O_Reg := 0;

   --  Not a value.  Used for statements.
   R_None : constant O_Reg := 1;

   --  Memory.
   R_Mem : constant O_Reg := 2;

   --  Spilled out.
   R_Spill : constant O_Reg := 3;

   --  Register or memory.
   --  THis can only be requested.
   R_Rm : constant O_Reg := 48;

   --  Immediat
   R_Imm : constant O_Reg := 49;

   --  Immediat, register or memory.
   --  This can be requested.
   R_Irm : constant O_Reg := 50;

   --  Immediat or register.
   --  This can be requested.
   R_Ir : constant O_Reg := 51;

   --  BASE + OFFSET
   R_B_Off : constant O_Reg := 52;

   --  BASE+INDEX*SCALE+OFFSET
   --  This can be requested.
   R_Sib : constant O_Reg := 53;

   --  INDEX*SCALE + OFFSET
   --  This can be requested.
   R_I_Off : constant O_Reg := 54;

   --  BASE + INDEX*SCALE
   R_B_I : constant O_Reg := 55;

   --  INDEX*SCALE
   R_I : constant O_Reg := 56;

   subtype Regs_Imm32 is O_Reg range R_Irm .. R_I_Off;

   R_Any8  : constant O_Reg := 5;
   R_Any32 : constant O_Reg := 6;
   R_Any64 : constant O_Reg := 7;
   R_Ax : constant O_Reg := 8;
   R_Cx : constant O_Reg := 9;
   R_Dx : constant O_Reg := 10;
   R_Bx : constant O_Reg := 11;
   R_Sp : constant O_Reg := 12;
   R_Bp : constant O_Reg := 13;
   R_Si : constant O_Reg := 14;
   R_Di : constant O_Reg := 15;
   R_R8 : constant O_Reg := 16;
   R_R9 : constant O_Reg := 17;
   R_R10 : constant O_Reg := 18;
   R_R11 : constant O_Reg := 19;
   R_R12 : constant O_Reg := 20;
   R_R13 : constant O_Reg := 21;
   R_R14 : constant O_Reg := 22;
   R_R15 : constant O_Reg := 23;

   subtype Regs_R8 is O_Reg range R_Ax .. R_Bx;
   subtype Regs_R32 is O_Reg range R_Ax .. R_Di;
   subtype Regs_R64 is O_Reg range R_Ax .. R_R15;
   subtype Regs_R8_R15 is O_Reg range R_R8 .. R_R15;

   R_St0 : constant O_Reg := 24;
   R_St1 : constant O_Reg := 25;
   R_St2 : constant O_Reg := 26;
   R_St3 : constant O_Reg := 27;
   R_St4 : constant O_Reg := 28;
   R_St5 : constant O_Reg := 29;
   R_St6 : constant O_Reg := 30;
   R_St7 : constant O_Reg := 31;
   --R_Any_Fp : constant O_Reg := 24;

   subtype Regs_Fp is O_Reg range R_St0 .. R_St7;

   --  Any condition register.
   R_Any_Cc : constant O_Reg := 32;
   R_Ov : constant O_Reg := 32;
   R_No : constant O_Reg := 33;
   R_Ult : constant O_Reg := 34;
   R_Uge : constant O_Reg := 35;
   R_Eq : constant O_Reg := 36;
   R_Ne : constant O_Reg := 37;
   R_Ule : constant O_Reg := 38;
   R_Ugt : constant O_Reg := 39;
   R_Slt : constant O_Reg := 44;
   R_Sge : constant O_Reg := 45;
   R_Sle : constant O_Reg := 46;
   R_Sgt : constant O_Reg := 47;

   subtype Regs_Cc is O_Reg range R_Ov .. R_Sgt;

   R_Edx_Eax : constant O_Reg := 64;
   R_Ebx_Ecx : constant O_Reg := 65;
   R_Esi_Edi : constant O_Reg := 66;
   R_AnyPair : constant O_Reg := 67;

   subtype Regs_Pair is O_Reg range R_Edx_Eax .. R_Esi_Edi;

   R_Any_Xmm : constant O_Reg := 79;

   R_Xmm0  : constant O_Reg := 80;
   R_Xmm1  : constant O_Reg := R_Xmm0 + 1;
   R_Xmm2  : constant O_Reg := R_Xmm0 + 2;
   R_Xmm3  : constant O_Reg := R_Xmm0 + 3;
   R_Xmm4  : constant O_Reg := R_Xmm0 + 4;
   R_Xmm5  : constant O_Reg := R_Xmm0 + 5;
   R_Xmm6  : constant O_Reg := R_Xmm0 + 6;
   R_Xmm7  : constant O_Reg := R_Xmm0 + 7;
   R_Xmm8  : constant O_Reg := R_Xmm0 + 8;
   R_Xmm9  : constant O_Reg := R_Xmm0 + 9;
   R_Xmm10 : constant O_Reg := R_Xmm0 + 10;
   R_Xmm11 : constant O_Reg := R_Xmm0 + 11;
   R_Xmm12 : constant O_Reg := R_Xmm0 + 12;
   R_Xmm13 : constant O_Reg := R_Xmm0 + 13;
   R_Xmm14 : constant O_Reg := R_Xmm0 + 14;
   R_Xmm15 : constant O_Reg := R_Xmm0 + 15;

   subtype Regs_X86_64_Xmm is O_Reg range R_Xmm0 .. R_Xmm15;
   subtype Regs_X86_Xmm is O_Reg range R_Xmm0 .. R_Xmm7;
   subtype Regs_Xmm is O_Reg range R_Xmm0 .. R_Xmm15;
   subtype Regs_Xmm8_Xmm15 is O_Reg range R_Xmm8 .. R_Xmm15;

   function Get_Pair_High (Reg : Regs_Pair) return Regs_R32;
   function Get_Pair_Low (Reg : Regs_Pair) return Regs_R32;

   function Inverse_Cc (R : O_Reg) return O_Reg;

   --  Intrinsic subprograms.
   Intrinsic_Mul_Ov_U64 : constant Int32 := 1;
   Intrinsic_Div_Ov_U64 : constant Int32 := 2;
   Intrinsic_Mod_Ov_U64 : constant Int32 := 3;
   Intrinsic_Mul_Ov_I64 : constant Int32 := 4;
   Intrinsic_Div_Ov_I64 : constant Int32 := 5;
   Intrinsic_Mod_Ov_I64 : constant Int32 := 6;
   Intrinsic_Rem_Ov_I64 : constant Int32 := 7;

   subtype Intrinsics_X86 is Int32
     range Intrinsic_Mul_Ov_U64 .. Intrinsic_Rem_Ov_I64;

   type O_Reg_Array is array (Natural range <>) of O_Reg;

   type O_Reg_Bitmap is array (O_Reg range <>) of Boolean;
   pragma Pack (O_Reg_Bitmap);

   subtype Regs_R32_Bitmap is O_Reg_Bitmap (Regs_R32);
   subtype Regs_R64_Bitmap is O_Reg_Bitmap (Regs_R64);
   subtype Regs_Xmm64_Bitmap is O_Reg_Bitmap (Regs_X86_64_Xmm);

   --  Registers preserved accross calls.
   Preserved_Regs_32 : constant Regs_R32_Bitmap :=
     (R_Di | R_Si | R_Bx => True, others => False);
   Preserved_Regs_Lin64 : constant Regs_R64_Bitmap :=
     (R_Bx | R_R12 | R_R13 | R_R14 | R_R15 => True, others => False);
   Preserved_Regs_Win64 : constant Regs_R64_Bitmap :=
     (R_Di | R_Si | R_Bx | R_R12 | R_R13 | R_R14 | R_R15 => True,
      others => False);
   Preserved_Xmm_Win64 : constant Regs_Xmm64_Bitmap :=
     (R_Xmm6 | R_Xmm7 | R_Xmm8 | R_Xmm9 | R_Xmm1 | R_Xmm11
        | R_Xmm12 | R_Xmm13 | R_Xmm14 | R_Xmm15 => True,
      others => False);
end Ortho_Code.X86;
