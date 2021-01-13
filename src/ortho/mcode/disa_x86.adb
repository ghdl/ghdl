--  X86 disassembler.
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
with System.Address_To_Access_Conversions;

package body Disa_X86 is
   type Byte is new Interfaces.Unsigned_8;
   type Bf_2 is mod 2 ** 2;
   type Bf_3 is mod 2 ** 3;
   type Byte_Vector is array (Natural) of Byte;
   package Bv_Addr2acc is new System.Address_To_Access_Conversions
     (Object => Byte_Vector);
   use Bv_Addr2acc;

   type Cstring_Acc is access constant String;
   type Index_Type is
     (
      N_None,
      N_Push,
      N_Pop,
      N_Ret,
      N_Mov,
      N_Add,
      N_Or,
      N_Adc,
      N_Sbb,
      N_And,
      N_Sub,
      N_Xor,
      N_Cmp,
      N_Into,
      N_Jmp,
      N_Jcc,
      N_Setcc,
      N_Call,
      N_Int,
      N_Cdq,
      N_Imul,
      N_Mul,
      N_Leave,
      N_Test,
      N_Lea,
      N_O,
      N_No,
      N_B,
      N_AE,
      N_E,
      N_Ne,
      N_Be,
      N_A,
      N_S,
      N_Ns,
      N_P,
      N_Np,
      N_L,
      N_Ge,
      N_Le,
      N_G,
      N_Not,
      N_Neg,
      N_Cbw,
      N_Div,
      N_Idiv,
      N_Movsx,
      N_Movzx,
      N_Nop,
      N_Hlt,
      N_Inc,
      N_Dec,
      N_Rol,
      N_Ror,
      N_Rcl,
      N_Rcr,
      N_Shl,
      N_Shr,
      N_Sar,
      N_Fadd,
      N_Fmul,
      N_Fcom,
      N_Fcomp,
      N_Fsub,
      N_Fsubr,
      N_Fdiv,
      N_Fdivr,

      G_1,
      G_2,
      G_3,
      G_5
     );

   type Names_Type is array (Index_Type range <>) of Cstring_Acc;
   subtype S is String;
   Names : constant Names_Type :=
     (N_None => new S'("none"),
      N_Push => new S'("push"),
      N_Pop => new S'("pop"),
      N_Ret => new S'("ret"),
      N_Mov => new S'("mov"),
      N_Add => new S'("add"),
      N_Or => new S'("or"),
      N_Adc => new S'("adc"),
      N_Sbb => new S'("sbb"),
      N_And => new S'("and"),
      N_Sub => new S'("sub"),
      N_Xor => new S'("xor"),
      N_Cmp => new S'("cmp"),
      N_Into => new S'("into"),
      N_Jmp => new S'("jmp"),
      N_Jcc => new S'("j"),
      N_Int => new S'("int"),
      N_Cdq => new S'("cdq"),
      N_Call => new S'("call"),
      N_Imul => new S'("imul"),
      N_Mul => new S'("mul"),
      N_Leave => new S'("leave"),
      N_Test => new S'("test"),
      N_Setcc => new S'("set"),
      N_Lea => new S'("lea"),
      N_O => new S'("o"),
      N_No => new S'("no"),
      N_B => new S'("b"),
      N_AE => new S'("ae"),
      N_E => new S'("e"),
      N_Ne => new S'("ne"),
      N_Be => new S'("be"),
      N_A => new S'("a"),
      N_S => new S'("s"),
      N_Ns => new S'("ns"),
      N_P => new S'("p"),
      N_Np => new S'("np"),
      N_L => new S'("l"),
      N_Ge => new S'("ge"),
      N_Le => new S'("le"),
      N_G => new S'("g"),
      N_Not => new S'("not"),
      N_Neg => new S'("neg"),
      N_Cbw => new S'("cbw"),
      N_Div => new S'("div"),
      N_Idiv => new S'("idiv"),
      N_Movsx => new S'("movsx"),
      N_Movzx => new S'("movzx"),
      N_Nop => new S'("nop"),
      N_Hlt => new S'("hlt"),
      N_Inc => new S'("inc"),
      N_Dec => new S'("dec"),
      N_Rol => new S'("rol"),
      N_Ror => new S'("ror"),
      N_Rcl => new S'("rcl"),
      N_Rcr => new S'("rcr"),
      N_Shl => new S'("shl"),
      N_Shr => new S'("shr"),
      N_Sar => new S'("sar"),
      N_Fadd => new S'("fadd"),
      N_Fmul => new S'("fmul"),
      N_Fcom => new S'("fcom"),
      N_Fcomp => new S'("fcomp"),
      N_Fsub => new S'("fsub"),
      N_Fsubr => new S'("fsubr"),
      N_Fdiv => new S'("fdiv"),
      N_Fdivr => new S'("fdivr")
     );



   --  Format of an instruction.
   --  MODRM_SRC_8 : modrm byte follow, and modrm is source, witdh = 8bits
   --  MODRM_DST_8 : modrm byte follow, and modrm is dest, width = 8 bits.
   --  MODRM_SRC_W : modrm byte follow, and modrm is source, width = 16/32 bits
   --  MODRM_DST_W : modrm byte follow, and modrm is dest, width =16/32 bits.
   --  MODRM_IMM_W : modrm byte follow, with an opcode in the reg field,
   --                followed by an immediat, width = 16/32 bits.
   --  MODRM_IMM_8 : modrm byte follow, with an opcode in the reg field,
   --               followed by an immediat, width = 8 bits.
   --  IMM : the opcode is followed by an immediate value.
   --  PREFIX : the opcode is a prefix (1 byte).
   --  OPCODE : inherent addressing.
   --  OPCODE2 : a second byte specify the instruction.
   --  REG_IMP : register is in the 3 LSB of the opcode.
   --  REG_IMM_W : register is in the 3 LSB of the opcode, followed by an
   --              immediat, width = 16/32 bits.
   --  DISP_W : a wide displacement (16/32 bits).
   --  DISP_8 : short displacement (8 bits).
   --  INVALID : bad opcode.
   type Format_Type is (Modrm_Src, Modrm_Dst,
                        Modrm_Imm, Modrm_Imm_S,
                        Modrm,
                        Modrm_Ax,
                        Modrm_Imm8,
                        Imm, Imm_S, Imm_8,
                        Eax_Imm,
                        Prefix, Opcode, Opcode2, Reg_Imp,
                        Reg_Imm,
                        Imp,
                        Disp_W, Disp_8,
                        Cond_Disp_W, Cond_Disp_8,
                        Cond_Modrm,
                        Ax_Off_Src, Ax_Off_Dst,
                        Invalid);

   type Width_Type is (W_None, W_8, W_16, W_32, W_Data);

   --  Description for one instruction.
   type Insn_Desc_Type is record
      --  Name of the operation.
      Name : Index_Type;

      --  Width of the instruction.
      --  This is used to add a suffix (b,w,l) to the instruction.
      --  This may also be the size of a data.
      Width : Width_Type;

      --  Format of the instruction.
      Format : Format_Type;
   end record;

   Desc_Invalid : constant Insn_Desc_Type := (N_None, W_None, Invalid);

   type Insn_Desc_Array_Type is array (Byte) of Insn_Desc_Type;
   type Group_Desc_Array_Type is array (Bf_3) of Insn_Desc_Type;
   Insn_Desc : constant Insn_Desc_Array_Type :=
     (
      2#00_000_000# => (N_Add, W_8, Modrm_Dst),
      2#00_000_001# => (N_Add, W_Data, Modrm_Dst),
      2#00_000_010# => (N_Add, W_8, Modrm_Src),
      2#00_000_011# => (N_Add, W_Data, Modrm_Src),

      2#00_001_000# => (N_Or, W_8, Modrm_Dst),
      2#00_001_001# => (N_Or, W_Data, Modrm_Dst),
      2#00_001_010# => (N_Or, W_8, Modrm_Src),
      2#00_001_011# => (N_Or, W_Data, Modrm_Src),

      2#00_011_000# => (N_Sbb, W_8, Modrm_Dst),
      2#00_011_001# => (N_Sbb, W_Data, Modrm_Dst),
      2#00_011_010# => (N_Sbb, W_8, Modrm_Src),
      2#00_011_011# => (N_Sbb, W_Data, Modrm_Src),

      2#00_100_000# => (N_And, W_8, Modrm_Dst),
      2#00_100_001# => (N_And, W_Data, Modrm_Dst),
      2#00_100_010# => (N_And, W_8, Modrm_Src),
      2#00_100_011# => (N_And, W_Data, Modrm_Src),

      2#00_101_000# => (N_Sub, W_8, Modrm_Dst),
      2#00_101_001# => (N_Sub, W_Data, Modrm_Dst),
      2#00_101_010# => (N_Sub, W_8, Modrm_Src),
      2#00_101_011# => (N_Sub, W_Data, Modrm_Src),

      2#00_110_000# => (N_Xor, W_8, Modrm_Dst),
      2#00_110_001# => (N_Xor, W_Data, Modrm_Dst),
      2#00_110_010# => (N_Xor, W_8, Modrm_Src),
      2#00_110_011# => (N_Xor, W_Data, Modrm_Src),

      2#00_111_000# => (N_Cmp, W_8, Modrm_Dst),
      2#00_111_001# => (N_Cmp, W_Data, Modrm_Dst),
      2#00_111_010# => (N_Cmp, W_8, Modrm_Src),
      2#00_111_011# => (N_Cmp, W_Data, Modrm_Src),

      2#00_111_100# => (N_Cmp, W_8, Eax_Imm),
      2#00_111_101# => (N_Cmp, W_Data, Eax_Imm),

      2#0101_0_000# => (N_Push, W_Data, Reg_Imp),
      2#0101_0_001# => (N_Push, W_Data, Reg_Imp),
      2#0101_0_010# => (N_Push, W_Data, Reg_Imp),
      2#0101_0_011# => (N_Push, W_Data, Reg_Imp),
      2#0101_0_100# => (N_Push, W_Data, Reg_Imp),
      2#0101_0_101# => (N_Push, W_Data, Reg_Imp),
      2#0101_0_110# => (N_Push, W_Data, Reg_Imp),
      2#0101_0_111# => (N_Push, W_Data, Reg_Imp),

      2#0101_1_000# => (N_Pop, W_Data, Reg_Imp),
      2#0101_1_001# => (N_Pop, W_Data, Reg_Imp),
      2#0101_1_010# => (N_Pop, W_Data, Reg_Imp),
      2#0101_1_011# => (N_Pop, W_Data, Reg_Imp),
      2#0101_1_100# => (N_Pop, W_Data, Reg_Imp),
      2#0101_1_101# => (N_Pop, W_Data, Reg_Imp),
      2#0101_1_110# => (N_Pop, W_Data, Reg_Imp),
      2#0101_1_111# => (N_Pop, W_Data, Reg_Imp),

      2#0110_1000# => (N_Push, W_Data, Imm),
      2#0110_1010# => (N_Push, W_Data, Imm_S),

      2#0111_0000# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_0001# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_0010# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_0011# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_0100# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_0101# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_0110# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_0111# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1000# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1001# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1010# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1011# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1100# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1101# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1110# => (N_Jcc, W_None, Cond_Disp_8),
      2#0111_1111# => (N_Jcc, W_None, Cond_Disp_8),

      2#1000_0000# => (G_1, W_8, Modrm_Imm),
      2#1000_0001# => (G_1, W_Data, Modrm_Imm),
      2#1000_0011# => (G_1, W_Data, Modrm_Imm_S),

      2#1000_0101# => (N_Test, W_Data, Modrm_Src),
      2#1000_1101# => (N_Lea, W_Data, Modrm_Src),

      2#1000_1010# => (N_Mov, W_8, Modrm_Src),
      2#1000_1011# => (N_Mov, W_Data, Modrm_Src),
      2#1000_1000# => (N_Mov, W_8, Modrm_Dst),
      2#1000_1001# => (N_Mov, W_Data, Modrm_Dst),

      2#1001_0000# => (N_Nop, W_None, Opcode),
      2#1001_1001# => (N_Cdq, W_Data, Imp),

      2#1010_0000# => (N_Mov, W_8, Ax_Off_Src),
      2#1010_0001# => (N_Mov, W_Data, Ax_Off_Src),
      2#1010_0010# => (N_Mov, W_8, Ax_Off_Dst),
      2#1010_0011# => (N_Mov, W_Data, Ax_Off_Dst),

      2#1011_0000# => (N_Mov, W_8, Reg_Imm),

      2#1011_1000# => (N_Mov, W_Data, Reg_Imm),
      2#1011_1001# => (N_Mov, W_Data, Reg_Imm),
      2#1011_1010# => (N_Mov, W_Data, Reg_Imm),
      2#1011_1011# => (N_Mov, W_Data, Reg_Imm),
      2#1011_1100# => (N_Mov, W_Data, Reg_Imm),
      2#1011_1101# => (N_Mov, W_Data, Reg_Imm),
      2#1011_1110# => (N_Mov, W_Data, Reg_Imm),
      2#1011_1111# => (N_Mov, W_Data, Reg_Imm),

      2#1100_0000# => (G_2, W_8, Modrm_Imm8),
      2#1100_0001# => (G_2, W_Data, Modrm_Imm8),

      2#1100_0011# => (N_Ret, W_None, Opcode),
      2#1100_0110# => (N_Mov, W_8, Modrm_Imm),
      2#1100_0111# => (N_Mov, W_Data, Modrm_Imm),
      2#1100_1001# => (N_Leave, W_None, Opcode),
      2#1100_1101# => (N_Int, W_None, Imm_8),
      2#1100_1110# => (N_Into, W_None, Opcode),

      2#1110_1000# => (N_Call, W_None, Disp_W),
      2#1110_1001# => (N_Jmp, W_None, Disp_W),
      2#1110_1011# => (N_Jmp, W_None, Disp_8),

      2#1111_0100# => (N_Hlt, W_None, Opcode),

      2#1111_0110# => (G_3, W_None, Invalid),
      2#1111_0111# => (G_3, W_None, Invalid),

      2#1111_1111# => (G_5, W_None, Invalid),
      --2#1111_1111# => (N_Push, W_Data, Modrm),
      others => (N_None, W_None, Invalid));

   Insn_Desc_0F : constant Insn_Desc_Array_Type :=
     (2#1000_0000# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_0001# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_0010# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_0011# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_0100# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_0101# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_0110# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_0111# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1000# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1001# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1010# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1011# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1100# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1101# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1110# => (N_Jcc, W_None, Cond_Disp_W),
      2#1000_1111# => (N_Jcc, W_None, Cond_Disp_W),

      2#1001_0000# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_0001# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_0010# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_0011# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_0100# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_0101# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_0110# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_0111# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1000# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1001# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1010# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1011# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1100# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1101# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1110# => (N_Setcc, W_8, Cond_Modrm),
      2#1001_1111# => (N_Setcc, W_8, Cond_Modrm),

      2#1011_0110# => (N_Movzx, W_Data, Modrm_Dst),
      2#1011_1110# => (N_Movsx, W_Data, Modrm_Dst),
      others => (N_None, W_None, Invalid));

   --  16#F7#
   Insn_Desc_G3 : constant Group_Desc_Array_Type :=
     (2#000# => (N_Test, W_Data, Reg_Imm),
      2#010# => (N_Not, W_Data, Modrm_Dst),
      2#011# => (N_Neg, W_Data, Modrm_Dst),
      2#100# => (N_Mul, W_Data, Modrm_Ax),
      2#101# => (N_Imul, W_Data, Modrm_Ax),
      2#110# => (N_Div, W_Data, Modrm_Ax),
      2#111# => (N_Idiv, W_Data, Modrm_Ax),
      others => (N_None, W_None, Invalid));

   Insn_Desc_G5 : constant Group_Desc_Array_Type :=
     (2#000# => (N_Inc, W_Data, Modrm),
      2#001# => (N_Dec, W_Data, Modrm),
      2#010# => (N_Call, W_Data, Modrm),
      --2#011# => (N_Call, W_Data, Modrm_Ax),
      2#100# => (N_Jmp, W_Data, Modrm),
      --2#101# => (N_Jmp, W_Data, Modrm_Ax),
      2#110# => (N_Push, W_Data, Modrm_Ax),
      others => (N_None, W_None, Invalid));

   type Group_Name_Array_Type is array (Index_Type range G_1 .. G_2, Bf_3)
     of Index_Type;
   Group_Name : constant Group_Name_Array_Type :=
     (
      G_1 => (N_Add, N_Or, N_Adc, N_Sbb, N_And, N_Sub, N_Xor, N_Cmp),
      G_2 => (N_Rol, N_Ror, N_Rcl, N_Rcr, N_Shl, N_Shr, N_None, N_Sar)
     );

   --  Standard widths of operations.
   type Width_Array_Type is array (Width_Type) of Character;
   Width_Char : constant Width_Array_Type :=
     (W_None => '-', W_8 => 'b', W_16 => 'w', W_32 => 'l', W_Data => '?');
   type Width_Len_Type is array (Width_Type) of Natural;
   Width_Len : constant Width_Len_Type :=
     (W_None => 0, W_8 => 1, W_16 => 2, W_32 => 4, W_Data => 0);

   --  Registers.
--    type Reg_Type is (Reg_Ax, Reg_Bx, Reg_Cx, Reg_Dx,
--                      Reg_Bp, Reg_Sp, Reg_Si, Reg_Di,
--                      Reg_Al, Reg_Ah, Reg_Bl, Reg_Bh,
--                      Reg_Cl, Reg_Ch, Reg_Dl, Reg_Dh);

   --  Bits extraction from byte functions.
   --  For a byte, MSB (most significant bit) is bit 7 while
   --  LSB (least significant bit) is bit 0.

   --  Extract bits 2, 1 and 0.
   function Ext_210 (B : Byte) return Bf_3;
   pragma Inline (Ext_210);

   --  Extract bits 5-3 of byte B.
   function Ext_543 (B : Byte) return Bf_3;
   pragma Inline (Ext_543);

   --  Extract bits 7-6 of byte B.
   function Ext_76 (B : Byte) return Bf_2;
   pragma Inline (Ext_76);

   function Ext_210 (B : Byte) return Bf_3 is
   begin
      return Bf_3 (B and 2#111#);
   end Ext_210;

   function Ext_543 (B : Byte) return Bf_3 is
   begin
      return Bf_3 (Shift_Right (B, 3) and 2#111#);
   end Ext_543;

   function Ext_76 (B : Byte) return Bf_2 is
   begin
      return Bf_2 (Shift_Right (B, 6) and 2#11#);
   end Ext_76;

   function Ext_Modrm_Mod (B : Byte) return Bf_2 renames Ext_76;
   function Ext_Modrm_Rm (B : Byte) return Bf_3 renames Ext_210;
   function Ext_Modrm_Reg (B : Byte) return Bf_3 renames Ext_543;
   function Ext_Sib_Base (B : Byte) return Bf_3 renames Ext_210;
   function Ext_Sib_Index (B : Byte) return Bf_3 renames Ext_543;
   function Ext_Sib_Scale (B : Byte) return Bf_2 renames Ext_76;

   procedure Disassemble_Insn (Addr : System.Address;
                               Pc : Unsigned_32;
                               Line : in out String;
                               Line_Len : out Natural;
                               Insn_Len : out Natural;
                               Proc_Cb : Symbol_Proc_Type)
   is
      --  Index in LINE of the next character to be written.
      Lo : Natural;

      --  Default width.
      W_Default : constant Width_Type := W_32;

      --  The instruction memory, 0 based.
      Mem : Bv_Addr2acc.Object_Pointer;

      --  Add NAME to the line.
      procedure Add_Name (Name : Index_Type);
      pragma Inline (Add_Name);

      --  Add CHAR to the line.
      procedure Add_Char (C : Character);
      pragma Inline (Add_Char);

      --  Add STR to the line.
      procedure Add_String (Str : String) is
      begin
         Line (Lo .. Lo + Str'Length - 1) := Str;
         Lo := Lo + Str'Length;
      end Add_String;

      --  Add BYTE to the line.
      procedure Add_Byte (V : Byte) is
         type My_Str is array (Natural range 0 .. 15) of Character;
         Hex_Digit : constant My_Str := "0123456789abcdef";
      begin
         Add_Char (Hex_Digit (Natural (Shift_Right (V, 4) and 16#0f#)));
         Add_Char (Hex_Digit (Natural (Shift_Right (V, 0) and 16#0f#)));
      end Add_Byte;

      procedure Add_Name (Name : Index_Type) is
      begin
         Add_String (Names (Name).all);
      end Add_Name;

      procedure Add_Char (C : Character) is
      begin
         Line (Lo) := C;
         Lo := Lo + 1;
      end Add_Char;

      procedure Add_Comma is
      begin
         Add_String (", ");
      end Add_Comma;

      procedure Name_Align (Orig : Natural) is
      begin
         Add_Char (' ');
         while Lo - Orig < 8 loop
            Add_Char (' ');
         end loop;
      end Name_Align;

      procedure Add_Opcode (Name : Index_Type; Width : Width_Type)
      is
         L : constant Natural := Lo;
      begin
         Add_Name (Name);
         if False and Width /= W_None then
            Add_Char (Width_Char (Width));
         end if;
         Name_Align (L);
      end Add_Opcode;

      procedure Add_Cond_Opcode (Name : Index_Type; B : Byte)
      is
         L : constant Natural := Lo;
      begin
         Add_Name (Name);
         Add_Name (Index_Type'Val (Index_Type'Pos (N_O)
                                     + Byte'Pos (B and 16#0f#)));
         Name_Align (L);
      end Add_Cond_Opcode;

      procedure Decode_Reg_Field (F : Bf_3; W : Width_Type) is
         type Reg_Name2_Array is array (Bf_3) of String (1 .. 2);
         type Reg_Name3_Array is array (Bf_3) of String (1 .. 3);
         Regs_8 : constant Reg_Name2_Array :=
           ("al", "cl", "dl", "bl", "ah", "ch", "dh", "bh");
         Regs_16 : constant Reg_Name2_Array :=
           ("ax", "cx", "dx", "bx", "sp", "bp", "si", "di");
         Regs_32 : constant Reg_Name3_Array :=
           ("eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi");
      begin
         Add_Char ('%');
         case W is
            when W_8 =>
               Add_String (Regs_8 (F));
            when W_16 =>
               Add_String (Regs_16 (F));
            when W_32 =>
               Add_String (Regs_32 (F));
            when W_None
              | W_Data =>
               raise Program_Error;
         end case;
      end Decode_Reg_Field;

      procedure Decode_Val (Off : Natural; Width : Width_Type)
      is
      begin
         case Width is
            when W_8 =>
               Add_Byte (Mem (Off));
            when W_16 =>
               Add_Byte (Mem (Off + 1));
               Add_Byte (Mem (Off));
            when W_32 =>
               Add_Byte (Mem (Off + 3));
               Add_Byte (Mem (Off + 2));
               Add_Byte (Mem (Off + 1));
               Add_Byte (Mem (Off + 0));
            when W_None
              | W_Data =>
               raise Program_Error;
         end case;
      end Decode_Val;

      function Decode_Val (Off : Natural; Width : Width_Type)
                          return Unsigned_32
      is
         V : Unsigned_32;
      begin
         case Width is
            when W_8 =>
               V := Unsigned_32 (Mem (Off));
               --  Sign extension.
               if V >= 16#80# then
                  V := 16#Ffff_Ff00# or V;
               end if;
               return V;
            when W_16 =>
               return Shift_Left (Unsigned_32 (Mem (Off + 1)), 8)
                 or Unsigned_32 (Mem (Off));
            when W_32 =>
               return  Shift_Left (Unsigned_32 (Mem (Off + 3)), 24)
                 or Shift_Left (Unsigned_32 (Mem (Off + 2)), 16)
                 or Shift_Left (Unsigned_32 (Mem (Off + 1)), 8)
                 or Shift_Left (Unsigned_32 (Mem (Off + 0)), 0);
            when W_None
              | W_Data =>
               raise Program_Error;
         end case;
      end Decode_Val;

      procedure Decode_Imm (Off : in out Natural; Width : Width_Type)
      is
      begin
         Add_String ("$0x");
         Decode_Val (Off, Width);
         Off := Off + Width_Len (Width);
      end Decode_Imm;

      procedure Decode_Disp (Off : in out Natural;
                             Width : Width_Type;
                             Offset : Unsigned_32 := 0)
      is
         L : Natural;
         V : Unsigned_32;
         Off_Orig : constant Natural := Off;
      begin
         L := Lo;
         V := Decode_Val (Off, Width) + Offset;
         Off := Off + Width_Len (Width);
         if Proc_Cb /= null then
            Proc_Cb.all (Mem (Off)'Address,
                         Line (Lo .. Line'Last), Lo);
         end if;
         if L /= Lo then
            if V = 0 then
               return;
            end if;
            Add_String (" + ");
         end if;
         Add_String ("0x");
         if Offset = 0 then
            Decode_Val (Off_Orig, Width);
         else
            Add_Byte (Byte (Shift_Right (V, 24) and 16#Ff#));
            Add_Byte (Byte (Shift_Right (V, 16) and 16#Ff#));
            Add_Byte (Byte (Shift_Right (V, 8) and 16#Ff#));
            Add_Byte (Byte (Shift_Right (V, 0) and 16#Ff#));
         end if;
      end Decode_Disp;

      procedure Decode_Modrm_Reg (B : Byte; Width : Width_Type) is
      begin
         Decode_Reg_Field (Ext_Modrm_Reg (B), Width);
      end Decode_Modrm_Reg;

      procedure Decode_Sib (Sib : Byte; B_Mod : Bf_2)
      is
         S : Bf_2;
         I : Bf_3;
         B : Bf_3;
      begin
         S := Ext_Sib_Scale (Sib);
         B := Ext_Sib_Base (Sib);
         I := Ext_Sib_Index (Sib);
         Add_Char ('(');
         if B = 2#101# and then B_Mod /= 0 then
            Decode_Reg_Field (B, W_32);
            Add_Char (',');
         end if;
         if I /= 2#100# then
            Decode_Reg_Field (I, W_32);
            case S is
               when 2#00# =>
                  null;
               when 2#01# =>
                  Add_String (",2");
               when 2#10# =>
                  Add_String (",4");
               when 2#11# =>
                  Add_String (",8");
            end case;
         end if;
         Add_Char (')');
      end Decode_Sib;

      procedure Decode_Modrm_Mem (Off : in out Natural; Width : Width_Type)
      is
         B : Byte;
         B_Mod : Bf_2;
         B_Rm : Bf_3;
         Off_Orig : Natural;
      begin
         B := Mem (Off);
         B_Mod := Ext_Modrm_Mod (B);
         B_Rm := Ext_Modrm_Rm (B);
         Off_Orig := Off;
         case B_Mod is
            when 2#11# =>
               Decode_Reg_Field (B_Rm, Width);
               Off := Off + 1;
            when 2#10# =>
               if B_Rm = 2#100# then
                  Off := Off + 2;
                  Decode_Disp (Off, W_32);
                  Decode_Sib (Mem (Off_Orig + 1), B_Mod);
               else
                  Off := Off + 1;
                  Decode_Disp (Off, W_32);
                  Add_Char ('(');
                  Decode_Reg_Field (B_Rm, W_32);
                  Add_Char (')');
               end if;
            when 2#01# =>
               if B_Rm = 2#100# then
                  Off := Off + 2;
                  Decode_Disp (Off, W_8);
                  Decode_Sib (Mem (Off_Orig + 1), B_Mod);
               else
                  Off := Off + 1;
                  Decode_Disp (Off, W_8);
                  Add_Char ('(');
                  Decode_Reg_Field (B_Rm, W_32);
                  Add_Char (')');
               end if;
            when 2#00# =>
               if B_Rm = 2#100# then
                  Off := Off + 2;
                  Decode_Sib (Mem (Off_Orig + 1), B_Mod);
               elsif B_Rm = 2#101# then
                  Off := Off + 1;
                  Decode_Disp (Off, W_32);
               else
                  Add_Char ('(');
                  Decode_Reg_Field (B_Rm, W_32);
                  Add_Char (')');
                  Off := Off + 1;
               end if;
         end case;
      end Decode_Modrm_Mem;

      --  Return the length of the modrm bytes.
      --  At least 1 (mod/rm), at most 6 (mod/rm + SUB + disp32).
      function Decode_Modrm_Len (Off : Natural) return Natural
      is
         B : Byte;
         M_Mod : Bf_2;
         M_Rm : Bf_3;
      begin
         B := Mem (Off);
         M_Mod := Ext_Modrm_Mod (B);
         M_Rm := Ext_Modrm_Rm (B);
         case M_Mod is
            when 2#11# =>
               return 1;
            when 2#10# =>
               if M_Rm = 2#100# then
                  return 1 + 1 + 4;
               else
                  return 1 + 4;
               end if;
            when 2#01# =>
               if M_Rm = 2#100# then
                  return 1 + 1 + 1;
               else
                  return 1 + 1;
               end if;
            when 2#00# =>
               if M_Rm = 2#101# then
                  --  disp32.
                  return 1 + 4;
               elsif M_Rm = 2#100# then
                  --  SIB
                  return 1 + 1;
               else
                  return 1;
               end if;
         end case;
      end Decode_Modrm_Len;


      Off : Natural;
      B : Byte;
      B1 : Byte;
      Desc : Insn_Desc_Type;
      Name : Index_Type;
      W : Width_Type;
   begin
      Mem := To_Pointer (Addr);
      Off := 0;
      Lo := Line'First;

      B := Mem (0);
      if B = 2#0000_1111# then
         B := Mem (1);
         Off := 2;
         Insn_Len := 2;
         Desc := Insn_Desc_0F (B);
      else
         Off := 1;
         Insn_Len := 1;
         Desc := Insn_Desc (B);
      end if;

      if Desc.Name >= G_1 then
         B1 := Mem (Off);
         case Desc.Name is
            when G_1
              | G_2 =>
               Name := Group_Name (Desc.Name, Ext_543 (B1));
            when G_3 =>
               Desc := Insn_Desc_G3 (Ext_543 (B1));
               Name := Desc.Name;
            when G_5 =>
               Desc := Insn_Desc_G5 (Ext_543 (B1));
               Name := Desc.Name;
            when others =>
               Desc := Desc_Invalid;
         end case;
      else
         Name := Desc.Name;
      end if;

      case Desc.Width is
         when W_Data =>
            W := W_Default;
         when W_8
           | W_16
           | W_32 =>
            W := Desc.Width;
         when W_None =>
            case Desc.Format is
               when Disp_8
                 | Cond_Disp_8
                 | Imm_8 =>
                  W := W_8;
               when Disp_W
                 | Cond_Disp_W =>
                  W := W_Default;
               when Invalid
                 | Opcode =>
                  W := W_None;
               when others =>
                  raise Program_Error;
            end case;
      end case;

      case Desc.Format is
         when Reg_Imp =>
            Add_Opcode (Desc.Name, W_Default);
            Decode_Reg_Field (Ext_210 (B), W_Default);
         when Opcode =>
            Add_Opcode (Desc.Name, W_None);
         when Modrm =>
            Add_Opcode (Desc.Name, W);
            Decode_Modrm_Mem (Insn_Len, W);
         when Modrm_Src =>
            Add_Opcode (Desc.Name, W);
            --  Disp source first.
            Decode_Modrm_Mem (Insn_Len, W);
            Add_Comma;
            B := Mem (Off);
            Decode_Modrm_Reg (Mem (Off), W);
         when Modrm_Dst =>
            Add_Opcode (Desc.Name, W);
            --  Disp source first.
            B := Mem (Off);
            Decode_Modrm_Reg (B, W);
            Add_Comma;
            Decode_Modrm_Mem (Insn_Len, W);
         when Modrm_Imm =>
            Add_Opcode (Name, W);
            Insn_Len := Off + Decode_Modrm_Len (Off);
            Decode_Imm (Insn_Len, W);
            Add_Comma;
            Decode_Modrm_Mem (Off, W);
         when Modrm_Imm_S =>
            Add_Opcode (Name, W);
            Insn_Len := Off + Decode_Modrm_Len (Off);
            Decode_Imm (Insn_Len, W_8);
            Add_Comma;
            Decode_Modrm_Mem (Off, W);
         when Modrm_Imm8 =>
            Add_Opcode (Name, W);
            Decode_Modrm_Mem (Off, W);
            Add_Comma;
            Decode_Imm (Off, W_8);

         when Reg_Imm =>
            Add_Opcode (Desc.Name, W);
            Decode_Imm (Insn_Len, W);
            Add_Comma;
            Decode_Reg_Field (Ext_210 (B), W);
         when Eax_Imm =>
            Add_Opcode (Desc.Name, W);
            Decode_Imm (Insn_Len, W);
            Add_Comma;
            Decode_Reg_Field (2#000#, W);

         when Disp_W
           | Disp_8 =>
            Add_Opcode (Desc.Name, W_None);
            Decode_Disp (Insn_Len, W,
                         Pc + Unsigned_32 (Insn_Len + Width_Len (W)));

         when Cond_Disp_8
           | Cond_Disp_W =>
            Add_Cond_Opcode (Desc.Name, B);
            Decode_Disp (Insn_Len, W,
                         Pc + Unsigned_32 (Insn_Len + Width_Len (W)));

         when Cond_Modrm =>
            Add_Cond_Opcode (Desc.Name, B);
            Decode_Modrm_Mem (Insn_Len, W);

         when Imm =>
            Add_Opcode (Desc.Name, W);
            Decode_Imm (Insn_Len, W);

         when Imm_S
           | Imm_8 =>
            Add_Opcode (Desc.Name, W);
            Decode_Imm (Insn_Len, W_8);

         when Modrm_Ax =>
            if (B and 2#1#) = 2#0# then
               W := W_8;
            else
               W := W_Default;
            end if;
            Add_Opcode (Desc.Name, W);
            Decode_Reg_Field (0, W);
            Add_Comma;
            Decode_Modrm_Mem (Off, W);

         when Ax_Off_Src =>
            Add_Opcode (Desc.Name, W);
            Decode_Disp (Insn_Len, W);
            Add_Comma;
            Decode_Reg_Field (0, W);

         when Ax_Off_Dst =>
            Add_Opcode (Desc.Name, W);
            Decode_Reg_Field (0, W);
            Add_Comma;
            Decode_Disp (Insn_Len, W);

         when Imp =>
            Add_Opcode (Desc.Name, W_Default);

         when Invalid
           | Prefix
           | Opcode2 =>
            Add_String ("invalid ");
            if Insn_Len = 2 then
               Add_Byte (Mem (0));
            end if;
            Add_Byte (B);
            Insn_Len := 1;
      end case;

      Line_Len := Lo - Line'First;
   end Disassemble_Insn;
end Disa_X86;


