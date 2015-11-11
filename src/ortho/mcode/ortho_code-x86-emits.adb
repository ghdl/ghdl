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
with Ortho_Code.Abi;
with Ortho_Code.Decls;
with Ortho_Code.Types;
with Ortho_Code.Consts;
with Ortho_Code.Debug;
with Ortho_Code.X86.Insns;
with Ortho_Code.X86.Flags;
with Ortho_Code.Flags;
with Ortho_Code.Dwarf;
with Ortho_Code.Binary; use Ortho_Code.Binary;
with Ortho_Ident;
with Ada.Text_IO;
with Interfaces; use Interfaces;

package body Ortho_Code.X86.Emits is
   type Insn_Size is (Sz_8, Sz_16, Sz_32l, Sz_32h);

   --  Well known sections.
   Sect_Text : Binary_File.Section_Acc;
   Sect_Rodata : Binary_File.Section_Acc;
   Sect_Bss : Binary_File.Section_Acc;

   --  For 64 bit to 32 bit conversion, we need an extra register.  Just before
   --  the conversion, there is an OE_Reg instruction containing the extra
   --  register.  Its value is saved here.
   Reg_Helper : O_Reg;

   Subprg_Pc : Pc_Type;

   --  x86 opcodes.
   Opc_Data16 : constant := 16#66#;
   Opc_Into   : constant := 16#ce#;
   Opc_Cdq    : constant := 16#99#;
   Opc_Int    : constant := 16#cd#;
   Opc_Addl_Reg_Rm  : constant := 16#03#;
   Opc_Xorl_Rm_Reg  : constant := 16#31#;
   Opc_Subl_Reg_Rm  : constant := 16#2b#;  --  Reg <- Reg - Rm
   Opc_Cmpl_Rm_Reg  : constant := 16#39#;
   Opc_Leal_Reg_Rm  : constant := 16#8d#;
   Opc_Movb_Imm_Reg : constant := 16#b0#;
   Opc_Movl_Imm_Reg : constant := 16#b8#;
   Opc_Imul_Reg_Rm_Imm32 : constant := 16#69#;
   Opc_Imul_Reg_Rm_Imm8  : constant := 16#6b#;
   Opc_Mov_Rm_Imm : constant := 16#c6#;  -- Eb,Ib  or Ev,Iz (grp11, opc2=0)
   Opc_Mov_Rm_Reg : constant := 16#88#;  -- Store: Eb,Gb  or  Ev,Gv
   Opc_Mov_Reg_Rm : constant := 16#8a#;  -- Load:  Gb,Eb  or  Gv,Ev
   Opc_Movl_Reg_Rm : constant := 16#8b#;  -- Load: Gv,Ev
   --  Opc_Grp1_Rm_Imm : constant := 16#80#;
   Opc_Grp1b_Rm_Imm8  : constant := 16#80#;
   Opc_Grp1v_Rm_Imm32 : constant := 16#81#;
   --  Opc_Grp1b_Rm_Imm8  : constant := 16#82#; -- Should not be used.
   Opc_Grp1v_Rm_Imm8  : constant := 16#83#;
   Opc2_Grp1_Add   : constant := 2#000_000#; --  Second byte
   Opc2_Grp1_Or    : constant := 2#001_000#; --  Second byte
   Opc2_Grp1_Adc   : constant := 2#010_000#; --  Second byte
   Opc2_Grp1_Sbb   : constant := 2#011_000#; --  Second byte
   Opc2_Grp1_And   : constant := 2#100_000#; --  Second byte
   Opc2_Grp1_Sub   : constant := 2#101_000#; --  Second byte
   Opc2_Grp1_Xor   : constant := 2#110_000#; --  Second byte
   Opc2_Grp1_Cmp   : constant := 2#111_000#; --  Second byte
   Opc_Grp3_Width  : constant := 16#f6#;
   Opc2_Grp3_Not   : constant := 2#010_000#;
   Opc2_Grp3_Neg   : constant := 2#011_000#;
   Opc2_Grp3_Mul   : constant := 2#100_000#;
   Opc2_Grp3_Imul  : constant := 2#101_000#;
   Opc2_Grp3_Div   : constant := 2#110_000#;
   Opc2_Grp3_Idiv  : constant := 2#111_000#;
   Opc_Test_Rm_Reg : constant := 16#84#;  --  Eb,Gb  or  Ev,Gv
   Opc_Push_Imm8   : constant := 16#6a#;
   Opc_Push_Imm    : constant := 16#68#;
   Opc_Push_Reg    : constant := 16#50#; --  opc[2:0] is reg.
   Opc_Pop_Reg     : constant := 16#58#; --  opc[2:0] is reg.
   Opc_Grp5        : constant := 16#ff#;
   Opc2_Grp5_Push_Rm : constant := 2#110_000#;
   --  Opc_Grp1a       : constant := 16#8f#;
   --  Opc2_Grp1a_Pop_Rm : constant := 2#000_000#;
   Opc_Jcc         : constant := 16#70#;
   Opc_0f          : constant := 16#0f#;
   Opc2_0f_Jcc     : constant := 16#80#;
   Opc2_0f_Setcc   : constant := 16#90#;
   Opc2_0f_Movzx   : constant := 16#b6#;
   Opc2_0f_Imul    : constant := 16#af#;
   Opc2_0f_Andnp   : constant := 16#55#;
   Opc2_0f_Xorp    : constant := 16#57#;
   Opc_Call        : constant := 16#e8#;
   Opc_Jmp_Long    : constant := 16#e9#;
   Opc_Jmp_Short   : constant := 16#eb#;
   Opc_Ret         : constant := 16#c3#;
   Opc_Leave       : constant := 16#c9#;

   procedure Error_Emit (Msg : String; Insn : O_Enode)
   is
      use Ada.Text_IO;
   begin
      Put ("error_emit: ");
      Put (Msg);
      Put (", insn=");
      Put (O_Enode'Image (Insn));
      Put (" (");
      Put (OE_Kind'Image (Get_Expr_Kind (Insn)));
      Put (")");
      New_Line;
      raise Program_Error;
   end Error_Emit;

   --  For many opcodes, the size of the operand is coded in bit 0, and the
   --  prefix data16 can be used for 16-bit operation.
   --  Deal with size.
   procedure Gen_Insn_Sz (B : Byte; Sz : Insn_Size) is
   begin
      case Sz is
         when Sz_8 =>
            Gen_B8 (B);
         when Sz_16 =>
            Gen_B8 (Opc_Data16);
            Gen_B8 (B + 1);
         when Sz_32l
           | Sz_32h =>
            Gen_B8 (B + 1);
      end case;
   end Gen_Insn_Sz;

   procedure Gen_Insn_Sz_S8 (B : Byte; Sz : Insn_Size) is
   begin
      case Sz is
         when Sz_8 =>
            Gen_B8 (B);
         when Sz_16 =>
            Gen_B8 (Opc_Data16);
            Gen_B8 (B + 3);
         when Sz_32l
           | Sz_32h =>
            Gen_B8 (B + 3);
      end case;
   end Gen_Insn_Sz_S8;

   function Get_Const_Val (C : O_Enode; Sz : Insn_Size) return Uns32 is
   begin
      case Sz is
         when Sz_8
           | Sz_16
           | Sz_32l =>
            return Get_Expr_Low (C);
         when Sz_32h =>
            return Get_Expr_High (C);
      end case;
   end Get_Const_Val;

   function Is_Imm8 (N : O_Enode; Sz : Insn_Size) return Boolean is
   begin
      if Get_Expr_Kind (N) /= OE_Const then
         return False;
      end if;
      return Get_Const_Val (N, Sz) <= 127;
   end Is_Imm8;

   procedure Gen_Imm8 (N : O_Enode; Sz : Insn_Size) is
   begin
      Gen_B8 (Byte (Get_Const_Val (N, Sz)));
   end Gen_Imm8;

--     procedure Gen_Imm32 (N : O_Enode; Sz : Insn_Size)
--     is
--        use Interfaces;
--     begin
--        case Get_Expr_Kind (N) is
--           when OE_Const =>
--              Gen_Le32 (Unsigned_32 (Get_Const_Val (N, Sz)));
--           when OE_Addrg =>
--              Gen_X86_32 (Get_Decl_Symbol (Get_Addr_Object (N)), 0);
--           when others =>
--              raise Program_Error;
--        end case;
--     end Gen_Imm32;

   --  Generate an immediat constant.
   procedure Gen_Imm (N : O_Enode; Sz : Insn_Size) is
   begin
      case Get_Expr_Kind (N) is
         when OE_Const =>
            case Sz is
               when Sz_8 =>
                  Gen_B8 (Byte (Get_Expr_Low (N) and 16#FF#));
               when Sz_16 =>
                  Gen_Le16 (Unsigned_32 (Get_Expr_Low (N) and 16#FF_FF#));
               when Sz_32l =>
                  Gen_Le32 (Unsigned_32 (Get_Expr_Low (N)));
               when Sz_32h =>
                  Gen_Le32 (Unsigned_32 (Get_Expr_High (N)));
            end case;
         when OE_Add
           | OE_Addrg =>
            --  Only for 32-bit immediat.
            pragma Assert (Sz = Sz_32l);
            declare
               P : O_Enode;
               L, R : O_Enode;
               S, C : O_Enode;
               Off : Int32;
            begin
               Off := 0;
               P := N;
               while Get_Expr_Kind (P) = OE_Add loop
                  L := Get_Expr_Left (P);
                  R := Get_Expr_Right (P);

                  --  Extract the const node.
                  if Get_Expr_Kind (R) = OE_Const then
                     S := L;
                     C := R;
                  elsif Get_Expr_Kind (L) = OE_Const then
                     S := R;
                     C := L;
                  else
                     raise Program_Error;
                  end if;
                  pragma Assert (Get_Expr_Mode (C) = Mode_U32);
                  Off := Off + To_Int32 (Get_Expr_Low (C));
                  P := S;
               end loop;
               pragma Assert (Get_Expr_Kind (P) = OE_Addrg);
               Gen_X86_32 (Get_Decl_Symbol (Get_Addr_Object (P)),
                           Integer_32 (Off));
            end;
         when others =>
            raise Program_Error;
      end case;
   end Gen_Imm;

   --  SIB + disp values.
   SIB_Scale : Byte;
   SIB_Index : O_Reg;
   Rm_Base : O_Reg;
   Rm_Offset : Int32;
   Rm_Sym : Symbol;

   --  If not R_Nil, encode mod=11 (no memory access).  All above variables
   --  must be 0/R_Nil.
   Rm_Reg : O_Reg;
   Rm_Sz : Insn_Size;

   procedure Fill_Sib (N : O_Enode)
   is
      use Ortho_Code.Decls;
      Reg : constant O_Reg := Get_Expr_Reg (N);
   begin
      --  A simple register.
      if Reg in Regs_R32 then
         if Rm_Base = R_Nil then
            Rm_Base := Reg;
         elsif SIB_Index = R_Nil then
            SIB_Index := Reg;
         else
            --  It is not possible to add 3 registers with SIB.
            raise Program_Error;
         end if;
         return;
      end if;

      case Get_Expr_Kind (N) is
         when OE_Indir =>
            Fill_Sib (Get_Expr_Operand (N));
         when OE_Addrl =>
            declare
               Frame : constant O_Enode := Get_Addrl_Frame (N);
            begin
               if Frame = O_Enode_Null then
                  --  Local frame: use the frame pointer.
                  Rm_Base := R_Bp;
               else
                  --  In an outer frame: use the computed frame register.
                  Rm_Base := Get_Expr_Reg (Frame);
               end if;
            end;
            Rm_Offset := Rm_Offset + Get_Local_Offset (Get_Addr_Object (N));
         when OE_Addrg =>
            --  Cannot add two symbols.
            pragma Assert (Rm_Sym = Null_Symbol);
            Rm_Sym := Get_Decl_Symbol (Get_Addr_Object (N));
         when OE_Add =>
            Fill_Sib (Get_Expr_Left (N));
            Fill_Sib (Get_Expr_Right (N));
         when OE_Const =>
            Rm_Offset := Rm_Offset + To_Int32 (Get_Expr_Low (N));
         when OE_Shl =>
            --  Only one scale.
            pragma Assert (SIB_Index = R_Nil);
            SIB_Index := Get_Expr_Reg (Get_Expr_Left (N));
            SIB_Scale := Byte (Get_Expr_Low (Get_Expr_Right (N)));
         when others =>
            Error_Emit ("fill_sib", N);
      end case;
   end Fill_Sib;

   function To_Reg32 (R : O_Reg) return Byte is
   begin
      pragma Assert (R in Regs_R32);
      return O_Reg'Pos (R) - O_Reg'Pos (R_Ax);
   end To_Reg32;
   pragma Inline (To_Reg32);

   function To_Reg_Xmm (R : O_Reg) return Byte is
   begin
      return O_Reg'Pos (R) - O_Reg'Pos (R_Xmm0);
   end To_Reg_Xmm;
   pragma Inline (To_Reg_Xmm);

   function To_Reg32 (R : O_Reg; Sz : Insn_Size) return Byte is
   begin
      case Sz is
         when Sz_8 =>
            pragma Assert (R in Regs_R8);
            return O_Reg'Pos (R) - O_Reg'Pos (R_Ax);
         when Sz_16 =>
            pragma Assert (R in Regs_R32);
            return O_Reg'Pos (R) - O_Reg'Pos (R_Ax);
         when Sz_32l =>
            case R is
               when Regs_R32 =>
                  return O_Reg'Pos (R) - O_Reg'Pos (R_Ax);
               when R_Edx_Eax =>
                  return 2#000#;
               when R_Ebx_Ecx =>
                  return 2#001#;
               when R_Esi_Edi =>
                  return 2#111#;
               when others =>
                  raise Program_Error;
            end case;
         when Sz_32h =>
            case R is
               when R_Edx_Eax =>
                  return 2#010#;
               when R_Ebx_Ecx =>
                  return 2#011#;
               when R_Esi_Edi =>
                  return 2#110#;
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end To_Reg32;

   function To_Cond (R : O_Reg) return Byte is
   begin
      return O_Reg'Pos (R) - O_Reg'Pos (R_Ov);
   end To_Cond;
   pragma Inline (To_Cond);

   --  Write the SIB byte.
   procedure Gen_Sib
   is
      Base : Byte;
   begin
      if Rm_Base = R_Nil then
         Base := 2#101#;
      else
         Base := To_Reg32 (Rm_Base);
      end if;
      Gen_B8 (SIB_Scale * 2#1_000_000#
                + To_Reg32 (SIB_Index) * 2#1_000#
                + Base);
   end Gen_Sib;

   procedure Init_Modrm_Reg (Reg : O_Reg; Sz : Insn_Size) is
   begin
      Rm_Base := R_Nil;
      SIB_Index := R_Nil;
      SIB_Scale := 0;
      Rm_Sym := Null_Symbol;
      Rm_Offset := 0;

      Rm_Reg := Reg;
      Rm_Sz := Sz;
   end Init_Modrm_Reg;

   --  Note: SZ is not relevant.
   procedure Init_Modrm_Sym (Sym : Symbol; Sz : Insn_Size) is
   begin
      Rm_Base := R_Nil;
      SIB_Index := R_Nil;
      SIB_Scale := 0;
      Rm_Sym := Sym;
      Rm_Offset := 0;

      Rm_Reg := R_Nil;
      Rm_Sz := Sz;
   end Init_Modrm_Sym;

   procedure Init_Modrm_Mem (N : O_Enode; Sz : Insn_Size)
   is
      Reg : constant O_Reg := Get_Expr_Reg (N);
   begin
      Rm_Base := R_Nil;
      SIB_Index := R_Nil;
      Rm_Reg := R_Nil;
      Rm_Sz := Sz;

      if Sz = Sz_32h then
         Rm_Offset := 4;
      else
         Rm_Offset := 0;
      end if;
      SIB_Scale := 0;
      Rm_Sym := Null_Symbol;
      case Reg is
         when R_Mem
           | R_Imm
           | R_Eq
           | R_B_Off
           | R_B_I
           | R_I_Off
           | R_Sib =>
            Fill_Sib (N);
         when Regs_R32 =>
            Rm_Base := Reg;
         when R_Spill =>
            Rm_Base := R_Bp;
            Rm_Offset := Rm_Offset + Get_Spill_Info (N);
         when others =>
            Error_Emit ("init_modrm_mem: unhandled reg", N);
      end case;
   end Init_Modrm_Mem;

   procedure Init_Rm_Expr (N : O_Enode; Sz : Insn_Size)
   is
      Reg : constant O_Reg := Get_Expr_Reg (N);
   begin
      case Reg is
         when Regs_R32
           | Regs_R64
           | Regs_Xmm =>
            --  Destination is a register.
            Init_Modrm_Reg (Reg, Sz);
         when others =>
            --  Destination is an effective address.
            Init_Modrm_Mem (N, Sz);
      end case;
   end Init_Rm_Expr;

   procedure Init_Modrm_Offset (Base : O_Reg; Off : Int32; Sz : Insn_Size) is
   begin
      SIB_Index := R_Nil;
      SIB_Scale := 0;
      Rm_Reg := R_Nil;
      Rm_Sym := Null_Symbol;
      Rm_Sz := Sz;

      Rm_Base := Base;

      if Sz = Sz_32h then
         Rm_Offset := Off + 4;
      else
         Rm_Offset := Off;
      end if;
   end Init_Modrm_Offset;

   --  Generate an R/M (+ SIB) byte.
   --  R is added to the R/M byte.
   procedure Gen_Mod_Rm (R : Byte) is
   begin
      --  Emit bytes.
      if SIB_Index /= R_Nil then
         pragma Assert (Rm_Reg = R_Nil);
         --  SIB.
         if Rm_Base = R_Nil then
            --  No base (but index).  Use the special encoding with base=BP.
            Gen_B8 (2#00_000_100# + R);
            Rm_Base := R_Bp;
            Gen_Sib;
            Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
         elsif Rm_Sym = Null_Symbol and Rm_Offset = 0 and Rm_Base /= R_Bp then
            --  No offset (only allowed if base is not BP).
            Gen_B8 (2#00_000_100# + R);
            Gen_Sib;
         elsif Rm_Sym = Null_Symbol and Rm_Offset <= 127 and Rm_Offset >= -128
         then
            --  Disp8
            Gen_B8 (2#01_000_100# + R);
            Gen_Sib;
            Gen_B8 (Byte (To_Uns32 (Rm_Offset) and 16#Ff#));
         else
            --  Disp32
            Gen_B8 (2#10_000_100# + R);
            Gen_Sib;
            Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
         end if;
         return;
      end if;

      --  No SIB.
      if Rm_Reg /= R_Nil then
         --  Mod is register, no memory access.
         pragma Assert (Rm_Base = R_Nil);
         pragma Assert (Rm_Sym = Null_Symbol);
         pragma Assert (Rm_Offset = 0);
         if Rm_Reg in Regs_Xmm then
            Gen_B8 (2#11_000_000# + R + To_Reg_Xmm (Rm_Reg));
         else
            Gen_B8 (2#11_000_000# + R + To_Reg32 (Rm_Reg, Rm_Sz));
         end if;
         return;
      end if;

      case Rm_Base is
         when R_Sp =>
            --  It isn't possible to use SP as a base register without using
            --  an SIB encoding.
            raise Program_Error;
         when R_Nil =>
            --  Encode for disp32 (Mod=00, R/M=101).
            Gen_B8 (2#00_000_101# + R);
            Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
         when R_Ax
            | R_Bx
            | R_Cx
            | R_Dx
            | R_Bp
            | R_Si
            | R_Di =>
            if Rm_Offset = 0 and Rm_Sym = Null_Symbol and Rm_Base /= R_Bp then
               --  No disp: use Mod=00 (not supported if base is BP).
               Gen_B8 (2#00_000_000# + R + To_Reg32 (Rm_Base));
            elsif Rm_Sym = Null_Symbol
               and Rm_Offset <= 127 and Rm_Offset >= -128
            then
               --  Disp8 (Mod=01)
               Gen_B8 (2#01_000_000# + R + To_Reg32 (Rm_Base));
               Gen_B8 (Byte (To_Uns32 (Rm_Offset) and 16#Ff#));
            else
               --  Disp32 (Mod=10)
               Gen_B8 (2#10_000_000# + R + To_Reg32 (Rm_Base));
               Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
            end if;
         when others =>
            raise Program_Error;
      end case;
   end Gen_Mod_Rm;

   procedure Gen_Rm (R : Byte; N : O_Enode; Sz : Insn_Size)
   is
      Reg : constant O_Reg := Get_Expr_Reg (N);
   begin
      if Reg in Regs_R32 or Reg in Regs_R64 then
         --  Destination is a register.
         Gen_B8 (2#11_000_000# + R + To_Reg32 (Reg, Sz));
      else
         --  Destination is an effective address.
         Init_Modrm_Mem (N, Sz);
         Gen_Mod_Rm (R);
      end if;
   end Gen_Rm;

   procedure Gen_Grp1_Insn (Op : Byte; Stmt : O_Enode; Sz : Insn_Size)
   is
      L : constant O_Enode := Get_Expr_Left (Stmt);
      R : constant O_Enode := Get_Expr_Right (Stmt);
      Lr : constant O_Reg := Get_Expr_Reg (L);
      Rr : constant O_Reg := Get_Expr_Reg (R);
   begin
      Start_Insn;
      case Rr is
         when R_Imm =>
            if Is_Imm8 (R, Sz) then
               Gen_Insn_Sz_S8 (16#80#, Sz);
               Gen_Rm (Op, L, Sz);
               Gen_Imm8 (R, Sz);
            elsif Lr = R_Ax then
               Gen_Insn_Sz (2#000_000_100# + Op, Sz);
               Gen_Imm (R, Sz);
            else
               Gen_Insn_Sz (16#80#, Sz);
               Gen_Rm (Op, L, Sz);
               Gen_Imm (R, Sz);
            end if;
         when R_Mem
           | R_Spill
           | Regs_R32
           | Regs_R64 =>
            Gen_Insn_Sz (2#00_000_010# + Op, Sz);
            Gen_Rm (To_Reg32 (Lr, Sz) * 8, R, Sz);
         when others =>
            Error_Emit ("emit_op", Stmt);
      end case;
      End_Insn;
   end Gen_Grp1_Insn;

   --  Emit a one byte instruction.
   procedure Gen_1 (B : Byte) is
   begin
      Start_Insn;
      Gen_B8 (B);
      End_Insn;
   end Gen_1;

   --  Emit a two byte instruction.
   procedure Gen_2 (B1, B2 : Byte) is
   begin
      Start_Insn;
      Gen_B8 (B1);
      Gen_B8 (B2);
      End_Insn;
   end Gen_2;

   --  Grp1 instructions have a mod/rm and an immediate value VAL.
   --  Mod/Rm must be initialized.
   procedure Gen_Insn_Grp1 (Opc2 : Byte; Sz : Insn_Size; Val : Int32) is
   begin
      Start_Insn;
      if Val in -128 .. 127 then
         case Sz is
            when Sz_8 =>
               Gen_B8 (Opc_Grp1b_Rm_Imm8);
            when Sz_16 =>
               Gen_B8 (Opc_Data16);
               Gen_B8 (Opc_Grp1v_Rm_Imm8);
            when Sz_32l
              | Sz_32h =>
               Gen_B8 (Opc_Grp1v_Rm_Imm8);
         end case;
         Gen_Mod_Rm (Opc2);
         Gen_B8 (Byte (To_Uns32 (Val) and 16#Ff#));
      else
         case Sz is
            when Sz_8 =>
               pragma Assert (False);
               null;
            when Sz_16 =>
               Gen_B8 (Opc_Data16);
               Gen_B8 (Opc_Grp1v_Rm_Imm32);
            when Sz_32l
              | Sz_32h =>
               Gen_B8 (Opc_Grp1v_Rm_Imm32);
         end case;
         Gen_Mod_Rm (Opc2);
         Gen_Le32 (Unsigned_32 (To_Uns32 (Val)));
      end if;
      End_Insn;
   end Gen_Insn_Grp1;

   procedure Gen_Into is
   begin
      Gen_1 (Opc_Into);
   end Gen_Into;

   procedure Gen_Cdq is
   begin
      Gen_1 (Opc_Cdq);
   end Gen_Cdq;

   procedure Gen_Clear_Edx is
   begin
      --  Xorl edx, edx
      Gen_2 (Opc_Xorl_Rm_Reg, 2#11_010_010#);
   end Gen_Clear_Edx;

   procedure Gen_Grp3_Insn (Op : Byte; Val : O_Enode; Sz : Insn_Size) is
   begin
      Start_Insn;
      --  Unary Group 3 (test, not, neg...)
      Gen_Insn_Sz (Opc_Grp3_Width, Sz);
      Gen_Rm (Op, Val, Sz);
      End_Insn;
   end Gen_Grp3_Insn;

   procedure Gen_Grp3_Insn_Stmt (Op : Byte; Stmt : O_Enode; Sz : Insn_Size)
   is
   begin
      Gen_Grp3_Insn (Op, Get_Expr_Operand (Stmt), Sz);
   end Gen_Grp3_Insn_Stmt;

   procedure Emit_Load_Imm (Stmt : O_Enode; Sz : Insn_Size)
   is
      Tr : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      Start_Insn;
      --  TODO: handle 0 specially: use xor
      --  Mov immediate.
      case Sz is
         when Sz_8 =>
            Gen_B8 (Opc_Movb_Imm_Reg + To_Reg32 (Tr, Sz));
         when Sz_16 =>
            Gen_B8 (Opc_Data16);
            Gen_B8 (Opc_Movl_Imm_Reg + To_Reg32 (Tr, Sz));
         when Sz_32l
           | Sz_32h =>
            Gen_B8 (Opc_Movl_Imm_Reg + To_Reg32 (Tr, Sz));
      end case;
      Gen_Imm (Stmt, Sz);
      End_Insn;
   end Emit_Load_Imm;

   function Mode_Fp_To_Mf (Mode : Mode_Fp) return Byte is
   begin
      case Mode is
         when Mode_F32 =>
            return 2#00_0#;
         when Mode_F64 =>
            return 2#10_0#;
      end case;
   end Mode_Fp_To_Mf;

   function Gen_Constant_Start return Symbol
   is
      Sym : Symbol;
   begin
      --  Write the constant in .rodata
      Set_Current_Section (Sect_Rodata);
      Gen_Pow_Align (3);
      Prealloc (8);
      Sym := Create_Local_Symbol;
      Set_Symbol_Pc (Sym, False);
      return Sym;
   end Gen_Constant_Start;

   function Gen_Constant_32 (Val : Unsigned_32) return Symbol
   is
      Sym : Symbol;
   begin
      Sym := Gen_Constant_Start;
      Gen_Le32 (Val);
      Set_Current_Section (Sect_Text);
      return Sym;
   end Gen_Constant_32;

   function Gen_Constant_64 (Lo, Hi : Unsigned_32) return Symbol
   is
      Sym : Symbol;
   begin
      Sym := Gen_Constant_Start;
      Gen_Le32 (Lo);
      Gen_Le32 (Hi);
      Set_Current_Section (Sect_Text);
      return Sym;
   end Gen_Constant_64;

   Xmm_Sign32_Sym : Symbol := Null_Symbol;
   Xmm_Sign64_Sym : Symbol := Null_Symbol;

   function Get_Xmm_Sign_Constant (Mode : Mode_Fp) return Symbol is
   begin
      case Mode is
         when Mode_F32 =>
            if Xmm_Sign32_Sym = Null_Symbol then
               Xmm_Sign32_Sym := Gen_Constant_32 (16#8000_0000#);
            end if;
            return Xmm_Sign32_Sym;
         when Mode_F64 =>
            if Xmm_Sign64_Sym = Null_Symbol then
               Xmm_Sign64_Sym := Gen_Constant_64 (0, 16#8000_0000#);
            end if;
            return Xmm_Sign64_Sym;
      end case;
   end Get_Xmm_Sign_Constant;

   procedure Gen_SSE_Rep_Opc (Sz : Mode_Fp; Opc : Byte) is
   begin
      case Sz is
         when Mode_F32 =>
            Gen_B8 (16#f3#);
         when Mode_F64 =>
            Gen_B8 (16#f2#);
      end case;
      Gen_B8 (16#0f#);
      Gen_B8 (Opc);
   end Gen_SSE_Rep_Opc;

   procedure Gen_SSE_D16_Opc (Sz : Mode_Fp; Opc : Byte) is
   begin
      case Sz is
         when Mode_F32 =>
            null;
         when Mode_F64 =>
            Gen_B8 (Opc_Data16);
      end case;
      Gen_B8 (16#0f#);
      Gen_B8 (Opc);
   end Gen_SSE_D16_Opc;

   procedure Emit_Load_Fp (Stmt : O_Enode; Mode : Mode_Fp)
   is
      Sym : Symbol;
      R : O_Reg;
      Lo : constant Unsigned_32 := Unsigned_32 (Get_Expr_Low (Stmt));
   begin
      case Mode is
         when Mode_F32 =>
            Sym := Gen_Constant_32 (Lo);
         when Mode_F64 =>
            Sym := Gen_Constant_64 (Lo, Unsigned_32 (Get_Expr_High (Stmt)));
      end case;

      --  Load the constant.
      R := Get_Expr_Reg (Stmt);
      case R is
         when R_St0 =>
            Start_Insn;
            Gen_B8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
            Gen_B8 (2#00_000_101#);
            Gen_X86_32 (Sym, 0);
            End_Insn;
         when Regs_Xmm =>
            Start_Insn;
            Gen_SSE_Rep_Opc (Mode, 16#10#);
            Gen_B8 (2#00_000_101# + To_Reg_Xmm (R) * 2#1_000#);
            Gen_X86_32 (Sym, 0);
            End_Insn;
         when others =>
            raise Program_Error;
      end case;
   end Emit_Load_Fp;

   function Xmm_To_Modrm_Reg (R : O_Reg) return Byte is
   begin
      return To_Reg_Xmm (R) * 8;
   end Xmm_To_Modrm_Reg;

   procedure Gen_Xmm_Modrm (Mode : Mode_Fp; Opc : Byte; Dest : O_Reg) is
   begin
      Start_Insn;
      Gen_SSE_Rep_Opc (Mode, Opc);
      Gen_Mod_Rm (Xmm_To_Modrm_Reg (Dest));
      End_Insn;
   end Gen_Xmm_Modrm;

   procedure Emit_Load_Fp_Mem (Stmt : O_Enode; Mode : Mode_Fp)
   is
      Dest : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      Init_Modrm_Mem (Get_Expr_Operand (Stmt), Sz_32l);
      if Dest in Regs_Xmm then
         Gen_Xmm_Modrm (Mode, 16#10#, Dest);
      else
         Start_Insn;
         Gen_B8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
         Init_Modrm_Mem (Get_Expr_Operand (Stmt), Sz_32l);
         Gen_Mod_Rm (2#000_000#);
         End_Insn;
      end if;
   end Emit_Load_Fp_Mem;

   procedure Emit_Load_Mem (Stmt : O_Enode; Sz : Insn_Size)
   is
      Tr  : constant O_Reg := Get_Expr_Reg (Stmt);
      Val : constant O_Enode := Get_Expr_Operand (Stmt);
   begin
      case Tr is
         when Regs_R32
           | Regs_R64 =>
            --  mov REG, OP
            Init_Modrm_Mem (Val, Sz);
            Start_Insn;
            Gen_Insn_Sz (Opc_Mov_Reg_Rm, Sz);
            Gen_Mod_Rm (To_Reg32 (Tr, Sz) * 8);
            End_Insn;
         when R_Eq =>
            --  Cmp OP, 1
            Init_Modrm_Mem (Val, Sz);
            Gen_Insn_Grp1 (Opc2_Grp1_Cmp, Sz, 1);
         when others =>
            Error_Emit ("emit_load_mem", Stmt);
      end case;
   end Emit_Load_Mem;


   procedure Emit_Store (Stmt : O_Enode; Sz : Insn_Size)
   is
      T : constant O_Enode := Get_Assign_Target (Stmt);
      R : constant O_Enode := Get_Expr_Operand (Stmt);
      Tr : constant O_Reg := Get_Expr_Reg (T);
      Rr : constant O_Reg := Get_Expr_Reg (R);
      B : Byte;
   begin
      Start_Insn;
      case Rr is
         when R_Imm =>
            if False and (Tr in Regs_R32 or Tr in Regs_R64) then
               B := 2#1011_1_000#;
               case Sz is
                  when Sz_8 =>
                     B := B and not 2#0000_1_000#;
                  when Sz_16 =>
                     Gen_B8 (16#66#);
                  when Sz_32l
                    | Sz_32h =>
                     null;
               end case;
               Gen_B8 (B + To_Reg32 (Tr, Sz));
            else
               Init_Modrm_Mem (T, Sz);
               Gen_Insn_Sz (Opc_Mov_Rm_Imm, Sz);
               Gen_Mod_Rm (16#00#);
            end if;
            Gen_Imm (R, Sz);
         when Regs_R32
           | Regs_R64 =>
            Gen_Insn_Sz (Opc_Mov_Rm_Reg, Sz);
            Init_Modrm_Mem (T, Sz);
            Gen_Mod_Rm (To_Reg32 (Rr, Sz) * 8);
         when others =>
            Error_Emit ("emit_store", Stmt);
      end case;
      End_Insn;
   end Emit_Store;

   procedure Emit_Store_Fp (Stmt : O_Enode; Sz : Mode_Fp)
   is
   begin
      -- fstp
      Start_Insn;
      Gen_B8 (2#11011_00_1# + Mode_Fp_To_Mf (Sz));
      Init_Modrm_Mem (Get_Assign_Target (Stmt), Sz_32l);
      Gen_Mod_Rm (2#011_000#);
      End_Insn;
   end Emit_Store_Fp;

   procedure Emit_Push_32 (Val : O_Enode; Sz : Insn_Size)
   is
      R : constant O_Reg := Get_Expr_Reg (Val);
   begin
      Start_Insn;
      case R is
         when R_Imm =>
            if Is_Imm8 (Val, Sz) then
               Gen_B8 (Opc_Push_Imm8);
               Gen_Imm8 (Val, Sz);
            else
               Gen_B8 (Opc_Push_Imm);
               Gen_Imm (Val, Sz);
            end if;
         when Regs_R32
           | Regs_R64 =>
            Gen_B8 (Opc_Push_Reg + To_Reg32 (R, Sz));
         when others =>
            Gen_B8 (Opc_Grp5);
            Gen_Rm (Opc2_Grp5_Push_Rm, Val, Sz);
      end case;
      End_Insn;
   end Emit_Push_32;

   procedure Emit_Subl_Sp_Imm (Len : Byte) is
   begin
      Start_Insn;
      Gen_B8 (Opc_Grp1v_Rm_Imm8);
      Gen_B8 (Opc2_Grp1_Sub + 2#11_000_100#);
      Gen_B8 (Len);
      End_Insn;
   end Emit_Subl_Sp_Imm;

   procedure Emit_Addl_Sp_Imm (Len : Byte) is
   begin
      Start_Insn;
      Gen_B8 (Opc_Grp1v_Rm_Imm8);
      Gen_B8 (Opc2_Grp1_Add + 2#11_000_100#);
      Gen_B8 (Len);
      End_Insn;
   end Emit_Addl_Sp_Imm;

   procedure Emit_Push_Fp (Op : O_Enode; Mode : Mode_Fp)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Op);
      Len : Byte;
   begin
      --  subl esp, val
      case Mode is
         when Mode_F32 =>
            Len := 4;
         when Mode_F64 =>
            Len := 8;
      end case;
      Emit_Subl_Sp_Imm (Len);

      if Reg = R_St0 then
         --  fstp st, (esp)
         Start_Insn;
         Gen_B8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
         Gen_B8 (2#00_011_100#);  --  Modrm: SIB, no disp
         Gen_B8 (2#00_100_100#);  --  SIB: SS=0, no index, base=esp
         End_Insn;
      else
         pragma Assert (Reg in Regs_Xmm);
         Start_Insn;
         Gen_SSE_Rep_Opc (Mode, 16#11#);
         Gen_B8 (To_Reg_Xmm (Reg) * 8 + 2#00_000_100#);  --  Modrm: [--]
         Gen_B8 (2#00_100_100#);  --  SIB: SS=0, no index, base=esp
         End_Insn;
      end if;
   end Emit_Push_Fp;

   function Prepare_Label (Label : O_Enode) return Symbol
   is
      Sym : Symbol;
   begin
      Sym := Get_Label_Symbol (Label);
      if Sym = Null_Symbol then
         Sym := Create_Local_Symbol;
         Set_Label_Symbol (Label, Sym);
      end if;
      return Sym;
   end Prepare_Label;

   procedure Emit_Jmp_T (Stmt : O_Enode; Reg : O_Reg)
   is
      Sym : Symbol;
      Val : Pc_Type;
      Opc : Byte;
   begin
      Sym := Prepare_Label (Get_Jump_Label (Stmt));
      Val := Get_Symbol_Value (Sym);
      Start_Insn;
      Opc := To_Cond (Reg);
      if Val = 0 then
         --  Assume long jmp.
         Gen_B8 (Opc_0f);
         Gen_B8 (Opc2_0f_Jcc + Opc);
         Gen_X86_Pc32 (Sym);
      else
         if Val + 128 < Get_Current_Pc + 4 then
            --  Long jmp.
            Gen_B8 (Opc_0f);
            Gen_B8 (Opc2_0f_Jcc + Opc);
            Gen_Le32 (Unsigned_32 (Val - (Get_Current_Pc + 4)));
         else
            --  short jmp.
            Gen_B8 (Opc_Jcc + Opc);
            Gen_B8 (Byte (Val - (Get_Current_Pc + 1)));
         end if;
      end if;
      End_Insn;
   end Emit_Jmp_T;

   procedure Emit_Jmp (Stmt : O_Enode)
   is
      Sym : Symbol;
      Val : Pc_Type;
   begin
      Sym := Prepare_Label (Get_Jump_Label (Stmt));
      Val := Get_Symbol_Value (Sym);
      Start_Insn;
      if Val = 0 then
         --  Assume long jmp.
         Gen_B8 (Opc_Jmp_Long);
         Gen_X86_Pc32 (Sym);
      else
         if Val + 128 < Get_Current_Pc + 4 then
            --  Long jmp.
            Gen_B8 (Opc_Jmp_Long);
            Gen_Le32 (Unsigned_32 (Val - (Get_Current_Pc + 4)));
         else
            --  short jmp.
            Gen_B8 (Opc_Jmp_Short);
            Gen_B8 (Byte ((Val - (Get_Current_Pc + 1)) and 16#Ff#));
         end if;
      end if;
      End_Insn;
   end Emit_Jmp;

   procedure Emit_Label (Stmt : O_Enode)
   is
      Sym : Symbol;
   begin
      Sym := Prepare_Label (Stmt);
      Set_Symbol_Pc (Sym, False);
   end Emit_Label;

   procedure Gen_Call (Sym : Symbol) is
   begin
      Start_Insn;
      Gen_B8 (Opc_Call);
      Gen_X86_Pc32 (Sym);
      End_Insn;
   end Gen_Call;

   procedure Emit_Stack_Adjust (Stmt : O_Enode)
   is
      Val : constant Int32 := Get_Stack_Adjust (Stmt);
   begin
      if Val > 0 then
         --  subl esp, val
         Emit_Subl_Sp_Imm (Byte (Val));
      elsif Val < 0 then
         Init_Modrm_Reg (R_Sp, Sz_32l);
         Gen_Insn_Grp1 (Opc2_Grp1_Add, Sz_32l, -Val);
      end if;
   end Emit_Stack_Adjust;

   procedure Emit_Call (Stmt : O_Enode)
   is
      use Ortho_Code.Decls;
      Subprg : constant O_Dnode := Get_Call_Subprg (Stmt);
      Sym : constant Symbol := Get_Decl_Symbol (Subprg);
      Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
   begin
      Gen_Call (Sym);

      if Abi.Flag_Sse2 and then Mode in Mode_Fp then
         --  Move from St0 to Xmm0.
         --  fstp slot(%ebp)
         Init_Modrm_Offset
           (R_Bp, -Int32 (Cur_Subprg.Target.Fp_Slot), Sz_32l);
         Start_Insn;
         Gen_B8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
         Gen_Mod_Rm (2#00_011_000#);
         End_Insn;
         --  movsd slot(%ebp), %xmm0
         Start_Insn;
         Gen_SSE_Rep_Opc (Mode, 16#10#);
         Gen_Mod_Rm (2#00_000_000#);
         End_Insn;
      end if;
   end Emit_Call;

   procedure Emit_Intrinsic (Stmt : O_Enode)
   is
      Op : constant Int32 := Get_Intrinsic_Operation (Stmt);
   begin
      --  Call sym
      Gen_Call (Intrinsics_Symbol (Op));

      --  addl esp, val
      Emit_Addl_Sp_Imm (16);
   end Emit_Intrinsic;

   procedure Emit_Setcc (Dest : O_Enode; Cond : O_Reg) is
   begin
      pragma Assert (Cond in Regs_Cc);
      Start_Insn;
      Gen_B8 (Opc_0f);
      Gen_B8 (Opc2_0f_Setcc + To_Cond (Cond));
      Gen_Rm (2#000_000#, Dest, Sz_8);
      End_Insn;
   end Emit_Setcc;

   procedure Emit_Setcc_Reg (Reg : O_Reg; Cond : O_Reg) is
   begin
      pragma Assert (Cond in Regs_Cc);
      Start_Insn;
      Gen_B8 (Opc_0f);
      Gen_B8 (Opc2_0f_Setcc + To_Cond (Cond));
      Gen_B8 (2#11_000_000# + To_Reg32 (Reg, Sz_8));
      End_Insn;
   end Emit_Setcc_Reg;

   procedure Emit_Tst (Reg : O_Reg; Sz : Insn_Size) is
   begin
      Start_Insn;
      Gen_Insn_Sz (Opc_Test_Rm_Reg, Sz);
      Gen_B8 (2#11_000_000# + To_Reg32 (Reg, Sz) * 9);
      End_Insn;
   end Emit_Tst;

   procedure Gen_Cmp_Imm (Reg : O_Reg; Val : Int32; Sz : Insn_Size) is
   begin
      Init_Modrm_Reg (Reg, Sz);
      Gen_Insn_Grp1 (Opc2_Grp1_Cmp, Sz, Val);
   end Gen_Cmp_Imm;

   procedure Emit_Spill (Stmt : O_Enode; Sz : Insn_Size)
   is
      Expr : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg : constant O_Reg := Get_Expr_Reg (Expr);
   begin
      --  A reload is missing.
      pragma Assert (Reg /= R_Spill);
      Init_Modrm_Mem (Stmt, Sz);
      Start_Insn;
      Gen_Insn_Sz (Opc_Mov_Rm_Reg, Sz);
      Gen_Mod_Rm (To_Reg32 (Reg, Sz) * 8);
      End_Insn;
   end Emit_Spill;

   procedure Emit_Load (Reg : O_Reg; Val : O_Enode; Sz : Insn_Size)
   is
   begin
      Start_Insn;
      Gen_Insn_Sz (Opc_Mov_Reg_Rm, Sz);
      Gen_Rm (To_Reg32 (Reg, Sz) * 8, Val, Sz);
      End_Insn;
   end Emit_Load;

   procedure Emit_Lea (Stmt : O_Enode)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      --  Hack: change the register to use the real address instead of it.
      Set_Expr_Reg (Stmt, R_Mem);

      Init_Modrm_Mem (Stmt, Sz_32l);
      Start_Insn;
      Gen_B8 (Opc_Leal_Reg_Rm);
      Gen_Mod_Rm (To_Reg32 (Reg) * 8);
      End_Insn;

      --  Restore.
      Set_Expr_Reg (Stmt, Reg);
   end Emit_Lea;

   procedure Gen_Umul (Stmt : O_Enode; Sz : Insn_Size)
   is
   begin
      pragma Assert (Get_Expr_Reg (Get_Expr_Left (Stmt)) = R_Ax);
      Start_Insn;
      Gen_Insn_Sz (Opc_Grp3_Width, Sz);
      Gen_Rm (Opc2_Grp3_Mul, Get_Expr_Right (Stmt), Sz);
      End_Insn;
   end Gen_Umul;

   procedure Gen_Mul (Stmt : O_Enode; Sz : Insn_Size)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
      Right : constant O_Enode := Get_Expr_Right (Stmt);
      Reg_R : O_Reg;
   begin
      pragma Assert (Get_Expr_Reg (Get_Expr_Left (Stmt)) = Reg);
      pragma Assert (Sz = Sz_32l);
      Start_Insn;
      if Reg = R_Ax then
         Gen_Insn_Sz (Opc_Grp3_Width, Sz);
         Gen_Rm (Opc2_Grp3_Mul, Right, Sz);
      else
         Reg_R := Get_Expr_Reg (Right);
         case Reg_R is
            when R_Imm =>
               if Is_Imm8 (Right, Sz) then
                  Gen_B8 (Opc_Imul_Reg_Rm_Imm8);
                  Gen_B8 (To_Reg32 (Reg, Sz) * 9 or 2#11_000_000#);
                  Gen_Imm8 (Right, Sz);
               else
                  Gen_B8 (Opc_Imul_Reg_Rm_Imm32);
                  Gen_B8 (To_Reg32 (Reg, Sz) * 9 or 2#11_000_000#);
                  Gen_Imm (Right, Sz);
               end if;
            when R_Mem
              | R_Spill
              | Regs_R32 =>
               Gen_B8 (Opc_0f);
               Gen_B8 (Opc2_0f_Imul);
               Gen_Rm (To_Reg32 (Reg, Sz) * 8, Right, Sz);
            when others =>
               Error_Emit ("gen_mul", Stmt);
         end case;
      end if;
      End_Insn;
   end Gen_Mul;

   --  Do not trap if COND is true.
   procedure Gen_Ov_Check (Cond : O_Reg) is
   begin
      --  JXX +2
      Gen_2 (Opc_Jcc + To_Cond (Cond), 16#02#);
      --  INT 4 (overflow).
      Gen_2 (Opc_Int, 16#04#);
   end Gen_Ov_Check;

   procedure Emit_Abs (Val : O_Enode; Mode : Mode_Type)
   is
      Szh : Insn_Size;
      Pc_Jmp : Pc_Type;
   begin
      case Mode is
         when Mode_I32 =>
            Szh := Sz_32l;
         when Mode_I64 =>
            Szh := Sz_32h;
         when others =>
            raise Program_Error;
      end case;
      Emit_Tst (Get_Expr_Reg (Val), Szh);
      --  JXX +
      Start_Insn;
      Gen_B8 (Opc_Jcc + To_Cond (R_Sge));
      Gen_B8 (0);
      End_Insn;
      Pc_Jmp := Get_Current_Pc;
      --  NEG
      Gen_Grp3_Insn (Opc2_Grp3_Neg, Val, Sz_32l);
      if Mode = Mode_I64 then
         --  Propagate carry.
         --  Adc reg,0
         --  neg reg
         Init_Rm_Expr (Val, Sz_32h);
         Gen_Insn_Grp1 (Opc2_Grp1_Adc, Sz_32h, 0);
         Gen_Grp3_Insn (Opc2_Grp3_Neg, Val, Sz_32h);
      end if;
      Gen_Into;
      Patch_B8 (Pc_Jmp - 1, Unsigned_8 (Get_Current_Pc - Pc_Jmp));
   end Emit_Abs;

   procedure Gen_Alloca (Stmt : O_Enode)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Get_Expr_Operand (Stmt));
   begin
      pragma Assert (Reg in Regs_R32);
      pragma Assert (Reg = Get_Expr_Reg (Stmt));
      --  Align stack on word.
      --  Add reg, (stack_boundary - 1)
      Start_Insn;
      Gen_B8 (Opc_Grp1v_Rm_Imm8);
      Gen_B8 (Opc2_Grp1_Add or 2#11_000_000# or To_Reg32 (Reg));
      Gen_B8 (Byte (X86.Flags.Stack_Boundary - 1));
      End_Insn;
      --  and reg, ~(stack_boundary - 1)
      Start_Insn;
      Gen_B8 (Opc_Grp1v_Rm_Imm32);
      Gen_B8 (Opc2_Grp1_And or 2#11_000_000# or To_Reg32 (Reg));
      Gen_Le32 (not (X86.Flags.Stack_Boundary - 1));
      End_Insn;
      if X86.Flags.Flag_Alloca_Call then
         Gen_Call (Chkstk_Symbol);
      else
         --  subl esp, reg
         Gen_2 (Opc_Subl_Reg_Rm, 2#11_100_000# + To_Reg32 (Reg));
      end if;
      --  movl reg, esp
      Gen_2 (Opc_Mov_Rm_Reg + 1, 2#11_100_000# + To_Reg32 (Reg));
   end Gen_Alloca;

   --  Byte/word to long.
   procedure Gen_Movzx (Reg : Regs_R32; Op : O_Enode; Sz : Insn_Size)
   is
      B : Byte;
   begin
      Start_Insn;
      Gen_B8 (Opc_0f);
      case Sz is
         when Sz_8 =>
            B := 0;
         when Sz_16 =>
            B := 1;
         when Sz_32l
           | Sz_32h =>
            raise Program_Error;
      end case;
      Gen_B8 (Opc2_0f_Movzx + B);
      Gen_Rm (To_Reg32 (Reg) * 8, Op, Sz_8);
      End_Insn;
   end Gen_Movzx;

   --  Convert U32 to xx.
   procedure Gen_Conv_U32 (Stmt : O_Enode)
   is
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_I32 =>
            pragma Assert (Reg_Res in Regs_R32);
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32l);
            end if;
            Emit_Tst (Reg_Res, Sz_32l);
            Gen_Ov_Check (R_Sge);
         when Mode_I64 =>
            pragma Assert (Reg_Res = R_Edx_Eax);
            pragma Assert (Reg_Op = R_Ax);
            --  Clear edx.
            Gen_Clear_Edx;
         when Mode_U8
           | Mode_B2 =>
            pragma Assert (Reg_Res in Regs_R32);
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32l);
            end if;
            --  cmpl VAL, 0xff
            Start_Insn;
            Gen_B8 (Opc_Grp1v_Rm_Imm32);
            Gen_Rm (Opc2_Grp1_Cmp, Op, Sz_32l);
            Gen_Le32 (16#00_00_00_Ff#);
            End_Insn;
            Gen_Ov_Check (R_Ule);
         when others =>
            Error_Emit ("gen_conv_u32", Stmt);
      end case;
   end Gen_Conv_U32;

   --  Convert I32 to xxx
   procedure Gen_Conv_I32 (Stmt : O_Enode)
   is
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_I64 =>
            pragma Assert (Reg_Res = R_Edx_Eax);
            pragma Assert (Reg_Op = R_Ax);
            Gen_Cdq;
         when Mode_U32 =>
            pragma Assert (Reg_Res in Regs_R32);
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32l);
            end if;
            Emit_Tst (Reg_Res, Sz_32l);
            Gen_Ov_Check (R_Sge);
         when Mode_B2 =>
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32l);
            end if;
            Gen_Cmp_Imm (Reg_Res, 1, Sz_32l);
            Gen_Ov_Check (R_Ule);
         when Mode_U8 =>
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32l);
            end if;
            Gen_Cmp_Imm (Reg_Res, 16#Ff#, Sz_32l);
            Gen_Ov_Check (R_Ule);
         when Mode_F64 =>
            Emit_Push_32 (Op, Sz_32l);
            --  fild (%esp)
            Start_Insn;
            Gen_B8 (2#11011_011#);
            Gen_B8 (2#00_000_100#);
            Gen_B8 (2#00_100_100#);
            End_Insn;
            if Reg_Res in Regs_Xmm then
               --  fstp (%esp)
               Start_Insn;
               Gen_B8 (2#11011_00_1# + Mode_Fp_To_Mf (Mode_F64));
               Gen_B8 (2#00_011_100#);
               Gen_B8 (2#00_100_100#);
               End_Insn;
               --  movsd (%esp), %xmm
               Start_Insn;
               Gen_SSE_Rep_Opc (Mode_F64, 16#10#);
               Gen_B8 (To_Reg_Xmm (Reg_Res) * 8 + 2#00_000_100#);
               Gen_B8 (2#00_100_100#);
               End_Insn;
            end if;
            --  addl %esp, 4
            Emit_Addl_Sp_Imm (4);
         when others =>
            Error_Emit ("gen_conv_i32", Stmt);
      end case;
   end Gen_Conv_I32;

   --  Convert U8 to xxx
   procedure Gen_Conv_U8 (Stmt : O_Enode)
   is
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_U32
           | Mode_I32
           | Mode_U16
           | Mode_I16 =>
            pragma Assert (Reg_Res in Regs_R32);
            Gen_Movzx (Reg_Res, Op, Sz_8);
         when Mode_I64
           | Mode_U64 =>
            pragma Assert (Reg_Res = R_Edx_Eax);
            pragma Assert (Reg_Op = R_Ax);
            Gen_Movzx (R_Ax, Op, Sz_8);
            --  Sign-extend, but we know the sign is positive.
            Gen_Cdq;
         when others =>
            Error_Emit ("gen_conv_U8", Stmt);
      end case;
   end Gen_Conv_U8;

   --  Convert B2 to xxx
   procedure Gen_Conv_B2 (Stmt : O_Enode)
   is
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_U32
           | Mode_I32
           | Mode_U16
           | Mode_I16 =>
            pragma Assert (Reg_Res in Regs_R32);
            Gen_Movzx (Reg_Res, Op, Sz_8);
         when Mode_I64 =>
            pragma Assert (Reg_Res = R_Edx_Eax);
            pragma Assert (Reg_Op = R_Ax);
            Gen_Movzx (R_Ax, Op, Sz_8);
            --  Sign-extend, but we know the sign is positive.
            Gen_Cdq;
         when others =>
            Error_Emit ("gen_conv_B2", Stmt);
      end case;
   end Gen_Conv_B2;

   --  Convert I64 to xxx
   procedure Gen_Conv_I64 (Stmt : O_Enode)
   is
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_I32 =>
            pragma Assert (Reg_Op = R_Edx_Eax);
            pragma Assert (Reg_Res = R_Ax);
            --  move dx to reg_helper
            Start_Insn;
            Gen_B8 (Opc_Mov_Rm_Reg + 1);
            Gen_B8 (2#11_010_000# + To_Reg32 (Reg_Helper));
            End_Insn;
            --  Sign extend eax.
            Gen_Cdq;
            --  cmp reg_helper, dx
            Start_Insn;
            Gen_B8 (Opc_Cmpl_Rm_Reg);
            Gen_B8 (2#11_010_000# + To_Reg32 (Reg_Helper));
            End_Insn;
            --  Overflow if extended value is different from initial value.
            Gen_Ov_Check (R_Eq);
         when Mode_U8 =>
            pragma Assert (Reg_Op in Regs_R64);
            --  Check MSB = 0
            Emit_Tst (Reg_Op, Sz_32h);
            Gen_Ov_Check (R_Eq);
            --  Check LSB <= 255
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32l);
            end if;
            Gen_Cmp_Imm (Reg_Res, 16#Ff#, Sz_32l);
            Gen_Ov_Check (R_Ule);
         when Mode_B2 =>
            pragma Assert (Reg_Op in Regs_R64);
            --  Check MSB = 0
            Emit_Tst (Reg_Op, Sz_32h);
            Gen_Ov_Check (R_Eq);
            --  Check LSB <= 1
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32l);
            end if;
            Gen_Cmp_Imm (Reg_Res, 16#1#, Sz_32l);
            Gen_Ov_Check (R_Ule);
         when Mode_F64 =>
            Emit_Push_32 (Op, Sz_32h);
            Emit_Push_32 (Op, Sz_32l);
            --  fild (%esp)
            Start_Insn;
            Gen_B8 (2#11011_111#);
            Gen_B8 (2#00_101_100#);
            Gen_B8 (2#00_100_100#);
            End_Insn;
            if Reg_Res in Regs_Xmm then
               --  fstp (%esp)
               Start_Insn;
               Gen_B8 (2#11011_00_1# + Mode_Fp_To_Mf (Mode_F64));
               Gen_B8 (2#00_011_100#);
               Gen_B8 (2#00_100_100#);
               End_Insn;
               --  movsd (%esp), %xmm
               Start_Insn;
               Gen_SSE_Rep_Opc (Mode_F64, 16#10#);
               Gen_B8 (To_Reg_Xmm (Reg_Res) * 8 + 2#00_000_100#);
               Gen_B8 (2#00_100_100#);
               End_Insn;
            end if;
            --  addl %esp, 8
            Emit_Addl_Sp_Imm (8);
         when others =>
            Error_Emit ("gen_conv_I64", Stmt);
      end case;
   end Gen_Conv_I64;

   --  Convert FP to xxx.
   procedure Gen_Conv_Fp (Stmt : O_Enode)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_I32 =>
            --  fistpl slot(%ebp)
            Init_Modrm_Offset
              (R_Bp, -Int32 (Cur_Subprg.Target.Fp_Slot), Sz_32l);
            Start_Insn;
            Gen_B8 (2#11011_011#);
            Gen_Mod_Rm (2#00_011_000#);
            End_Insn;
            --  movl slot(%ebp), reg
            --  Keep same modrm parameters.
            Start_Insn;
            Gen_B8 (Opc_Movl_Reg_Rm);
            Gen_Mod_Rm (To_Reg32 (Reg, Sz_32l) * 8);
            End_Insn;
         when Mode_I64 =>
            --  fistpq (%esp)
            Init_Modrm_Offset
              (R_Bp, -Int32 (Cur_Subprg.Target.Fp_Slot), Sz_32l);
            Start_Insn;
            Gen_B8 (2#11011_111#);
            Gen_Mod_Rm (2#00_111_000#);
            End_Insn;
            --  movl slot(%ebp), reg
            --  Keep same modrm parameters.
            Start_Insn;
            Gen_B8 (Opc_Movl_Reg_Rm);
            Gen_Mod_Rm (To_Reg32 (Reg, Sz_32l) * 8);
            End_Insn;
            Rm_Offset := Rm_Offset + 4;
            Start_Insn;
            Gen_B8 (Opc_Movl_Reg_Rm);
            Gen_Mod_Rm (To_Reg32 (Reg, Sz_32h) * 8);
            End_Insn;
         when others =>
            Error_Emit ("gen_conv_fp", Stmt);
      end case;
   end Gen_Conv_Fp;

   procedure Gen_Grp1_Insn_Mode (Stmt : O_Enode; Cl : Byte; Ch : Byte) is
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_U32
           | Mode_I32
           | Mode_P32 =>
            Gen_Grp1_Insn (Cl, Stmt, Sz_32l);
         when Mode_I64
           | Mode_U64 =>
            Gen_Grp1_Insn (Cl, Stmt, Sz_32l);
            Gen_Grp1_Insn (Ch, Stmt, Sz_32h);
         when Mode_B2
           | Mode_I8
           | Mode_U8 =>
            Gen_Grp1_Insn (Cl, Stmt, Sz_8);
         when others =>
            Error_Emit ("gen_grp1_insn_mode", Stmt);
      end case;
   end Gen_Grp1_Insn_Mode;

   procedure Gen_Check_Overflow (Mode : Mode_Type) is
   begin
      case Mode is
         when Mode_I32
           | Mode_I64
           | Mode_I8 =>
            Gen_Into;
         when Mode_U64
           | Mode_U32
           | Mode_U8 =>
            --  FIXME: check no carry.
            null;
         when Mode_B2 =>
            null;
         when others =>
            raise Program_Error;
      end case;
   end Gen_Check_Overflow;

   procedure Gen_Emit_Fp_Op (Stmt : O_Enode; B_St1 : Byte; B_Mem : Byte)
   is
      Right : O_Enode;
      Reg : O_Reg;
      B_Size : Byte;
   begin
      Right := Get_Expr_Right (Stmt);
      Reg := Get_Expr_Reg (Right);
      Start_Insn;
      case Reg is
         when R_St0 =>
            Gen_B8 (2#11011_110#);
            Gen_B8 (2#11_000_001# or B_St1);
         when R_Mem =>
            case Get_Expr_Mode (Stmt) is
               when Mode_F32 =>
                  B_Size := 0;
               when Mode_F64 =>
                  B_Size := 2#100#;
               when others =>
                  raise Program_Error;
            end case;
            Gen_B8 (2#11011_000# or B_Size);
            Init_Modrm_Mem (Right, Sz_32l);
            Gen_Mod_Rm (B_Mem);
         when others =>
            raise Program_Error;
      end case;
      End_Insn;
   end Gen_Emit_Fp_Op;

   procedure Gen_Emit_Fp_Or_Xmm_Op
     (Stmt : O_Enode; B_St1 : Byte; B_Mem : Byte; Xmm_Op : Byte)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      if Reg in Regs_Xmm then
         declare
            Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
            Right : constant O_Enode := Get_Expr_Right (Stmt);
         begin
            Init_Rm_Expr (Right, Sz_32l);
            Gen_Xmm_Modrm (Mode, Xmm_Op, Reg);
         end;
      else
         Gen_Emit_Fp_Op (Stmt, B_St1, B_Mem);
      end if;
   end Gen_Emit_Fp_Or_Xmm_Op;

   procedure Emit_Mod (Stmt : O_Enode)
   is
      Right : O_Enode;
      Pc1, Pc2, Pc3: Pc_Type;
   begin
      --  a : EAX
      --  d : EDX
      --  b : Rm

      --  d := Rm
      --  d := d ^ a
      --  cltd
      --  if cc < 0 then
      --    idiv b
      --    if edx /= 0 then
      --      edx := edx + b
      --    end if
      --  else
      --    idiv b
      --  end if
      Right := Get_Expr_Right (Stmt);
      --  %edx <- right
      Emit_Load (R_Dx, Right, Sz_32l);
      --  xorl %eax -> %edx
      Start_Insn;
      Gen_B8 (Opc_Xorl_Rm_Reg);
      Gen_B8 (2#11_000_010#);
      End_Insn;
      Gen_Cdq;
      --  js
      Gen_2 (Opc_Jcc + 2#1000#, 0);
      Pc1 := Get_Current_Pc;
      --  idiv
      Gen_Grp3_Insn (Opc2_Grp3_Idiv, Right, Sz_32l);
      --  jmp
      Gen_2 (Opc_Jmp_Short, 0);
      Pc2 := Get_Current_Pc;
      Patch_B8 (Pc1 - 1, Unsigned_8 (Get_Current_Pc - Pc1));
      --  idiv
      Gen_Grp3_Insn (Opc2_Grp3_Idiv, Right, Sz_32l);
      --  tstl %edx,%edx
      Gen_2 (Opc_Test_Rm_Reg + 1, 2#11_010_010#);
      --  jz
      Gen_2 (Opc_Jcc + 2#0100#, 0);
      Pc3 := Get_Current_Pc;
      --  addl b, %edx
      Start_Insn;
      Gen_B8 (Opc_Addl_Reg_Rm);
      Gen_Rm (2#010_000#, Right, Sz_32l);
      End_Insn;
      Patch_B8 (Pc2 - 1, Unsigned_8 (Get_Current_Pc - Pc2));
      Patch_B8 (Pc3 - 1, Unsigned_8 (Get_Current_Pc - Pc3));
   end Emit_Mod;

   procedure Emit_Insn (Stmt : O_Enode)
   is
      use Ortho_Code.Flags;
      Kind : constant OE_Kind := Get_Expr_Kind (Stmt);
      Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
      Reg : O_Reg;
   begin
      case Kind is
         when OE_Beg =>
            if Flag_Debug /= Debug_None then
               Decls.Set_Block_Info1 (Get_Block_Decls (Stmt),
                                      Int32 (Get_Current_Pc - Subprg_Pc));
            end if;
         when OE_End =>
            if Flag_Debug /= Debug_None then
               Decls.Set_Block_Info2 (Get_Block_Decls (Get_End_Beg (Stmt)),
                                      Int32 (Get_Current_Pc - Subprg_Pc));
            end if;
         when OE_Leave =>
            null;
         when OE_BB =>
            null;
         when OE_Add_Ov =>
            if Mode in Mode_Fp then
               Gen_Emit_Fp_Or_Xmm_Op (Stmt, 2#000_000#, 2#000_000#, 16#58#);
            else
               Gen_Grp1_Insn_Mode (Stmt, Opc2_Grp1_Add, Opc2_Grp1_Adc);
               Gen_Check_Overflow (Mode);
            end if;
         when OE_Or =>
            Gen_Grp1_Insn_Mode (Stmt, Opc2_Grp1_Or, Opc2_Grp1_Or);
         when OE_And =>
            Gen_Grp1_Insn_Mode (Stmt, Opc2_Grp1_And, Opc2_Grp1_And);
         when OE_Xor =>
            Gen_Grp1_Insn_Mode (Stmt, Opc2_Grp1_Xor, Opc2_Grp1_Xor);
         when OE_Sub_Ov =>
            if Mode in Mode_Fp then
               Gen_Emit_Fp_Or_Xmm_Op (Stmt, 2#100_000#, 2#100_000#, 16#5c#);
            else
               Gen_Grp1_Insn_Mode (Stmt, Opc2_Grp1_Sub, Opc2_Grp1_Sbb);
               Gen_Check_Overflow (Mode);
            end if;
         when OE_Mul_Ov
           | OE_Mul =>
            case Mode is
               when Mode_U8 =>
                  Gen_Umul (Stmt, Sz_8);
               when Mode_U16 =>
                  Gen_Umul (Stmt, Sz_16);
               when Mode_U32 =>
                  Gen_Mul (Stmt, Sz_32l);
               when Mode_I32 =>
                  Gen_Grp3_Insn (Opc2_Grp3_Imul,
                                 Get_Expr_Right (Stmt), Sz_32l);
               when Mode_F32
                 | Mode_F64 =>
                  Gen_Emit_Fp_Or_Xmm_Op (Stmt, 2#001_000#, 2#001_000#, 16#59#);
               when others =>
                  Error_Emit ("emit_insn: mul_ov", Stmt);
            end case;
         when OE_Shl =>
            declare
               Right : O_Enode;
               Sz : Insn_Size;
               Val : Uns32;
            begin
               case Mode is
                  when Mode_U32 =>
                     Sz := Sz_32l;
                  when others =>
                     Error_Emit ("emit_insn: shl", Stmt);
               end case;
               Right := Get_Expr_Right (Stmt);
               if Get_Expr_Kind (Right) = OE_Const then
                  Val := Get_Expr_Low (Right);
                  Start_Insn;
                  if Val = 1 then
                     Gen_Insn_Sz (2#1101000_0#, Sz);
                     Gen_Rm (2#100_000#, Get_Expr_Left (Stmt), Sz);
                  else
                     Gen_Insn_Sz (2#1100000_0#, Sz);
                     Gen_Rm (2#100_000#, Get_Expr_Left (Stmt), Sz);
                     Gen_B8 (Byte (Val and 31));
                  end if;
                  End_Insn;
               else
                  pragma Assert (Get_Expr_Reg (Right) = R_Cx);
                  Start_Insn;
                  Gen_Insn_Sz (2#1101001_0#, Sz);
                  Gen_Rm (2#100_000#, Get_Expr_Left (Stmt), Sz);
                  End_Insn;
               end if;
            end;
         when OE_Mod
           | OE_Rem
           | OE_Div_Ov =>
            case Mode is
               when Mode_U32 =>
                  Gen_Clear_Edx;
                  Gen_Grp3_Insn (Opc2_Grp3_Div, Get_Expr_Right (Stmt), Sz_32l);
               when Mode_I32 =>
                  if Kind = OE_Mod then
                     Emit_Mod (Stmt);
                  else
                     Gen_Cdq;
                     Gen_Grp3_Insn
                       (Opc2_Grp3_Idiv, Get_Expr_Right (Stmt), Sz_32l);
                  end if;
               when Mode_F32
                 | Mode_F64 =>
                  --  No Mod or Rem for fp types.
                  pragma Assert (Kind = OE_Div_Ov);
                  Gen_Emit_Fp_Or_Xmm_Op
                    (Stmt, 2#111_000#, 2#110_000#, 16#5e#);
               when others =>
                  Error_Emit ("emit_insn: mod_ov", Stmt);
            end case;

         when OE_Not =>
            case Mode is
               when Mode_B2 =>
                  --  Xor VAL, $1
                  Start_Insn;
                  Gen_B8 (Opc_Grp1v_Rm_Imm8);
                  Gen_Rm (Opc2_Grp1_Xor, Stmt, Sz_8);
                  Gen_B8 (16#01#);
                  End_Insn;
               when Mode_U8 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_8);
               when Mode_U16 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_16);
               when Mode_U32 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_32l);
               when Mode_U64 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_32l);
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_32h);
               when others =>
                  Error_Emit ("emit_insn: not", Stmt);
            end case;

         when OE_Neg_Ov =>
            case Mode is
               when Mode_I8 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_8);
                  --Gen_Into;
               when Mode_I16 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_16);
                  --Gen_Into;
               when Mode_I32 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_32l);
                  --Gen_Into;
               when Mode_I64 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_32l);
                  -- adcl 0, high
                  Start_Insn;
                  Gen_B8 (Opc_Grp1v_Rm_Imm8);
                  Gen_Rm (Opc2_Grp1_Adc, Get_Expr_Operand (Stmt), Sz_32h);
                  Gen_B8 (0);
                  End_Insn;
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_32h);
                  --Gen_Into;
               when Mode_F32
                 | Mode_F64 =>
                  Reg := Get_Expr_Reg (Stmt);
                  if Reg in Regs_Xmm then
                     --  Xorp{sd} reg, cst
                     Init_Modrm_Sym (Get_Xmm_Sign_Constant (Mode), Sz_32l);
                     Start_Insn;
                     Gen_SSE_D16_Opc (Mode, Opc2_0f_Xorp);
                     Gen_Mod_Rm (Xmm_To_Modrm_Reg (Reg));
                     End_Insn;
                  else
                     --  fchs
                     Gen_2 (2#11011_001#, 2#1110_0000#);
                  end if;
               when others =>
                  Error_Emit ("emit_insn: neg_ov", Stmt);
            end case;

         when OE_Abs_Ov =>
            case Mode is
               when Mode_I32
                  | Mode_I64 =>
                  Emit_Abs (Get_Expr_Operand (Stmt), Mode);
               when Mode_F32
                 | Mode_F64 =>
                  Reg := Get_Expr_Reg (Stmt);
                  if Reg in Regs_Xmm then
                     --  Andnp{sd} reg, cst
                     Init_Modrm_Sym (Get_Xmm_Sign_Constant (Mode), Sz_32l);
                     Start_Insn;
                     Gen_SSE_D16_Opc (Mode, Opc2_0f_Andnp);
                     Gen_Mod_Rm (Xmm_To_Modrm_Reg (Reg));
                     End_Insn;
                  else
                     --  fabs
                     Gen_2 (2#11011_001#, 2#1110_0001#);
                  end if;
               when others =>
                  Error_Emit ("emit_insn: abs_ov", Stmt);
            end case;

         when OE_Kind_Cmp =>
            case Get_Expr_Mode (Get_Expr_Left (Stmt)) is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32l);
               when Mode_B2
                 | Mode_I8
                 | Mode_U8 =>
                  Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_8);
               when Mode_U64 =>
                  declare
                     Pc : Pc_Type;
                  begin
                     Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32h);
                     --  jne
                     Start_Insn;
                     Gen_B8 (Opc_Jcc + 2#0101#);
                     Gen_B8 (0);
                     End_Insn;
                     Pc := Get_Current_Pc;
                     Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32l);
                     Patch_B8 (Pc - 1, Unsigned_8 (Get_Current_Pc - Pc));
                  end;
               when Mode_I64 =>
                  declare
                     Pc : Pc_Type;
                  begin
                     Reg := Get_Expr_Reg (Stmt);
                     Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32h);
                     --  Note: this does not clobber a reg due to care in
                     --  insns.
                     Emit_Setcc_Reg (Reg, Insns.Ekind_Signed_To_Cc (Kind));
                     --  jne
                     Start_Insn;
                     Gen_B8 (Opc_Jcc + 2#0101#);
                     Gen_B8 (0);
                     End_Insn;
                     Pc := Get_Current_Pc;
                     Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32l);
                     Emit_Setcc_Reg (Reg, Insns.Ekind_Unsigned_To_Cc (Kind));
                     Patch_B8 (Pc - 1, Unsigned_8 (Get_Current_Pc - Pc));
                     return;
                  end;
               when Mode_F32
                 | Mode_F64 =>
                  --  fcomip st, st(1)
                  Start_Insn;
                  Gen_B8 (2#11011_111#);
                  Gen_B8 (2#1111_0001#);
                  End_Insn;
                  --  fstp st, st (0)
                  Start_Insn;
                  Gen_B8 (2#11011_101#);
                  Gen_B8 (2#11_011_000#);
                  End_Insn;
               when others =>
                  Error_Emit ("emit_insn: cmp", Stmt);
            end case;
            Reg := Get_Expr_Reg (Stmt);
            if Reg not in Regs_Cc then
               Error_Emit ("emit_insn/cmp: not cc", Stmt);
            end if;
         when OE_Const
           | OE_Addrg =>
            case Mode is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Load_Imm (Stmt, Sz_32l);
               when Mode_B2
                 | Mode_U8
                 | Mode_I8 =>
                  Emit_Load_Imm (Stmt, Sz_8);
               when Mode_I64
                 | Mode_U64 =>
                  Emit_Load_Imm (Stmt, Sz_32l);
                  Emit_Load_Imm (Stmt, Sz_32h);
               when Mode_Fp =>
                  Emit_Load_Fp (Stmt, Mode);
               when others =>
                  Error_Emit ("emit_insn: const", Stmt);
            end case;
         when OE_Indir =>
            case Mode is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Load_Mem (Stmt, Sz_32l);
               when Mode_B2
                 | Mode_U8
                 | Mode_I8 =>
                  Emit_Load_Mem (Stmt, Sz_8);
               when Mode_U64
                 | Mode_I64 =>
                  Emit_Load_Mem (Stmt, Sz_32l);
                  Emit_Load_Mem (Stmt, Sz_32h);
               when Mode_Fp =>
                  Emit_Load_Fp_Mem (Stmt, Mode);
               when others =>
                  Error_Emit ("emit_insn: indir", Stmt);
            end case;

         when OE_Conv =>
            --  Call Gen_Conv_FROM
            case Get_Expr_Mode (Get_Expr_Operand (Stmt)) is
               when Mode_U32 =>
                  Gen_Conv_U32 (Stmt);
               when Mode_I32 =>
                  Gen_Conv_I32 (Stmt);
               when Mode_U8 =>
                  Gen_Conv_U8 (Stmt);
               when Mode_B2 =>
                  Gen_Conv_B2 (Stmt);
               when Mode_I64 =>
                  Gen_Conv_I64 (Stmt);
               when Mode_F32
                 | Mode_F64 =>
                  Gen_Conv_Fp (Stmt);
               when others =>
                  Error_Emit ("emit_insn: conv", Stmt);
            end case;

         when OE_Asgn =>
            case Mode is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Store (Stmt, Sz_32l);
               when Mode_B2
                 | Mode_U8
                 | Mode_I8 =>
                  Emit_Store (Stmt, Sz_8);
               when Mode_U64
                 | Mode_I64 =>
                  Emit_Store (Stmt, Sz_32l);
                  Emit_Store (Stmt, Sz_32h);
               when Mode_Fp =>
                  Emit_Store_Fp (Stmt, Mode);
               when others =>
                  Error_Emit ("emit_insn: move", Stmt);
            end case;

         when OE_Jump_F =>
            Reg := Get_Expr_Reg (Get_Expr_Operand (Stmt));
            if Reg not in Regs_Cc then
               Error_Emit ("emit_insn/jmp_f: not cc", Stmt);
            end if;
            Emit_Jmp_T (Stmt, Inverse_Cc (Reg));
         when OE_Jump_T =>
            Reg := Get_Expr_Reg (Get_Expr_Operand (Stmt));
            if Reg not in Regs_Cc then
               Error_Emit ("emit_insn/jmp_t: not cc", Stmt);
            end if;
            Emit_Jmp_T (Stmt, Reg);
         when OE_Jump =>
            Emit_Jmp (Stmt);
         when OE_Label =>
            Emit_Label (Stmt);

         when OE_Ret =>
            --  Value already set.
            null;

         when OE_Arg =>
            case Mode is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Push_32 (Get_Expr_Operand (Stmt), Sz_32l);
               when Mode_U64
                 | Mode_I64 =>
                  Emit_Push_32 (Get_Expr_Operand (Stmt), Sz_32h);
                  Emit_Push_32 (Get_Expr_Operand (Stmt), Sz_32l);
               when Mode_Fp =>
                  Emit_Push_Fp (Get_Expr_Operand (Stmt), Mode);
               when others =>
                  Error_Emit ("emit_insn: oe_arg", Stmt);
            end case;
         when OE_Stack_Adjust =>
            Emit_Stack_Adjust (Stmt);
         when OE_Call =>
            Emit_Call (Stmt);
         when OE_Intrinsic =>
            Emit_Intrinsic (Stmt);

         when OE_Move =>
            declare
               Operand : constant O_Enode := Get_Expr_Operand (Stmt);
               Op_Reg : constant O_Reg := Get_Expr_Reg (Operand);
            begin
               Reg := Get_Expr_Reg (Stmt);
               case Mode is
                  when Mode_B2 =>
                     if Reg in Regs_R32 and then Op_Reg in Regs_Cc then
                        Emit_Setcc (Stmt, Op_Reg);
                     elsif (Reg = R_Eq or Reg = R_Ne)
                       and then Op_Reg in Regs_R32
                     then
                        Emit_Tst (Op_Reg, Sz_8);
                     else
                        Error_Emit ("emit_insn: move/b2", Stmt);
                     end if;
                  when Mode_U32
                    | Mode_I32 =>
                     --  mov REG, OP
                     Start_Insn;
                     Gen_Insn_Sz (Opc_Mov_Reg_Rm, Sz_32l);
                     Gen_Rm (To_Reg32 (Reg, Sz_32l) * 8, Operand, Sz_32l);
                     End_Insn;
                  when others =>
                     Error_Emit ("emit_insn: move", Stmt);
               end case;
            end;

         when OE_Alloca =>
            pragma Assert (Mode = Mode_P32);
            Gen_Alloca (Stmt);

         when OE_Set_Stack =>
            Emit_Load_Mem (Stmt, Sz_32l);

         when OE_Add
           | OE_Addrl =>
            case Mode is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Lea (Stmt);
               when others =>
                  Error_Emit ("emit_insn: oe_add", Stmt);
            end case;

         when OE_Spill =>
            case Mode is
               when Mode_B2
                 | Mode_U8
                 | Mode_I8 =>
                  Emit_Spill (Stmt, Sz_8);
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Spill (Stmt, Sz_32l);
               when Mode_U64
                 | Mode_I64 =>
                  Emit_Spill (Stmt, Sz_32l);
                  Emit_Spill (Stmt, Sz_32h);
               when Mode_F32
                 | Mode_F64 =>
                  Reg := Get_Expr_Reg (Stmt);
                  pragma Assert (Reg in Regs_Xmm);
                  --  movsd
                  Init_Modrm_Mem (Stmt, Sz_32l);
                  Start_Insn;
                  Gen_SSE_Rep_Opc (Mode_F64, 16#11#);
                  Gen_Mod_Rm (To_Reg_Xmm (Reg) * 8);
                  End_Insn;
               when others =>
                  Error_Emit ("emit_insn: spill", Stmt);
            end case;

         when OE_Reload =>
            declare
               Expr : constant O_Enode := Get_Expr_Operand (Stmt);
            begin
               Reg := Get_Expr_Reg (Stmt);
               case Mode is
                  when Mode_B2
                    | Mode_U8
                    | Mode_I8 =>
                     Emit_Load (Reg, Expr, Sz_8);
                  when Mode_U32
                    | Mode_I32
                    | Mode_P32 =>
                     Emit_Load (Reg, Expr, Sz_32l);
                  when Mode_U64
                    | Mode_I64 =>
                     Emit_Load (Reg, Expr, Sz_32l);
                     Emit_Load (Reg, Expr, Sz_32h);
                  when Mode_F32
                    | Mode_F64 =>
                     pragma Assert (Reg in Regs_Xmm);
                     --  movsd
                     Init_Modrm_Mem (Expr, Sz_32l);
                     Start_Insn;
                     Gen_SSE_Rep_Opc (Mode_F64, 16#10#);
                     Gen_Mod_Rm (To_Reg_Xmm (Reg) * 8);
                     End_Insn;
                  when others =>
                     Error_Emit ("emit_insn: reload", Stmt);
               end case;
            end;

         when OE_Reg =>
            Reg_Helper := Get_Expr_Reg (Stmt);

         when OE_Case_Expr
           | OE_Case =>
            null;

         when OE_Line =>
            if Flag_Debug = Debug_Dwarf then
               Dwarf.Set_Line_Stmt (Get_Expr_Line_Number (Stmt));
               Set_Current_Section (Sect_Text);
            end if;
         when others =>
            Error_Emit ("cannot handle insn", Stmt);
      end case;
   end Emit_Insn;

   procedure Push_Reg_If_Used (Reg : Regs_R32)
   is
      use Ortho_Code.X86.Insns;
   begin
      if Reg_Used (Reg) then
         Gen_1 (Opc_Push_Reg + To_Reg32 (Reg, Sz_32l));
      end if;
   end Push_Reg_If_Used;

   procedure Pop_Reg_If_Used (Reg : Regs_R32)
   is
      use Ortho_Code.X86.Insns;
   begin
      if Reg_Used (Reg) then
         Gen_1 (Opc_Pop_Reg + To_Reg32 (Reg, Sz_32l));
      end if;
   end Pop_Reg_If_Used;

   procedure Emit_Prologue (Subprg : Subprogram_Data_Acc)
   is
      use Ortho_Code.Decls;
      use Ortho_Code.Flags;
      use Ortho_Code.X86.Insns;
      Sym : Symbol;
      Subprg_Decl : O_Dnode;
      Is_Global : Boolean;
      Frame_Size : Unsigned_32;
      Saved_Regs_Size : Unsigned_32;
   begin
      --  Switch to .text section and align the function (to avoid the nested
      --  function trick and for performance).
      Set_Current_Section (Sect_Text);
      Gen_Pow_Align (2);

      Subprg_Decl := Subprg.D_Decl;
      Sym := Get_Decl_Symbol (Subprg_Decl);
      case Get_Decl_Storage (Subprg_Decl) is
         when O_Storage_Public
           | O_Storage_External =>
            --  FIXME: should not accept the external case.
            Is_Global := True;
         when others =>
            Is_Global := False;
      end case;
      Set_Symbol_Pc (Sym, Is_Global);
      Subprg_Pc := Get_Current_Pc;

      Saved_Regs_Size := Boolean'Pos (Reg_Used (R_Di)) * 4
        + Boolean'Pos (Reg_Used (R_Si)) * 4
        + Boolean'Pos (Reg_Used (R_Bx)) * 4;

      --  Compute frame size.
      --  8 bytes are used by return address and saved frame pointer.
      Frame_Size := Unsigned_32 (Subprg.Stack_Max) + 8 + Saved_Regs_Size;
      --  Align.
      Frame_Size := (Frame_Size + X86.Flags.Stack_Boundary - 1)
        and not (X86.Flags.Stack_Boundary - 1);
      --  The 8 bytes are already allocated.
      Frame_Size := Frame_Size - 8 - Saved_Regs_Size;

      --  Emit prolog.
      --  push %ebp
      Gen_1 (Opc_Push_Reg + To_Reg32 (R_Bp));
      --  movl %esp, %ebp
      Start_Insn;
      Gen_B8 (Opc_Mov_Rm_Reg + 1);
      Gen_B8 (2#11_100_101#);
      End_Insn;
      --  subl XXX, %esp
      if Frame_Size /= 0 then
         if not X86.Flags.Flag_Alloca_Call
            or else Frame_Size <= 4096
         then
            Init_Modrm_Reg (R_Sp, Sz_32l);
            Gen_Insn_Grp1 (Opc2_Grp1_Sub, Sz_32l, Int32 (Frame_Size));
         else
            --  mov stack_size,%eax
            Start_Insn;
            Gen_B8 (Opc_Movl_Imm_Reg + To_Reg32 (R_Ax));
            Gen_Le32 (Frame_Size);
            End_Insn;
            Gen_Call (Chkstk_Symbol);
         end if;
      end if;

      if Flag_Profile then
         Gen_Call (Mcount_Symbol);
      end if;

      --  Save registers.
      Push_Reg_If_Used (R_Di);
      Push_Reg_If_Used (R_Si);
      Push_Reg_If_Used (R_Bx);
   end Emit_Prologue;

   procedure Emit_Epilogue (Subprg : Subprogram_Data_Acc)
   is
      use Ortho_Code.Decls;
      use Ortho_Code.Types;
      use Ortho_Code.Flags;
      Decl : O_Dnode;
      Mode : Mode_Type;
   begin
      --  Restore registers.
      Pop_Reg_If_Used (R_Bx);
      Pop_Reg_If_Used (R_Si);
      Pop_Reg_If_Used (R_Di);

      Decl := Subprg.D_Decl;
      if Get_Decl_Kind (Decl) = OD_Function then
         Mode := Get_Type_Mode (Get_Decl_Type (Decl));
         case Mode is
            when Mode_U8
              | Mode_B2 =>
               --  movzx %al,%eax
               Start_Insn;
               Gen_B8 (Opc_0f);
               Gen_B8 (Opc2_0f_Movzx);
               Gen_B8 (2#11_000_000#);
               End_Insn;
            when Mode_U32
              | Mode_I32
              | Mode_U64
              | Mode_I64
              | Mode_P32 =>
               null;
            when  Mode_F32
              | Mode_F64 =>
               if Abi.Flag_Sse2 then
                  --  movsd %xmm0, slot(%ebp)
                  Init_Modrm_Offset
                    (R_Bp, -Int32 (Cur_Subprg.Target.Fp_Slot), Sz_32l);
                  Start_Insn;
                  Gen_SSE_Rep_Opc (Mode, 16#11#);
                  Gen_Mod_Rm (2#00_000_000#);
                  End_Insn;
                  --  fldl slot(%ebp)
                  Start_Insn;
                  Gen_B8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
                  Gen_Mod_Rm (2#00_000_000#);
                  End_Insn;
               end if;
            when others =>
               raise Program_Error;
         end case;
      end if;

      --  leave; ret;
      Gen_1 (Opc_Leave);
      Gen_1 (Opc_Ret);

      if Flag_Debug = Debug_Dwarf then
         Set_Body_Info (Subprg.D_Body, Int32 (Get_Current_Pc - Subprg_Pc));
      end if;
   end Emit_Epilogue;

   procedure Emit_Subprg (Subprg : Subprogram_Data_Acc)
   is
      pragma Assert (Subprg = Cur_Subprg);
      Stmt : O_Enode;
   begin
      if Debug.Flag_Debug_Code2 then
         Abi.Disp_Subprg_Decl (Subprg.D_Decl);
      end if;

      Emit_Prologue (Subprg);

      Stmt := Subprg.E_Entry;
      loop
         Stmt := Get_Stmt_Link (Stmt);

         if Debug.Flag_Debug_Code2 then
            Abi.Disp_Stmt (Stmt);
         end if;

         Emit_Insn (Stmt);
         exit when Get_Expr_Kind (Stmt) = OE_Leave;
      end loop;

      Emit_Epilogue (Subprg);
   end Emit_Subprg;

   procedure Emit_Var_Decl (Decl : O_Dnode)
   is
      use Decls;
      use Types;
      Sym : Symbol;
      Storage : O_Storage;
      Dtype : O_Tnode;
   begin
      Set_Current_Section (Sect_Bss);
      Sym := Create_Symbol (Get_Decl_Ident (Decl));
      Set_Decl_Info (Decl, To_Int32 (Uns32 (Sym)));
      Storage := Get_Decl_Storage (Decl);
      Dtype := Get_Decl_Type (Decl);
      case Storage is
         when O_Storage_External =>
            null;
         when O_Storage_Public
           | O_Storage_Private =>
            Gen_Pow_Align (Get_Type_Align (Dtype));
            Set_Symbol_Pc (Sym, Storage = O_Storage_Public);
            Gen_Space (Integer_32 (Get_Type_Size (Dtype)));
         when O_Storage_Local =>
            raise Program_Error;
      end case;
      Set_Current_Section (Sect_Text);
   end Emit_Var_Decl;

   procedure Emit_Const_Decl (Decl : O_Dnode)
   is
      use Decls;
      use Types;
      Sym : Symbol;
   begin
      Set_Current_Section (Sect_Rodata);
      Sym := Create_Symbol (Get_Decl_Ident (Decl));
      Set_Decl_Info (Decl, To_Int32 (Uns32 (Sym)));
      Set_Current_Section (Sect_Text);
   end Emit_Const_Decl;

   procedure Emit_Const (Val : O_Cnode)
   is
      use Consts;
      use Types;
      H, L : Uns32;
   begin
      case Get_Const_Kind (Val) is
         when OC_Signed
           | OC_Unsigned
           | OC_Float
           | OC_Null
           | OC_Lit =>
            Get_Const_Bytes (Val, H, L);
            case Get_Type_Mode (Get_Const_Type (Val)) is
               when Mode_U8
                 | Mode_I8
                 | Mode_B2 =>
                  Gen_B8 (Byte (L));
               when Mode_U32
                 | Mode_I32
                 | Mode_F32
                 | Mode_P32 =>
                  Gen_Le32 (Unsigned_32 (L));
               when Mode_F64
                 | Mode_I64
                 | Mode_U64 =>
                  Gen_Le32 (Unsigned_32 (L));
                  Gen_Le32 (Unsigned_32 (H));
               when others =>
                  raise Program_Error;
            end case;
         when OC_Address
           | OC_Subprg_Address =>
            Gen_X86_32 (Get_Decl_Symbol (Get_Const_Decl (Val)), 0);
         when OC_Array =>
            for I in 0 .. Get_Const_Aggr_Length (Val) - 1 loop
               Emit_Const (Get_Const_Aggr_Element (Val, I));
            end loop;
         when OC_Record =>
            declare
               E : O_Cnode;
            begin
               for I in 0 .. Get_Const_Aggr_Length (Val) - 1 loop
                  E := Get_Const_Aggr_Element (Val, I);
                  Gen_Pow_Align (Get_Type_Align (Get_Const_Type (E)));
                  Emit_Const (E);
               end loop;
            end;
         when OC_Sizeof
           | OC_Alignof
           | OC_Union =>
            raise Program_Error;
      end case;
   end Emit_Const;

   procedure Emit_Const_Value (Decl : O_Dnode; Val : O_Cnode)
   is
      use Decls;
      use Types;
      Sym : Symbol;
      Dtype : O_Tnode;
   begin
      Set_Current_Section (Sect_Rodata);
      Sym := Get_Decl_Symbol (Decl);

      Dtype := Get_Decl_Type (Decl);
      Gen_Pow_Align (Get_Type_Align (Dtype));
      Set_Symbol_Pc (Sym, Get_Decl_Storage (Decl) = O_Storage_Public);
      Prealloc (Pc_Type (Get_Type_Size (Dtype)));
      Emit_Const (Val);

      Set_Current_Section (Sect_Text);
   end Emit_Const_Value;

   procedure Init
   is
      use Ortho_Ident;
      use Ortho_Code.Flags;
   begin
      Arch := Arch_X86;

      Create_Section (Sect_Text, ".text", Section_Exec + Section_Read);
      Create_Section (Sect_Rodata, ".rodata", Section_Read);
      Create_Section (Sect_Bss, ".bss",
                      Section_Read + Section_Write + Section_Zero);

      Set_Current_Section (Sect_Text);

      if Flag_Profile then
         Mcount_Symbol := Create_Symbol (Get_Identifier ("mcount"));
      end if;

      if X86.Flags.Flag_Alloca_Call then
         Chkstk_Symbol := Create_Symbol (Get_Identifier ("___chkstk"));
      end if;

      Intrinsics_Symbol (Intrinsic_Mul_Ov_U64) :=
        Create_Symbol (Get_Identifier ("__muldi3"));
      Intrinsics_Symbol (Intrinsic_Div_Ov_U64) :=
        Create_Symbol (Get_Identifier ("__mcode_div_ov_u64"));
      Intrinsics_Symbol (Intrinsic_Mod_Ov_U64) :=
        Create_Symbol (Get_Identifier ("__mcode_mod_ov_u64"));
      Intrinsics_Symbol (Intrinsic_Mul_Ov_I64) :=
        Create_Symbol (Get_Identifier ("__muldi3"));
      Intrinsics_Symbol (Intrinsic_Div_Ov_I64) :=
        Create_Symbol (Get_Identifier ("__divdi3"));
      Intrinsics_Symbol (Intrinsic_Mod_Ov_I64) :=
        Create_Symbol (Get_Identifier ("__mcode_mod_ov_i64"));
      Intrinsics_Symbol (Intrinsic_Rem_Ov_I64) :=
        Create_Symbol (Get_Identifier ("__mcode_rem_ov_i64"));

      if Debug.Flag_Debug_Asm then
         Dump_Asm := True;
      end if;
      if Debug.Flag_Debug_Hex then
         Debug_Hex := True;
      end if;

      if Flag_Debug = Debug_Dwarf then
         Dwarf.Init;
         Set_Current_Section (Sect_Text);
      end if;
   end Init;

   procedure Finish
   is
      use Ortho_Code.Flags;
   begin
      if Flag_Debug = Debug_Dwarf then
         Set_Current_Section (Sect_Text);
         Dwarf.Finish;
      end if;
   end Finish;

end Ortho_Code.X86.Emits;
