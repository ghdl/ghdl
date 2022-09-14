--  Mcode back-end for ortho - Binary X86 instructions generator.
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
   type Insn_Size is (Sz_8, Sz_16, Sz_32, Sz_32l, Sz_32h, Sz_64);

   --  Sz_64 if M64 or Sz_32
   Sz_Ptr : constant Insn_Size := Insn_Size'Val
     (Boolean'Pos (Flags.M64) * Insn_Size'Pos (Sz_64)
        + Boolean'Pos (not Flags.M64) * Insn_Size'Pos (Sz_32));

   --  For FP, size doesn't matter in modrm and SIB.  But don't emit the REX.W
   --  prefix, that's useless.
   Sz_Fp : constant Insn_Size := Sz_32;

   type Int_Mode_To_Size_Array is array (Mode_U8 .. Mode_I64) of Insn_Size;
   Int_Mode_To_Size : constant Int_Mode_To_Size_Array :=
     (Mode_U8  | Mode_I8 => Sz_8,
      Mode_U16 | Mode_I16 => Sz_16,
      Mode_U32 | Mode_I32 => Sz_32,
      Mode_U64 | Mode_I64 => Sz_64);

   --  For 64 bit to 32 bit conversion, we need an extra register.  Just before
   --  the conversion, there is an OE_Reg instruction containing the extra
   --  register.  Its value is saved here.
   Reg_Helper : O_Reg;

   Subprg_Pc : Pc_Type;

   --  First byte in .xdata (for Win64)
   Xdata_Sym : Symbol;
   --  Last entry in .xdata (for Win64)
   Last_Unwind_Off : Pc_Type;

   --  x86 opcodes.
   Opc_Data16 : constant := 16#66#;
--   Opc_Rex    : constant := 16#40#;
   Opc_Rex_W  : constant := 16#48#;
   Opc_Rex_R  : constant := 16#44#;
   Opc_Rex_X  : constant := 16#42#;
   Opc_Rex_B  : constant := 16#41#;
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
   Opc_Movsxd_Reg_Rm : constant := 16#63#;
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
   Opc2_0f_Andp    : constant := 16#54#;
   Opc2_0f_Xorp    : constant := 16#57#;
   Opc_Call        : constant := 16#e8#;
   Opc_Jmp_Long    : constant := 16#e9#;
   Opc_Jmp_Short   : constant := 16#eb#;
   Opc_Ret         : constant := 16#c3#;
   Opc_Leave       : constant := 16#c9#;
   Opc_Movsd_Xmm_M64 : constant := 16#10#;  --  Load xmm <- M64
   Opc_Movsd_M64_Xmm : constant := 16#11#;  --  Store M64 <- xmm
   Opc_Cvtsi2sd_Xmm_Rm : constant := 16#2a#;  --  Xmm <- cvt (rm)
   Opc_Cvtsd2si_Reg_Xm : constant := 16#2d#;  --  Reg <- cvt (xmm/m64)

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

   procedure Gen_Rex (B : Byte) is
   begin
      if Flags.M64 then
         Gen_8 (B);
      end if;
   end Gen_Rex;

   procedure Gen_Rex_B (R : O_Reg; Sz : Insn_Size)
   is
      B : Byte;
   begin
      if Flags.M64 then
         B := 0;
         if R in Regs_R8_R15 or R in Regs_Xmm8_Xmm15 then
            B := B or Opc_Rex_B;
         end if;
         if Sz = Sz_64 then
            B := B or Opc_Rex_W;
         end if;
         if B /= 0 then
            Gen_8 (B);
         end if;
      end if;
   end Gen_Rex_B;

   --  For many opcodes, the size of the operand is coded in bit 0, and the
   --  prefix data16 can be used for 16-bit operation.
   --  Deal with size.
   procedure Gen_Insn_Sz (B : Byte; Sz : Insn_Size) is
   begin
      case Sz is
         when Sz_8 =>
            Gen_8 (B);
         when Sz_16 =>
            Gen_8 (Opc_Data16);
            Gen_8 (B + 1);
         when Sz_32
           | Sz_32l
           | Sz_32h
           | Sz_64 =>
            Gen_8 (B + 1);
      end case;
   end Gen_Insn_Sz;

   procedure Gen_Insn_Sz_S8 (B : Byte; Sz : Insn_Size) is
   begin
      case Sz is
         when Sz_8 =>
            Gen_8 (B);
         when Sz_16 =>
            Gen_8 (Opc_Data16);
            Gen_8 (B + 3);
         when Sz_32
           | Sz_32l
           | Sz_32h
           | Sz_64 =>
            Gen_8 (B + 3);
      end case;
   end Gen_Insn_Sz_S8;

   function Get_Const_Val (C : O_Enode; Sz : Insn_Size) return Uns32 is
   begin
      case Sz is
         when Sz_8
           | Sz_16
           | Sz_32
           | Sz_32l =>
            return Get_Expr_Low (C);
         when Sz_32h =>
            return Get_Expr_High (C);
         when Sz_64 =>
            return Get_Expr_Low (C);
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
      Gen_8 (Byte (Get_Const_Val (N, Sz)));
   end Gen_Imm8;

--     procedure Gen_Imm32 (N : O_Enode; Sz : Insn_Size)
--     is
--        use Interfaces;
--     begin
--        case Get_Expr_Kind (N) is
--           when OE_Const =>
--              Gen_32 (Unsigned_32 (Get_Const_Val (N, Sz)));
--           when OE_Addrg =>
--              Gen_X86_32 (Get_Decl_Symbol (Get_Addr_Object (N)), 0);
--           when others =>
--              raise Program_Error;
--        end case;
--     end Gen_Imm32;

   --  Generate an immediat constant.
   procedure Gen_Imm_Addr (N : O_Enode)
   is
      Sym : Symbol;
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
      pragma Assert (Get_Expr_Kind (P) = OE_Addrd);
      Sym := Get_Decl_Symbol (Get_Addr_Decl (P));
      Gen_Abs (Sym, Integer_32 (Off));
   end Gen_Imm_Addr;

   --  Generate an immediat constant.
   procedure Gen_Imm (N : O_Enode; Sz : Insn_Size) is
   begin
      case Get_Expr_Kind (N) is
         when OE_Const =>
            case Sz is
               when Sz_8 =>
                  Gen_8 (Byte (Get_Expr_Low (N) and 16#FF#));
               when Sz_16 =>
                  Gen_16 (Unsigned_32 (Get_Expr_Low (N) and 16#FF_FF#));
               when Sz_32
                 | Sz_32l =>
                  Gen_32 (Unsigned_32 (Get_Expr_Low (N)));
               when Sz_32h =>
                  Gen_32 (Unsigned_32 (Get_Expr_High (N)));
               when Sz_64 =>
                  --  Immediates are sign extended.
                  pragma Assert (Is_Expr_S32 (N));
                  Gen_32 (Unsigned_32 (Get_Expr_Low (N)));
            end case;
         when OE_Add
           | OE_Addrd =>
            --  Only for 32-bit immediat.
            pragma Assert (Sz = Sz_32);
            Gen_Imm_Addr (N);
         when others =>
            raise Program_Error;
      end case;
   end Gen_Imm;

   function To_Reg32 (R : O_Reg) return Byte is
   begin
      pragma Assert (R in Regs_R32);
      return O_Reg'Pos (R) - O_Reg'Pos (R_Ax);
   end To_Reg32;
   pragma Inline (To_Reg32);

   function To_Reg64 (R : O_Reg) return Byte is
   begin
      pragma Assert (R in Regs_R64);
      return Byte (O_Reg'Pos (R) - O_Reg'Pos (R_Ax)) and 7;
   end To_Reg64;
   pragma Inline (To_Reg64);

   function To_Reg_Xmm (R : O_Reg) return Byte is
   begin
      return O_Reg'Pos (R) - O_Reg'Pos (R_Xmm0);
   end To_Reg_Xmm;
   pragma Inline (To_Reg_Xmm);

   function To_Reg32 (R : O_Reg; Sz : Insn_Size) return Byte is
   begin
      case Sz is
         when Sz_8 =>
            pragma Assert ((not Flags.M64 and R in Regs_R8)
                           or (Flags.M64 and R in Regs_R64));
            return To_Reg64 (R);
         when Sz_16 =>
            pragma Assert (R in Regs_R32);
            return To_Reg64 (R);
         when Sz_32 =>
            pragma Assert ((not Flags.M64 and R in Regs_R32)
                           or (Flags.M64 and R in Regs_R64));
            return To_Reg64 (R);
         when Sz_32l =>
            pragma Assert (not Flags.M64);
            case R is
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
            pragma Assert (not Flags.M64);
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
         when Sz_64 =>
            pragma Assert (R in Regs_R64);
            return Byte (O_Reg'Pos (R) - O_Reg'Pos (R_Ax)) and 7;
      end case;
   end To_Reg32;

   function To_Cond (R : O_Reg) return Byte is
   begin
      return O_Reg'Pos (R) - O_Reg'Pos (R_Ov);
   end To_Cond;
   pragma Inline (To_Cond);

   function To_Reg (R : O_Reg; Sz : Insn_Size) return Byte is
   begin
      if R in Regs_Xmm then
         return To_Reg_Xmm (R);
      else
         return To_Reg32 (R, Sz);
      end if;
   end To_Reg;

   --  SIB + disp values.
   SIB_Scale : Byte;
   SIB_Index : O_Reg;
   Rm_Base : O_Reg;
   Rm_Offset : Int32;
   Rm_Sym : Symbol;

   --  If not R_Nil, the reg/opc field (bit 3-5) of the ModR/M byte is a
   --  register.
   Rm_Opc_Reg : O_Reg;
   Rm_Opc_Sz : Insn_Size;

   --  If not R_Nil, encode mod=11 (no memory access).  All above variables
   --  must be 0/R_Nil.
   Rm_Reg : O_Reg;
   Rm_Sz : Insn_Size;

   procedure Gen_Rex_Mod_Rm
   is
      B : Byte;
   begin
      if Flags.M64 then
         B := 0;
         if Rm_Sz = Sz_64 then
            B := B or Opc_Rex_W;
         end if;
         if Rm_Opc_Reg in Regs_R8_R15
           or Rm_Opc_Reg in Regs_Xmm8_Xmm15
         then
            B := B or Opc_Rex_R;
         end if;
         if Rm_Reg in Regs_R8_R15
           or Rm_Reg in Regs_Xmm8_Xmm15
           or Rm_Base in Regs_R8_R15
         then
            B := B or Opc_Rex_B;
         end if;
         if SIB_Index in Regs_R8_R15 then
            B := B or Opc_Rex_X;
         end if;
         if B /= 0 then
            Gen_8 (B);
         end if;
      end if;
   end Gen_Rex_Mod_Rm;

   procedure Fill_Sib (N : O_Enode)
   is
      use Ortho_Code.Decls;
      Reg : constant O_Reg := Get_Expr_Reg (N);
   begin
      --  A simple register.
      if Reg in Regs_R64 then
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
            Rm_Offset := Rm_Offset + Get_Local_Offset (Get_Addr_Decl (N));
         when OE_Addrd =>
            --  Cannot add two symbols.
            pragma Assert (Rm_Sym = Null_Symbol);
            Rm_Sym := Get_Decl_Symbol (Get_Addr_Decl (N));
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

   --  Write the SIB byte.
   procedure Gen_Sib
   is
      Base : Byte;
   begin
      if Rm_Base = R_Nil then
         Base := 2#101#;  --  BP
      else
         pragma Assert (not (SIB_Index = R_Sp
                               and (Rm_Base = R_Bp or Rm_Base = R_R13)));
         Base := To_Reg64 (Rm_Base);
      end if;
      Gen_8
        (SIB_Scale * 2#1_000_000# + To_Reg64 (SIB_Index) * 2#1_000# + Base);
   end Gen_Sib;

   --  ModRM is a register.
   procedure Init_Modrm_Reg (Reg : O_Reg;
                             Sz : Insn_Size;
                             Opc : O_Reg := R_Nil;
                             Opc_Sz : Insn_Size := Sz_32) is
   begin
      Rm_Base := R_Nil;
      SIB_Index := R_Nil;
      SIB_Scale := 0;
      Rm_Sym := Null_Symbol;
      Rm_Offset := 0;

      Rm_Opc_Reg := Opc;
      Rm_Opc_Sz := Opc_Sz;

      Rm_Reg := Reg;
      Rm_Sz := Sz;

      Gen_Rex_Mod_Rm;
   end Init_Modrm_Reg;

   --  Note: SZ is not relevant.
   procedure Init_Modrm_Sym (Sym : Symbol; Sz : Insn_Size; Opc_Reg : O_Reg) is
   begin
      Rm_Base := R_Nil;
      SIB_Index := R_Nil;
      SIB_Scale := 0;
      Rm_Sym := Sym;
      Rm_Offset := 0;

      Rm_Opc_Reg := Opc_Reg;
      Rm_Opc_Sz := Sz;

      Rm_Reg := R_Nil;
      Rm_Sz := Sz;

      Gen_Rex_Mod_Rm;
   end Init_Modrm_Sym;

   --  ModRM is a memory reference.
   procedure Init_Modrm_Mem (N : O_Enode; Sz : Insn_Size; Opc : O_Reg := R_Nil)
   is
      Reg : constant O_Reg := Get_Expr_Reg (N);
   begin
      Rm_Base := R_Nil;
      SIB_Index := R_Nil;
      Rm_Reg := R_Nil;
      Rm_Sz := Sz;

      Rm_Opc_Reg := Opc;
      Rm_Opc_Sz := Sz;

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
         when Regs_R64 =>
            Rm_Base := Reg;
         when R_Spill =>
            Rm_Base := R_Bp;
            Rm_Offset := Rm_Offset + Get_Spill_Info (N);
         when others =>
            Error_Emit ("init_modrm_mem: unhandled reg", N);
      end case;

      Gen_Rex_Mod_Rm;
   end Init_Modrm_Mem;

   procedure Init_Modrm_Expr
     (N : O_Enode; Sz : Insn_Size; Opc : O_Reg := R_Nil)
   is
      Reg : constant O_Reg := Get_Expr_Reg (N);
   begin
      case Reg is
         when Regs_R64
           | Regs_Pair
           | Regs_Xmm =>
            --  Destination is a register.
            Init_Modrm_Reg (Reg, Sz, Opc, Sz);
         when others =>
            --  Destination is an effective address.
            Init_Modrm_Mem (N, Sz, Opc);
      end case;
   end Init_Modrm_Expr;

   procedure Init_Modrm_Offset
     (Base : O_Reg; Off : Int32; Sz : Insn_Size; Opc : O_Reg := R_Nil) is
   begin
      SIB_Index := R_Nil;
      SIB_Scale := 0;
      Rm_Reg := R_Nil;
      Rm_Sym := Null_Symbol;
      Rm_Sz := Sz;

      Rm_Base := Base;

      Rm_Opc_Reg := Opc;
      Rm_Opc_Sz := Sz;

      if Sz = Sz_32h then
         Rm_Offset := Off + 4;
      else
         Rm_Offset := Off;
      end if;

      Gen_Rex_Mod_Rm;
   end Init_Modrm_Offset;

   --  Generate an R/M (+ SIB) byte.
   --  R is added to the R/M byte.
   procedure Gen_Mod_Rm_B (R : Byte) is
   begin
      if Rm_Reg /= R_Nil then
         --  Register: mod = 11, no memory access.
         pragma Assert (Rm_Base = R_Nil);
         pragma Assert (Rm_Sym = Null_Symbol);
         pragma Assert (Rm_Offset = 0);
         pragma Assert (SIB_Index = R_Nil);
         Gen_8 (2#11_000_000# + R + To_Reg (Rm_Reg, Rm_Sz));
         return;
      end if;

      if SIB_Index /= R_Nil or (Flags.M64 and Rm_Base = R_R12) then
         --  With SIB.
         if SIB_Index = R_Nil then
            SIB_Index := R_Sp;
         end if;
         if Rm_Base = R_Nil then
            --  No base (but index).  Use the special encoding with base=BP.
            Gen_8 (2#00_000_100# + R); --  mod=00, rm=SP -> disp32.
            Rm_Base := R_Bp;
            Gen_Sib;
            if Rm_Sym = Null_Symbol then
               Gen_32 (Unsigned_32 (To_Uns32 (Rm_Offset)));
            else
               pragma Assert (not Flags.M64);
               Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
            end if;
         elsif Rm_Sym = Null_Symbol and Rm_Offset = 0
           and Rm_Base /= R_Bp and Rm_Base /= R_R13
         then
            --  No offset (only allowed if base is not BP).
            Gen_8 (2#00_000_100# + R);
            Gen_Sib;
         elsif Rm_Sym = Null_Symbol and Rm_Offset in -128 .. 127 then
            --  Disp8
            Gen_8 (2#01_000_100# + R);
            Gen_Sib;
            Gen_8 (Byte (To_Uns32 (Rm_Offset) and 16#Ff#));
         else
            --  Disp32
            Gen_8 (2#10_000_100# + R);
            Gen_Sib;
            if Rm_Sym = Null_Symbol then
               Gen_32 (Unsigned_32 (To_Uns32 (Rm_Offset)));
            else
               pragma Assert (not Flags.M64);
               Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
            end if;
         end if;
      else
         case Rm_Base is
            when R_Sp =>
               --  It isn't possible to use SP as a base register without using
               --  an SIB encoding.
               raise Program_Error;
            when R_Nil =>
               --  There should be no case where the offset is negative.
               pragma Assert (Rm_Offset >= 0);
               --  Encode for disp32 (Mod=00, R/M=101) or RIP relative
               Gen_8 (2#00_000_101# + R);
               if Flags.M64 then
                  --  RIP relative
                  Gen_X86_Pc32 (Rm_Sym, Unsigned_32 (Rm_Offset));
               else
                  --  Disp32.
                  Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
               end if;
            when R_Ax
              | R_Bx
              | R_Cx
              | R_Dx
              | R_Bp
              | R_Si
              | R_Di
              | R_R8 .. R_R11
              | R_R13 .. R_R15 =>
               if Rm_Offset = 0 and Rm_Sym = Null_Symbol
                 and Rm_Base /= R_Bp and Rm_Base /= R_R13
               then
                  --  No disp: use Mod=00 (not supported if base is BP or R13).
                  Gen_8 (2#00_000_000# + R + To_Reg64 (Rm_Base));
               elsif Rm_Sym = Null_Symbol
                 and Rm_Offset <= 127 and Rm_Offset >= -128
               then
                  --  Disp8 (Mod=01)
                  Gen_8 (2#01_000_000# + R + To_Reg64 (Rm_Base));
                  Gen_8 (Byte (To_Uns32 (Rm_Offset) and 16#Ff#));
               else
                  --  Disp32 (Mod=10)
                  Gen_8 (2#10_000_000# + R + To_Reg64 (Rm_Base));
                  if Rm_Sym = Null_Symbol then
                     Gen_32 (Unsigned_32 (To_Uns32 (Rm_Offset)));
                  else
                     pragma Assert (not Flags.M64);
                     Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
                  end if;
               end if;
            when others =>
               raise Program_Error;
         end case;
      end if;
   end Gen_Mod_Rm_B;

   procedure Gen_Mod_Rm_Opc (R : Byte) is
   begin
      pragma Assert (Rm_Opc_Reg = R_Nil);
      Gen_Mod_Rm_B (R);
   end Gen_Mod_Rm_Opc;

   procedure Gen_Mod_Rm_Reg is
   begin
      pragma Assert (Rm_Opc_Reg /= R_Nil);
      Gen_Mod_Rm_B (To_Reg (Rm_Opc_Reg, Rm_Opc_Sz) * 8);
   end Gen_Mod_Rm_Reg;

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
            if Lr = R_Ax then
               --  Use compact encoding.
               if Sz = Sz_64 then
                  Gen_8 (Opc_Rex_W);
               end if;
               Gen_Insn_Sz (2#000_000_100# + Op, Sz);
               Gen_Imm (R, Sz);
            elsif Is_Imm8 (R, Sz) then
               Init_Modrm_Expr (L, Sz);
               Gen_Insn_Sz_S8 (16#80#, Sz);
               Gen_Mod_Rm_Opc (Op);
               Gen_Imm8 (R, Sz);
            else
               Init_Modrm_Expr (L, Sz);
               Gen_Insn_Sz (16#80#, Sz);
               Gen_Mod_Rm_Opc (Op);
               Gen_Imm (R, Sz);
            end if;
         when R_Mem
           | R_Spill
           | Regs_R64
           | Regs_Pair =>
            Init_Modrm_Expr (R, Sz, Lr);
            Gen_Insn_Sz (2#00_000_010# + Op, Sz);
            Gen_Mod_Rm_Reg;
         when others =>
            Error_Emit ("emit_op", Stmt);
      end case;
      End_Insn;
   end Gen_Grp1_Insn;

   --  Emit a one byte instruction.
   procedure Gen_1 (B : Byte) is
   begin
      Start_Insn;
      Gen_8 (B);
      End_Insn;
   end Gen_1;

   --  Emit a two byte instruction.
   procedure Gen_2 (B1, B2 : Byte) is
   begin
      Start_Insn;
      Gen_8 (B1);
      Gen_8 (B2);
      End_Insn;
   end Gen_2;

   --  Grp1 instructions have a mod/rm and an immediate value VAL.
   --  Mod/Rm must be initialized.
   procedure Gen_Insn_Grp1 (Opc2 : Byte; Val : Int32) is
   begin
      if Val in -128 .. 127 then
         case Rm_Sz is
            when Sz_8 =>
               Gen_8 (Opc_Grp1b_Rm_Imm8);
            when Sz_16 =>
               Gen_8 (Opc_Data16);
               Gen_8 (Opc_Grp1v_Rm_Imm8);
            when Sz_32
              | Sz_32l
              | Sz_32h
              | Sz_64 =>
               Gen_8 (Opc_Grp1v_Rm_Imm8);
         end case;
         Gen_Mod_Rm_Opc (Opc2);
         Gen_8 (Byte (To_Uns32 (Val) and 16#Ff#));
      else
         case Rm_Sz is
            when Sz_8 =>
               pragma Assert (False);
               null;
            when Sz_16 =>
               Gen_8 (Opc_Data16);
               Gen_8 (Opc_Grp1v_Rm_Imm32);
            when Sz_32
              | Sz_32l
              | Sz_32h
              | Sz_64 =>
               Gen_8 (Opc_Grp1v_Rm_Imm32);
         end case;
         Gen_Mod_Rm_Opc (Opc2);
         Gen_32 (Unsigned_32 (To_Uns32 (Val)));
      end if;
   end Gen_Insn_Grp1;

   procedure Gen_Cdq (Sz : Insn_Size) is
   begin
      Start_Insn;
      if Sz = Sz_64 then
         Gen_8 (Opc_Rex_W);
      end if;
      Gen_8 (Opc_Cdq);
      End_Insn;
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
      Init_Modrm_Expr (Val, Sz);
      Gen_Insn_Sz (Opc_Grp3_Width, Sz);
      Gen_Mod_Rm_Opc (Op);
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
            Gen_Rex_B (Tr, Sz);
            Gen_8 (Opc_Movb_Imm_Reg + To_Reg32 (Tr, Sz));
            Gen_Imm (Stmt, Sz);
         when Sz_16 =>
            Gen_8 (Opc_Data16);
            Gen_8 (Opc_Movl_Imm_Reg + To_Reg32 (Tr, Sz));
            Gen_Imm (Stmt, Sz);
         when Sz_32
           | Sz_32l
           | Sz_32h =>
            Gen_Rex_B (Tr, Sz);
            Gen_8 (Opc_Movl_Imm_Reg + To_Reg32 (Tr, Sz));
            Gen_Imm (Stmt, Sz);
         when Sz_64 =>
            if Get_Expr_Kind (Stmt) = OE_Const then
               if Get_Expr_High (Stmt) = 0 then
                  Gen_Rex_B (Tr, Sz_32);
                  Gen_8 (Opc_Movl_Imm_Reg + To_Reg32 (Tr, Sz));
                  Gen_32 (Unsigned_32 (Get_Expr_Low (Stmt)));
               else
                  Gen_Rex_B (Tr, Sz_64);
                  Gen_8 (Opc_Movl_Imm_Reg + To_Reg32 (Tr, Sz));
                  Gen_32 (Unsigned_32 (Get_Expr_Low (Stmt)));
                  Gen_32 (Unsigned_32 (Get_Expr_High (Stmt)));
               end if;
            else
               Gen_Rex_B (Tr, Sz_64);
               Gen_8 (Opc_Movl_Imm_Reg + To_Reg32 (Tr, Sz));
               Gen_Imm_Addr (Stmt);
            end if;
      end case;
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

   subtype Nat_Align is Natural range 0 .. 4;

   function Gen_Constant_Start (Log2sz : Nat_Align) return Symbol
   is
      Sym : Symbol;
   begin
      --  Write the constant in .rodata
      Set_Current_Section (Sect_Rodata);
      Gen_Pow_Align (Log2sz);
      Prealloc (2 ** Log2sz);
      Sym := Create_Local_Symbol;
      Set_Symbol_Pc (Sym, False);
      return Sym;
   end Gen_Constant_Start;

   function Gen_Constant_32 (Val : Unsigned_32) return Symbol
   is
      Sym : Symbol;
   begin
      Sym := Gen_Constant_Start (2);
      Gen_32 (Val);
      Set_Current_Section (Sect_Text);
      return Sym;
   end Gen_Constant_32;

   function Gen_Constant_64 (Lo, Hi : Unsigned_32) return Symbol
   is
      Sym : Symbol;
   begin
      Sym := Gen_Constant_Start (3);
      Gen_32 (Lo);
      Gen_32 (Hi);
      Set_Current_Section (Sect_Text);
      return Sym;
   end Gen_Constant_64;

   function Gen_Constant_128 (Lo, Hi : Unsigned_32) return Symbol
   is
      Sym : Symbol;
   begin
      Sym := Gen_Constant_Start (4);
      Gen_32 (Lo);
      Gen_32 (Hi);
      Gen_32 (Lo);
      Gen_32 (Hi);
      Set_Current_Section (Sect_Text);
      return Sym;
   end Gen_Constant_128;

   Xmm_Sign32_Sym : Symbol := Null_Symbol;
   Xmm_Sign64_Sym : Symbol := Null_Symbol;

   function Get_Xmm_Sign_Constant (Mode : Mode_Fp) return Symbol is
   begin
      case Mode is
         when Mode_F32 =>
            if Xmm_Sign32_Sym = Null_Symbol then
               Xmm_Sign32_Sym := Gen_Constant_128
                 (16#8000_0000#, 16#8000_0000#);
            end if;
            return Xmm_Sign32_Sym;
         when Mode_F64 =>
            if Xmm_Sign64_Sym = Null_Symbol then
               Xmm_Sign64_Sym := Gen_Constant_128
                 (0, 16#8000_0000#);
            end if;
            return Xmm_Sign64_Sym;
      end case;
   end Get_Xmm_Sign_Constant;

   Xmm_Mask32_Sym : Symbol := Null_Symbol;
   Xmm_Mask64_Sym : Symbol := Null_Symbol;

   function Get_Xmm_Mask_Constant (Mode : Mode_Fp) return Symbol is
   begin
      case Mode is
         when Mode_F32 =>
            if Xmm_Mask32_Sym = Null_Symbol then
               Xmm_Mask32_Sym := Gen_Constant_128
                 (16#7fff_ffff#, 16#7fff_ffff#);
            end if;
            return Xmm_Mask32_Sym;
         when Mode_F64 =>
            if Xmm_Mask64_Sym = Null_Symbol then
               Xmm_Mask64_Sym := Gen_Constant_128
                 (16#ffff_ffff#, 16#7fff_ffff#);
            end if;
            return Xmm_Mask64_Sym;
      end case;
   end Get_Xmm_Mask_Constant;

   procedure Gen_SSE_Prefix (Mode : Mode_Fp) is
   begin
      case Mode is
         when Mode_F32 =>
            Gen_8 (16#f3#);
         when Mode_F64 =>
            Gen_8 (16#f2#);
      end case;
   end Gen_SSE_Prefix;

   procedure Gen_SSE_Opc (Op : Byte) is
   begin
      Gen_8 (16#0f#, Op);
   end Gen_SSE_Opc;

   procedure Gen_SSE_D16_Opc (Mode : Mode_Fp; Opc : Byte) is
   begin
      case Mode is
         when Mode_F32 =>
            null;
         when Mode_F64 =>
            Gen_8 (Opc_Data16);
      end case;
      Gen_8 (16#0f#);
      Gen_8 (Opc);
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
            Gen_8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
            Gen_8 (2#00_000_101#);
            Gen_X86_32 (Sym, 0);
            End_Insn;
         when Regs_Xmm =>
            Start_Insn;
            Gen_SSE_Prefix (Mode);
            Gen_SSE_Opc (Opc_Movsd_Xmm_M64);
            Gen_8 (2#00_000_101# + To_Reg_Xmm (R) * 2#1_000#);
            if Flags.M64 then
               --  RIP relative
               Gen_X86_Pc32 (Sym, 0);
            else
               --  Disp32.
               Gen_X86_32 (Sym, 0);
            end if;
            End_Insn;
         when others =>
            raise Program_Error;
      end case;
   end Emit_Load_Fp;

   procedure Emit_Load_Fp_Mem (Stmt : O_Enode; Mode : Mode_Fp)
   is
      Dest : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      if Dest in Regs_Xmm then
         Start_Insn;
         Gen_SSE_Prefix (Mode);
         Init_Modrm_Mem (Get_Expr_Operand (Stmt), Sz_Fp, Dest);
         Gen_SSE_Opc (Opc_Movsd_Xmm_M64);
         Gen_Mod_Rm_Reg;
         End_Insn;
      else
         Start_Insn;
         Init_Modrm_Mem (Get_Expr_Operand (Stmt), Sz_Fp);
         Gen_8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
         Gen_Mod_Rm_Opc (2#000_000#);
         End_Insn;
      end if;
   end Emit_Load_Fp_Mem;

   procedure Emit_Load_Mem (Stmt : O_Enode; Sz : Insn_Size)
   is
      Tr  : constant O_Reg := Get_Expr_Reg (Stmt);
      Val : constant O_Enode := Get_Expr_Operand (Stmt);
   begin
      case Tr is
         when Regs_R64
           | Regs_Pair =>
            --  mov REG, OP
            Start_Insn;
            Init_Modrm_Mem (Val, Sz, Tr);
            Gen_Insn_Sz (Opc_Mov_Reg_Rm, Sz);
            Gen_Mod_Rm_Reg;
            End_Insn;
         when R_Eq =>
            --  Cmp OP, 1
            Start_Insn;
            Init_Modrm_Mem (Val, Sz);
            Gen_Insn_Grp1 (Opc2_Grp1_Cmp, 1);
            End_Insn;
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
            if False and (Tr in Regs_R64 or Tr in Regs_Pair) then
               B := 2#1011_1_000#;
               case Sz is
                  when Sz_8 =>
                     B := B and not 2#0000_1_000#;
                  when Sz_16 =>
                     Gen_8 (16#66#);
                  when Sz_32
                    | Sz_32l
                    | Sz_32h
                    | Sz_64 =>
                     null;
               end case;
               Gen_8 (B + To_Reg32 (Tr, Sz));
            else
               Init_Modrm_Mem (T, Sz);
               Gen_Insn_Sz (Opc_Mov_Rm_Imm, Sz);
               Gen_Mod_Rm_Opc (16#00#);
            end if;
            Gen_Imm (R, Sz);
         when Regs_R64
           | Regs_Pair =>
            Init_Modrm_Mem (T, Sz, Rr);
            Gen_Insn_Sz (Opc_Mov_Rm_Reg, Sz);
            Gen_Mod_Rm_Reg;
         when others =>
            Error_Emit ("emit_store", Stmt);
      end case;
      End_Insn;
   end Emit_Store;

   procedure Emit_Store_Fp (Stmt : O_Enode; Mode : Mode_Fp) is
   begin
      -- fstp
      Start_Insn;
      Init_Modrm_Mem (Get_Assign_Target (Stmt), Sz_Ptr);
      Gen_8 (2#11011_00_1# + Mode_Fp_To_Mf (Mode));
      Gen_Mod_Rm_Opc (2#011_000#);
      End_Insn;
   end Emit_Store_Fp;

   procedure Emit_Store_Xmm (Stmt : O_Enode; Mode : Mode_Fp) is
   begin
      --  movsd
      Start_Insn;
      Gen_SSE_Prefix (Mode);
      Init_Modrm_Mem (Get_Assign_Target (Stmt), Sz_Fp,
                      Get_Expr_Reg (Get_Expr_Operand (Stmt)));
      Gen_SSE_Opc (Opc_Movsd_M64_Xmm);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Emit_Store_Xmm;

   procedure Gen_Push_Pop_Reg (Opc : Byte; Reg : O_Reg; Sz : Insn_Size) is
   begin
      Start_Insn;
      if Reg in Regs_R8_R15 then
         Gen_8 (Opc_Rex_B);
      end if;
      Gen_8 (Opc + To_Reg32 (Reg, Sz));
      End_Insn;
   end Gen_Push_Pop_Reg;

   procedure Emit_Push (Val : O_Enode; Sz : Insn_Size)
   is
      R : constant O_Reg := Get_Expr_Reg (Val);
   begin
      case R is
         when R_Imm =>
            Start_Insn;
            if Is_Imm8 (Val, Sz) then
               Gen_8 (Opc_Push_Imm8);
               Gen_Imm8 (Val, Sz);
            else
               Gen_8 (Opc_Push_Imm);
               Gen_Imm (Val, Sz);
            end if;
            End_Insn;
         when Regs_R64
           | Regs_Pair =>
            Gen_Push_Pop_Reg (Opc_Push_Reg, R, Sz);
         when others =>
            Start_Insn;
            Init_Modrm_Expr (Val, Sz);
            Gen_8 (Opc_Grp5);
            Gen_Mod_Rm_Opc (Opc2_Grp5_Push_Rm);
            End_Insn;
      end case;
   end Emit_Push;

   procedure Emit_Subl_Sp_Imm (Len : Byte) is
   begin
      Start_Insn;
      Gen_Rex (Opc_Rex_W);
      Gen_8 (Opc_Grp1v_Rm_Imm8);
      Gen_8 (Opc2_Grp1_Sub + 2#11_000_100#);
      Gen_8 (Len);
      End_Insn;
   end Emit_Subl_Sp_Imm;

   procedure Emit_Addl_Sp_Imm (Len : Byte)
   is
      pragma Assert (not Flags.M64);
   begin
      Start_Insn;
      Gen_8 (Opc_Grp1v_Rm_Imm8);
      Gen_8 (Opc2_Grp1_Add + 2#11_000_100#);
      Gen_8 (Len);
      End_Insn;
   end Emit_Addl_Sp_Imm;

   procedure Gen_Sub_Sp_Imm (Imm : Int32) is
   begin
      Start_Insn;
      Init_Modrm_Reg (R_Sp, Sz_Ptr);
      Gen_Insn_Grp1 (Opc2_Grp1_Sub, Imm);
      End_Insn;
   end Gen_Sub_Sp_Imm;

   procedure Gen_Sub_Sp_Reg (Reg : O_Reg) is
   begin
      --  subl esp, reg
      Start_Insn;
      Gen_Rex_B (Reg, Sz_Ptr);
      Gen_8 (Opc_Subl_Reg_Rm);
      Gen_8 (2#11_100_000# + To_Reg32 (Reg));
      End_Insn;
   end Gen_Sub_Sp_Reg;

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
         Gen_8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
         Gen_8 (2#00_011_100#);  --  Modrm: SIB, no disp
         Gen_8 (2#00_100_100#);  --  SIB: SS=0, no index, base=esp
         End_Insn;
      else
         pragma Assert (Reg in Regs_Xmm);
         Start_Insn;
         Gen_SSE_Prefix (Mode);
         Gen_SSE_Opc (Opc_Movsd_M64_Xmm);
         Gen_8 (To_Reg_Xmm (Reg) * 8 + 2#00_000_100#);  --  Modrm: [--]
         Gen_8 (2#00_100_100#);  --  SIB: SS=0, no index, base=esp
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
         Gen_8 (Opc_0f);
         Gen_8 (Opc2_0f_Jcc + Opc);
         Gen_X86_Pc32 (Sym, 0);
      else
         if Val + 128 < Get_Current_Pc + 4 then
            --  Long jmp.
            Gen_8 (Opc_0f);
            Gen_8 (Opc2_0f_Jcc + Opc);
            Gen_32 (To_Unsigned_32 (Val - (Get_Current_Pc + 4)));
         else
            --  short jmp.
            Gen_8 (Opc_Jcc + Opc);
            Gen_8 (Byte (Val - (Get_Current_Pc + 1)));
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
         Gen_8 (Opc_Jmp_Long);
         Gen_X86_Pc32 (Sym, 0);
      else
         if Val + 128 < Get_Current_Pc + 4 then
            --  Long jmp.
            Gen_8 (Opc_Jmp_Long);
            Gen_32 (To_Unsigned_32 (Val - (Get_Current_Pc + 4)));
         else
            --  short jmp.
            Gen_8 (Opc_Jmp_Short);
            Gen_8 (Byte ((Val - (Get_Current_Pc + 1)) and 16#Ff#));
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
      Gen_8 (Opc_Call);
      Gen_X86_Pc32 (Sym, 0);
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
         Start_Insn;
         Init_Modrm_Reg (R_Sp, Sz_Ptr);
         Gen_Insn_Grp1 (Opc2_Grp1_Add, -Val);
         End_Insn;
      end if;
   end Emit_Stack_Adjust;

   procedure Emit_Call (Stmt : O_Enode)
   is
      Subprg : constant O_Dnode := Get_Call_Subprg (Stmt);
      Sym : constant Symbol := Get_Decl_Symbol (Subprg);
      Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
   begin
      Gen_Call (Sym);

      if Abi.Flag_Sse2 and then not Flags.M64 and then Mode in Mode_Fp then
         --  Convert return value from St0 to Xmm0.
         declare
            Sslot : constant Int32 := -Int32 (Cur_Subprg.Target.Fp_Slot);
         begin
            --  Move from St0 to Xmm0.
            --  fstp slot(%ebp)
            Start_Insn;
            Init_Modrm_Offset (R_Bp, Sslot, Sz_Fp);
            Gen_8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
            Gen_Mod_Rm_Opc (2#00_011_000#);
            End_Insn;
            --  movsd slot(%ebp), %xmm0
            Start_Insn;
            Gen_SSE_Prefix (Mode);
            Init_Modrm_Offset (R_Bp, Sslot, Sz_Fp);
            Gen_SSE_Opc (Opc_Movsd_Xmm_M64);
            Gen_Mod_Rm_Opc (2#00_000_000#);
            End_Insn;
         end;
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
      Init_Modrm_Expr (Dest, Sz_8);
      Gen_8 (Opc_0f);
      Gen_8 (Opc2_0f_Setcc + To_Cond (Cond));
      Gen_Mod_Rm_Opc (2#000_000#);
      End_Insn;
   end Emit_Setcc;

   procedure Emit_Setcc_Reg (Reg : O_Reg; Cond : O_Reg) is
   begin
      pragma Assert (Cond in Regs_Cc);
      Start_Insn;
      Gen_8 (Opc_0f);
      Gen_8 (Opc2_0f_Setcc + To_Cond (Cond));
      Gen_8 (2#11_000_000# + To_Reg32 (Reg, Sz_8));
      End_Insn;
   end Emit_Setcc_Reg;

   procedure Emit_Tst (Reg : O_Reg; Sz : Insn_Size) is
   begin
      Start_Insn;
      Init_Modrm_Reg (Reg, Sz, Reg, Sz);
      Gen_Insn_Sz (Opc_Test_Rm_Reg, Sz);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Emit_Tst;

   procedure Gen_Cmp_Imm (Reg : O_Reg; Val : Int32; Sz : Insn_Size) is
   begin
      Start_Insn;
      Init_Modrm_Reg (Reg, Sz);
      Gen_Insn_Grp1 (Opc2_Grp1_Cmp, Val);
      End_Insn;
   end Gen_Cmp_Imm;

   procedure Emit_Spill (Stmt : O_Enode; Sz : Insn_Size)
   is
      Expr : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg : constant O_Reg := Get_Expr_Reg (Expr);
   begin
      --  A reload is missing.
      pragma Assert (Reg /= R_Spill);
      Start_Insn;
      Init_Modrm_Mem (Stmt, Sz, Reg);
      Gen_Insn_Sz (Opc_Mov_Rm_Reg, Sz);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Emit_Spill;

   procedure Emit_Spill_Xmm (Stmt : O_Enode; Mode : Mode_Fp)
   is
      Expr : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg : constant O_Reg := Get_Expr_Reg (Expr);
   begin
      --  A reload is missing.
      pragma Assert (Reg in Regs_Xmm);
      --  movsd
      Start_Insn;
      Gen_SSE_Prefix (Mode);
      Init_Modrm_Mem (Stmt, Sz_Fp, Reg);
      Gen_SSE_Opc (Opc_Movsd_M64_Xmm);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Emit_Spill_Xmm;

   procedure Emit_Load (Reg : O_Reg; Val : O_Enode; Sz : Insn_Size)
   is
   begin
      Start_Insn;
      Init_Modrm_Expr (Val, Sz, Reg);
      Gen_Insn_Sz (Opc_Mov_Reg_Rm, Sz);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Emit_Load;

   procedure Emit_Lea (Stmt : O_Enode)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      --  Hack: change the register to use the real address instead of it.
      Set_Expr_Reg (Stmt, R_Mem);

      Start_Insn;
      Init_Modrm_Mem (Stmt, Sz_Ptr, Reg);
      Gen_8 (Opc_Leal_Reg_Rm);
      Gen_Mod_Rm_Reg;
      End_Insn;

      --  Restore.
      Set_Expr_Reg (Stmt, Reg);
   end Emit_Lea;

   procedure Gen_Umul (Stmt : O_Enode; Sz : Insn_Size)
   is
   begin
      pragma Assert (Get_Expr_Reg (Get_Expr_Left (Stmt)) = R_Ax);
      Start_Insn;
      Init_Modrm_Expr (Get_Expr_Right (Stmt), Sz);
      Gen_Insn_Sz (Opc_Grp3_Width, Sz);
      Gen_Mod_Rm_Opc (Opc2_Grp3_Mul);
      End_Insn;
   end Gen_Umul;

   procedure Gen_Mul (Stmt : O_Enode; Sz : Insn_Size)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
      Right : constant O_Enode := Get_Expr_Right (Stmt);
      Reg_R : O_Reg;
   begin
      pragma Assert (Get_Expr_Reg (Get_Expr_Left (Stmt)) = Reg);
      Start_Insn;
      if Reg = R_Ax then
         Init_Modrm_Expr (Right, Sz);
         Gen_Insn_Sz (Opc_Grp3_Width, Sz);
         Gen_Mod_Rm_Opc (Opc2_Grp3_Mul);
      else
         Reg_R := Get_Expr_Reg (Right);
         case Reg_R is
            when R_Imm =>
               Init_Modrm_Reg (Reg, Sz, Reg, Sz);
               if Is_Imm8 (Right, Sz) then
                  Gen_8 (Opc_Imul_Reg_Rm_Imm8);
                  Gen_Mod_Rm_Reg;
                  Gen_Imm8 (Right, Sz);
               else
                  Gen_8 (Opc_Imul_Reg_Rm_Imm32);
                  Gen_Mod_Rm_Reg;
                  Gen_Imm (Right, Sz);
               end if;
            when R_Mem
              | R_Spill
              | Regs_R64 =>
               Init_Modrm_Expr (Right, Sz, Reg);
               Gen_8 (Opc_0f);
               Gen_8 (Opc2_0f_Imul);
               Gen_Mod_Rm_Reg;
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

   procedure Gen_Into is
   begin
      if Flags.M64 then
         Gen_Ov_Check (R_No);
      else
         Gen_1 (Opc_Into);
      end if;
   end Gen_Into;

   procedure Emit_Abs (Val : O_Enode; Mode : Mode_Type)
   is
      Szl, Szh : Insn_Size;
      Pc_Jmp : Pc_Type;
   begin
      case Mode is
         when Mode_I32 =>
            Szh := Sz_32;
            Szl := Sz_32;
         when Mode_I64 =>
            if Flags.M64 then
               Szh := Sz_64;
               Szl := Sz_64;
            else
               Szh := Sz_32h;
               Szl := Sz_32l;
            end if;
         when others =>
            raise Program_Error;
      end case;
      Emit_Tst (Get_Expr_Reg (Val), Szh);
      --  JGE xxx (skip if positive).
      Gen_2 (Opc_Jcc + To_Cond (R_Sge), 0);
      Pc_Jmp := Get_Current_Pc;
      --  NEG
      Gen_Grp3_Insn (Opc2_Grp3_Neg, Val, Szl);
      if (not Flags.M64) and Mode = Mode_I64 then
         --  Propagate carry.
         --  Adc reg,0
         --  neg reg
         Start_Insn;
         Init_Modrm_Expr (Val, Sz_32h);
         Gen_Insn_Grp1 (Opc2_Grp1_Adc, 0);
         End_Insn;
         Gen_Grp3_Insn (Opc2_Grp3_Neg, Val, Sz_32h);
      end if;
      Gen_Into;
      Patch_8 (Pc_Jmp - 1, Unsigned_8 (Get_Current_Pc - Pc_Jmp));
   end Emit_Abs;

   procedure Gen_Alloca (Stmt : O_Enode)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Get_Expr_Operand (Stmt));
   begin
      pragma Assert (Reg in Regs_R64);
      pragma Assert (Reg = Get_Expr_Reg (Stmt));
      --  Align stack on word.
      --  Add reg, (stack_boundary - 1)
      Start_Insn;
      Gen_Rex_B (Reg, Sz_Ptr);
      Gen_8 (Opc_Grp1v_Rm_Imm8);
      Gen_8 (Opc2_Grp1_Add or 2#11_000_000# or To_Reg32 (Reg));
      Gen_8 (Byte (X86.Flags.Stack_Boundary - 1));
      End_Insn;
      --  and reg, ~(stack_boundary - 1)
      Start_Insn;
      Gen_Rex_B (Reg, Sz_Ptr);
      Gen_8 (Opc_Grp1v_Rm_Imm32);
      Gen_8 (Opc2_Grp1_And or 2#11_000_000# or To_Reg32 (Reg));
      Gen_32 (not (X86.Flags.Stack_Boundary - 1));
      End_Insn;
      --  Call chkstk if needed.
      --  On windows x32, chkstk probes the stack and allocate stack.
      --  On windows x64, chkstk only probes the stack.
      if X86.Flags.Flag_Alloca_Call then
         Gen_Call (Chkstk_Symbol);
      end if;
      if (not X86.Flags.Flag_Alloca_Call) or X86.Flags.Win64 then
         Gen_Sub_Sp_Reg (Reg);
      end if;
      --  movl reg, esp
      Start_Insn;
      Gen_Rex_B (Reg, Sz_Ptr);
      Gen_8 (Opc_Mov_Rm_Reg + 1);
      Gen_8 (2#11_100_000# + To_Reg32 (Reg));
      End_Insn;
   end Gen_Alloca;

   --  Byte/word to long.
   procedure Gen_Movzx (Reg : Regs_R64; Op : O_Enode; Dst_Sz : Insn_Size) is
   begin
      Start_Insn;
      Init_Modrm_Expr (Op, Dst_Sz, Reg);
      Gen_8 (Opc_0f);
      case Get_Expr_Mode (Op) is
         when Mode_I8 | Mode_U8 | Mode_B2 =>
            Gen_8 (Opc2_0f_Movzx);
         when Mode_I16 | Mode_U16 =>
            Gen_8 (Opc2_0f_Movzx + 1);
         when others =>
            raise Program_Error;
      end case;
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Gen_Movzx;

   procedure Gen_Movsxd (Src : O_Reg; Dst : O_Reg) is
   begin
      Start_Insn;
      Init_Modrm_Reg (Src, Sz_64, Dst, Sz_64);
      Gen_8 (Opc_Movsxd_Reg_Rm);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Gen_Movsxd;

   procedure Emit_Move (Operand : O_Enode; Sz : Insn_Size; Reg : O_Reg) is
   begin
      --  mov REG, OP
      Start_Insn;
      Init_Modrm_Expr (Operand, Sz, Reg);
      Gen_Insn_Sz (Opc_Mov_Reg_Rm, Sz);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Emit_Move;

   procedure Emit_Move_Xmm (Operand : O_Enode; Mode : Mode_Fp; Reg : O_Reg) is
   begin
      --  movsd REG, OP
      Start_Insn;
      Gen_SSE_Prefix (Mode);
      Init_Modrm_Expr (Operand, Sz_Fp, Reg);
      Gen_SSE_Opc (Opc_Movsd_Xmm_M64);
      Gen_Mod_Rm_Reg;
      End_Insn;
   end Emit_Move_Xmm;

   --  Convert U32 to xx.
   procedure Gen_Conv_U32 (Stmt : O_Enode; Ov : Boolean)
   is
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_I32 =>
            pragma Assert (Reg_Res in Regs_R64);
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32);
            end if;
            if Ov then
               Emit_Tst (Reg_Res, Sz_32);
               Gen_Ov_Check (R_Sge);
            end if;
         when Mode_I64 =>
            if Flags.M64 then
               Emit_Move (Op, Sz_32, Reg_Res);
            else
               pragma Assert (Reg_Res = R_Edx_Eax);
               pragma Assert (Reg_Op = R_Ax);
               --  Clear edx.
               Gen_Clear_Edx;
            end if;
         when Mode_U8
           | Mode_B2 =>
            pragma Assert (Reg_Res in Regs_R32);
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32);
            end if;
            if Ov then
               --  cmpl VAL, 0xff
               Start_Insn;
               Init_Modrm_Expr (Op, Sz_32);
               Gen_8 (Opc_Grp1v_Rm_Imm32);
               Gen_Mod_Rm_Opc (Opc2_Grp1_Cmp);
               Gen_32 (16#00_00_00_Ff#);
               End_Insn;
               Gen_Ov_Check (R_Ule);
            end if;
         when others =>
            Error_Emit ("gen_conv_u32", Stmt);
      end case;
   end Gen_Conv_U32;

   --  Convert I32 to xxx
   procedure Gen_Conv_I32 (Stmt : O_Enode; Ov : Boolean)
   is
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_I64 =>
            if Flags.M64 then
               Gen_Movsxd (Reg_Op, Reg_Res);
            else
               pragma Assert (Reg_Res = R_Edx_Eax);
               pragma Assert (Reg_Op = R_Ax);
               Gen_Cdq (Sz_32);
            end if;
         when Mode_U32 =>
            pragma Assert (Reg_Res in Regs_R32);
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32);
            end if;
            if Ov then
               Emit_Tst (Reg_Res, Sz_32);
               Gen_Ov_Check (R_Sge);
            end if;
         when Mode_B2 =>
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32);
            end if;
            if Ov then
               Gen_Cmp_Imm (Reg_Res, 1, Sz_32);
               Gen_Ov_Check (R_Ule);
            end if;
         when Mode_U8 =>
            if Reg_Op /= Reg_Res then
               Emit_Load (Reg_Res, Op, Sz_32);
            end if;
            if Ov then
               Gen_Cmp_Imm (Reg_Res, 16#Ff#, Sz_32);
               Gen_Ov_Check (R_Ule);
            end if;
         when Mode_F64 =>
            if Reg_Res in Regs_Xmm then
               --  cvtsi2sd
               Gen_SSE_Prefix (Mode_F64);
               Init_Modrm_Expr (Op, Sz_32, Reg_Res);
               Gen_SSE_Opc (Opc_Cvtsi2sd_Xmm_Rm);
               Gen_Mod_Rm_Reg;
               End_Insn;
            else
               Emit_Push (Op, Sz_32);
               --  fild (%esp)
               Start_Insn;
               Gen_8 (2#11011_011#);
               Gen_8 (2#00_000_100#);
               Gen_8 (2#00_100_100#);
               End_Insn;
               --  addl %esp, 4
               Emit_Addl_Sp_Imm (4);
            end if;
         when others =>
            Error_Emit ("gen_conv_i32", Stmt);
      end case;
   end Gen_Conv_I32;

   --  Convert U8 to xxx
   procedure Gen_Conv_U8 (Stmt : O_Enode)
   is
      Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
   begin
      case Mode is
         when Mode_U32
           | Mode_I32
           | Mode_U16
           | Mode_I16 =>
            pragma Assert (Reg_Res in Regs_R64);
            Gen_Movzx (Reg_Res, Op, Int_Mode_To_Size (Mode));
         when Mode_I64
           | Mode_U64 =>
            if Flags.M64 then
               Gen_Movzx (Reg_Res, Op, Sz_64);
            else
               pragma Assert (Reg_Res = R_Edx_Eax);
               pragma Assert (Reg_Op = R_Ax);
               Gen_Movzx (R_Ax, Op, Sz_32);
               --  Sign-extend, but we know the sign is positive.
               Gen_Cdq (Sz_32);
            end if;
         when others =>
            Error_Emit ("gen_conv_U8", Stmt);
      end case;
   end Gen_Conv_U8;

   --  Convert B2 to xxx
   procedure Gen_Conv_B2 (Stmt : O_Enode)
   is
      Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Mode is
         when Mode_U32
           | Mode_I32
           | Mode_U16
           | Mode_I16 =>
            pragma Assert (Reg_Res in Regs_R64);
            Gen_Movzx (Reg_Res, Op, Int_Mode_To_Size (Mode));
         when Mode_I64 =>
            if Flags.M64 then
               Gen_Movzx (Reg_Res, Op, Sz_64);
            else
               pragma Assert (Reg_Res = R_Edx_Eax);
               pragma Assert (Reg_Op = R_Ax);
               Gen_Movzx (R_Ax, Op, Sz_32);
               --  Sign-extend, but we know the sign is positive.
               Gen_Cdq (Sz_32);
            end if;
         when others =>
            Error_Emit ("gen_conv_B2", Stmt);
      end case;
   end Gen_Conv_B2;

   --  Convert I64 to xxx
   procedure Gen_Conv_I64 (Stmt : O_Enode; Ov : Boolean)
   is
      Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
      Op : constant O_Enode := Get_Expr_Operand (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Op);
      Reg_Res : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      case Mode is
         when Mode_I32 =>
            if Flags.M64 then
               --  movsxd src, dst
               Gen_Movsxd (Reg_Op, Reg_Res);
               if Ov then
                  --  cmp src,dst
                  Start_Insn;
                  Init_Modrm_Reg (Reg_Op, Sz_64, Reg_Res, Sz_64);
                  Gen_8 (Opc_Cmpl_Rm_Reg);
                  Gen_Mod_Rm_Reg;
                  End_Insn;
                  --  Overflow if extended value is different from initial one.
                  Gen_Ov_Check (R_Eq);
               end if;
            else
               pragma Assert (Reg_Op = R_Edx_Eax);
               pragma Assert (Reg_Res = R_Ax);
               --  move dx to reg_helper
               Start_Insn;
               Gen_8 (Opc_Mov_Rm_Reg + 1);
               Gen_8 (2#11_010_000# + To_Reg32 (Reg_Helper));
               End_Insn;
               --  Sign extend eax.
               Gen_Cdq (Sz_32);
               if Ov then
                  --  cmp reg_helper, dx
                  Start_Insn;
                  Gen_8 (Opc_Cmpl_Rm_Reg);
                  Gen_8 (2#11_010_000# + To_Reg32 (Reg_Helper));
                  End_Insn;
                  --  Overflow if extended value is different from initial one.
                  Gen_Ov_Check (R_Eq);
               end if;
            end if;
         when Mode_U8
           | Mode_B2 =>
            declare
               Ubound : Int32;
            begin
               if Mode = Mode_B2 then
                  Ubound := 1;
               else
                  Ubound := 16#ff#;
               end if;

               if Flags.M64 then
                  Emit_Load (Reg_Res, Op, Sz_64);
                  if Ov then
                     Start_Insn;
                     Init_Modrm_Reg (Reg_Res, Sz_64);
                     Gen_Insn_Grp1 (Opc2_Grp1_Cmp, Ubound);
                     End_Insn;
                     Gen_Ov_Check (R_Ule);
                  end if;
               else
                  pragma Assert (Reg_Op in Regs_Pair);
                  if Ov then
                     --  Check MSB = 0
                     Emit_Tst (Reg_Op, Sz_32h);
                     Gen_Ov_Check (R_Eq);
                  end if;
                  --  Check LSB <= 255 (U8) or LSB <= 1 (B2)
                  if Reg_Op /= Reg_Res then
                     --  Move reg_op -> reg_res
                     --  FIXME: factorize with OE_Mov.
                     Start_Insn;
                     Init_Modrm_Reg (Reg_Op, Sz_32l, Reg_Res);
                     Gen_Insn_Sz (Opc_Mov_Reg_Rm, Sz_32);
                     Gen_Mod_Rm_Reg;
                     End_Insn;
                  end if;
                  if Ov then
                     Gen_Cmp_Imm (Reg_Res, Ubound, Sz_32);
                     Gen_Ov_Check (R_Ule);
                  end if;
               end if;
            end;
         when Mode_F64 =>
            if Flags.M64 then
               --  cvtsi2sd
               Gen_SSE_Prefix (Mode_F64);
               Init_Modrm_Expr (Op, Sz_64, Reg_Res);
               Gen_SSE_Opc (Opc_Cvtsi2sd_Xmm_Rm);
               Gen_Mod_Rm_Reg;
               End_Insn;
            else
               Emit_Push (Op, Sz_32h);
               Emit_Push (Op, Sz_32l);
               --  fild (%esp)
               Start_Insn;
               Gen_8 (2#11011_111#);
               Gen_8 (2#00_101_100#);
               Gen_8 (2#00_100_100#);
               End_Insn;
               if Reg_Res in Regs_Xmm then
                  --  fstp (%esp)
                  Start_Insn;
                  Gen_8 (2#11011_00_1# + Mode_Fp_To_Mf (Mode_F64));
                  Gen_8 (2#00_011_100#);
                  Gen_8 (2#00_100_100#);
                  End_Insn;
                  --  movsd (%esp), %xmm
                  Start_Insn;
                  Gen_SSE_Prefix (Mode_F64);
                  Gen_SSE_Opc (Opc_Movsd_Xmm_M64);
                  Gen_8 (To_Reg_Xmm (Reg_Res) * 8 + 2#00_000_100#);
                  Gen_8 (2#00_100_100#);
                  End_Insn;
               end if;
               --  addl %esp, 8
               Emit_Addl_Sp_Imm (8);
            end if;
         when others =>
            Error_Emit ("gen_conv_I64", Stmt);
      end case;
   end Gen_Conv_I64;

   --  Convert FP to xxx.
   procedure Gen_Conv_Fp (Stmt : O_Enode)
   is
      Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
      Reg_Op : constant O_Reg := Get_Expr_Reg (Get_Expr_Operand (Stmt));
      Sslot : constant Int32 := -Int32 (Cur_Subprg.Target.Fp_Slot);
   begin
      if Abi.Flag_Sse2 and then
        (Mode = Mode_I32 or (Flags.M64 and Mode = Mode_I64))
      then
         --  cvtsd2si
         Gen_SSE_Prefix (Mode_F64);
         Init_Modrm_Reg (Reg_Op, Int_Mode_To_Size (Mode), Reg);
         Gen_SSE_Opc (Opc_Cvtsd2si_Reg_Xm);
         Gen_Mod_Rm_Reg;
         End_Insn;
         return;
      end if;

      if Reg_Op in Regs_Xmm then
         --  movsd %xmm, (%ebp),
         Start_Insn;
         Gen_SSE_Prefix (Mode_F64);
         Init_Modrm_Offset (R_Bp, Sslot, Sz_Ptr, Reg_Op);
         Gen_SSE_Opc (Opc_Movsd_M64_Xmm);
         Gen_Mod_Rm_Reg;
         End_Insn;
         --  fldl slot(%ebp)
         Start_Insn;
         Init_Modrm_Offset (R_Bp, Sslot, Sz_Ptr);
         Gen_8 (2#11011_00_1# + Mode_Fp_To_Mf (Mode_F64));
         Gen_Mod_Rm_Opc (2#00_000_000#);
         End_Insn;
      end if;

      case Mode is
         when Mode_I32 =>
            --  fistpl slot(%ebp)
            Start_Insn;
            Init_Modrm_Offset (R_Bp, Sslot, Sz_32);
            Gen_8 (2#11011_011#);
            Gen_Mod_Rm_Opc (2#00_011_000#);
            End_Insn;
            --  movl slot(%ebp), reg
            Start_Insn;
            Init_Modrm_Offset (R_Bp, Sslot, Sz_32, Reg);
            Gen_8 (Opc_Movl_Reg_Rm);
            Gen_Mod_Rm_Reg;
            End_Insn;
         when Mode_I64 =>
            --  fistpq slot(%ebp)
            Start_Insn;
            Init_Modrm_Offset (R_Bp, Sslot, Sz_32);
            Gen_8 (2#11011_111#);
            Gen_Mod_Rm_Opc (2#00_111_000#);
            End_Insn;
            --  movl slot(%ebp), reg
            for Sz in Sz_32l .. Sz_32h loop
               Start_Insn;
               Init_Modrm_Offset (R_Bp, Sslot, Sz, Reg);
               Gen_8 (Opc_Movl_Reg_Rm);
               Gen_Mod_Rm_Reg;
               End_Insn;
            end loop;
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
            Gen_Grp1_Insn (Cl, Stmt, Sz_32);
         when Mode_I64
           | Mode_U64 =>
            if Flags.M64 then
               Gen_Grp1_Insn (Cl, Stmt, Sz_64);
            else
               Gen_Grp1_Insn (Cl, Stmt, Sz_32l);
               Gen_Grp1_Insn (Ch, Stmt, Sz_32h);
            end if;
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

   procedure Gen_Emit_Fp_Op (Stmt : O_Enode; Fp_Op : Byte)
   is
      Right : constant O_Enode := Get_Expr_Right (Stmt);
      Reg : constant O_Reg := Get_Expr_Reg (Right);
      B_Size : Byte;
   begin
      Start_Insn;
      case Reg is
         when R_St0 =>
            Gen_8 (2#11011_110#);
            Gen_8 (2#11_000_001# or Fp_Op);
         when R_Mem =>
            case Get_Expr_Mode (Stmt) is
               when Mode_F32 =>
                  B_Size := 0;
               when Mode_F64 =>
                  B_Size := 2#100#;
               when others =>
                  raise Program_Error;
            end case;
            Init_Modrm_Mem (Right, Sz_Ptr);
            Gen_8 (2#11011_000# or B_Size);
            Gen_Mod_Rm_Opc (Fp_Op);
         when others =>
            raise Program_Error;
      end case;
      End_Insn;
   end Gen_Emit_Fp_Op;

   procedure Gen_Emit_Fp_Or_Xmm_Op
     (Stmt : O_Enode; Fp_Op : Byte; Xmm_Op : Byte)
   is
      Reg : constant O_Reg := Get_Expr_Reg (Stmt);
   begin
      if Reg in Regs_Xmm then
         declare
            Mode : constant Mode_Type := Get_Expr_Mode (Stmt);
            Right : constant O_Enode := Get_Expr_Right (Stmt);
         begin
            Start_Insn;
            Gen_SSE_Prefix (Mode);
            Init_Modrm_Expr (Right, Sz_32, Reg);
            Gen_SSE_Opc (Xmm_Op);
            Gen_Mod_Rm_Reg;
            End_Insn;
         end;
      else
         Gen_Emit_Fp_Op (Stmt, Fp_Op);
      end if;
   end Gen_Emit_Fp_Or_Xmm_Op;

   procedure Emit_Mod (Stmt : O_Enode; Sz : Insn_Size)
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
      Emit_Load (R_Dx, Right, Sz);
      --  xorl %eax -> %edx
      Start_Insn;
      Gen_Rex_B (R_None, Sz);
      Gen_8 (Opc_Xorl_Rm_Reg);
      Gen_8 (2#11_000_010#);
      End_Insn;
      Gen_Cdq (Sz);
      --  js
      Gen_2 (Opc_Jcc + 2#1000#, 0);
      Pc1 := Get_Current_Pc;
      --  idiv
      Gen_Grp3_Insn (Opc2_Grp3_Idiv, Right, Sz);
      --  jmp
      Gen_2 (Opc_Jmp_Short, 0);
      Pc2 := Get_Current_Pc;
      Patch_8 (Pc1 - 1, Unsigned_8 (Get_Current_Pc - Pc1));
      --  idiv
      Gen_Grp3_Insn (Opc2_Grp3_Idiv, Right, Sz);
      --  tstl %edx,%edx
      Start_Insn;
      Gen_Rex_B (R_None, Sz);
      Gen_8 (Opc_Test_Rm_Reg + 1);
      Gen_8 (2#11_010_010#);
      End_Insn;
      --  jz
      Gen_2 (Opc_Jcc + 2#0100#, 0);
      Pc3 := Get_Current_Pc;
      --  addl b, %edx
      Start_Insn;
      Init_Modrm_Expr (Right, Sz, R_Dx);
      Gen_8 (Opc_Addl_Reg_Rm);
      Gen_Mod_Rm_Reg;
      End_Insn;
      Patch_8 (Pc2 - 1, Unsigned_8 (Get_Current_Pc - Pc2));
      Patch_8 (Pc3 - 1, Unsigned_8 (Get_Current_Pc - Pc3));
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
               Gen_Emit_Fp_Or_Xmm_Op (Stmt, 2#000_000#, 16#58#);
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
               Gen_Emit_Fp_Or_Xmm_Op (Stmt, 2#100_000#, 16#5c#);
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
                  Gen_Mul (Stmt, Sz_32);
               when Mode_I32 =>
                  Gen_Grp3_Insn (Opc2_Grp3_Imul, Get_Expr_Right (Stmt), Sz_32);
                  if Kind = OE_Mul_Ov then
                     Gen_Check_Overflow (Mode);
                  end if;
               when Mode_I64 =>
                  Gen_Grp3_Insn (Opc2_Grp3_Imul, Get_Expr_Right (Stmt), Sz_64);
                  if Kind = OE_Mul_Ov then
                     Gen_Check_Overflow (Mode);
                  end if;
               when Mode_U64 =>
                  pragma Assert (Flags.M64);
                  Gen_Mul (Stmt, Sz_64);
               when Mode_F32
                 | Mode_F64 =>
                  Gen_Emit_Fp_Or_Xmm_Op (Stmt, 2#001_000#, 16#59#);
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
                     Sz := Sz_32;
                  when others =>
                     Error_Emit ("emit_insn: shl", Stmt);
               end case;
               Right := Get_Expr_Right (Stmt);
               if Get_Expr_Kind (Right) = OE_Const then
                  Val := Get_Expr_Low (Right);
                  Start_Insn;
                  Init_Modrm_Expr (Get_Expr_Left (Stmt), Sz);
                  if Val = 1 then
                     Gen_Insn_Sz (2#1101000_0#, Sz);
                     Gen_Mod_Rm_Opc (2#100_000#);
                  else
                     Gen_Insn_Sz (2#1100000_0#, Sz);
                     Gen_Mod_Rm_Opc (2#100_000#);
                     Gen_8 (Byte (Val and 31));
                  end if;
                  End_Insn;
               else
                  pragma Assert (Get_Expr_Reg (Right) = R_Cx);
                  Start_Insn;
                  Init_Modrm_Expr (Get_Expr_Left (Stmt), Sz);
                  Gen_Insn_Sz (2#1101001_0#, Sz);
                  Gen_Mod_Rm_Opc (2#100_000#);
                  End_Insn;
               end if;
            end;
         when OE_Mod
           | OE_Rem
           | OE_Div_Ov =>
            case Mode is
               when Mode_U32
                 | Mode_U64 =>
                  Gen_Clear_Edx;
                  Gen_Grp3_Insn (Opc2_Grp3_Div, Get_Expr_Right (Stmt),
                                 Int_Mode_To_Size (Mode));
               when Mode_I32
                 | Mode_I64 =>
                  declare
                     Sz : constant Insn_Size := Int_Mode_To_Size (Mode);
                  begin
                     if Kind = OE_Mod then
                        Emit_Mod (Stmt, Sz);
                     else
                        Gen_Cdq (Sz);
                        Gen_Grp3_Insn
                          (Opc2_Grp3_Idiv, Get_Expr_Right (Stmt), Sz);
                     end if;
                  end;
               when Mode_F32
                 | Mode_F64 =>
                  --  No Mod or Rem for fp types.
                  pragma Assert (Kind = OE_Div_Ov);
                  Gen_Emit_Fp_Or_Xmm_Op (Stmt, 2#110_000#, 16#5e#);
               when others =>
                  Error_Emit ("emit_insn: mod_ov", Stmt);
            end case;

         when OE_Not =>
            case Mode is
               when Mode_B2 =>
                  --  Xor VAL, $1
                  Start_Insn;
                  Init_Modrm_Expr (Stmt, Sz_8);
                  Gen_8 (Opc_Grp1v_Rm_Imm8);
                  Gen_Mod_Rm_Opc (Opc2_Grp1_Xor);
                  Gen_8 (16#01#);
                  End_Insn;
               when Mode_U8 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_8);
               when Mode_U16 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_16);
               when Mode_U32 =>
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_32);
               when Mode_U64 =>
                  if Flags.M64 then
                     Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_64);
                  else
                     Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_32l);
                     Gen_Grp3_Insn_Stmt (Opc2_Grp3_Not, Stmt, Sz_32h);
                  end if;
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
                  Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_32);
                  --Gen_Into;
               when Mode_I64 =>
                  if Flags.M64 then
                     Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_64);
                  else
                     Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_32l);
                     -- adcl 0, high
                     Start_Insn;
                     Init_Modrm_Expr (Get_Expr_Operand (Stmt), Sz_32h);
                     Gen_8 (Opc_Grp1v_Rm_Imm8);
                     Gen_Mod_Rm_Opc (Opc2_Grp1_Adc);
                     Gen_8 (0);
                     End_Insn;
                     Gen_Grp3_Insn_Stmt (Opc2_Grp3_Neg, Stmt, Sz_32h);
                     --Gen_Into;
                  end if;
               when Mode_F32
                 | Mode_F64 =>
                  Reg := Get_Expr_Reg (Stmt);
                  if Reg in Regs_Xmm then
                     --  Xorp{sd} reg, cst
                     Start_Insn;
                     Init_Modrm_Sym (Get_Xmm_Sign_Constant (Mode), Sz_32, Reg);
                     Gen_SSE_D16_Opc (Mode, Opc2_0f_Xorp);
                     Gen_Mod_Rm_Reg;
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
                     --  Andp{sd} reg, cst
                     Start_Insn;
                     Init_Modrm_Sym (Get_Xmm_Mask_Constant (Mode), Sz_32, Reg);
                     Gen_SSE_D16_Opc (Mode, Opc2_0f_Andp);
                     Gen_Mod_Rm_Reg;
                     End_Insn;
                  else
                     --  fabs
                     Gen_2 (2#11011_001#, 2#1110_0001#);
                  end if;
               when others =>
                  Error_Emit ("emit_insn: abs_ov", Stmt);
            end case;

         when OE_Kind_Cmp =>
            declare
               Left : constant O_Enode := Get_Expr_Left (Stmt);
               Op_Mode : constant Mode_Type := Get_Expr_Mode (Left);
            begin
               case Op_Mode is
                  when Mode_U32
                    | Mode_I32
                    | Mode_P32 =>
                     Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32);
                  when Mode_B2
                    | Mode_I8
                    | Mode_U8 =>
                     Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_8);
                  when Mode_U64
                    | Mode_P64 =>
                     if Flags.M64 then
                        Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_64);
                     else
                        declare
                           Pc : Pc_Type;
                        begin
                           Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32h);
                           --  jne
                           Gen_2 (Opc_Jcc + 2#0101#, 0);
                           Pc := Get_Current_Pc;
                           Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32l);
                           Patch_8 (Pc - 1, Unsigned_8 (Get_Current_Pc - Pc));
                        end;
                     end if;
                  when Mode_I64 =>
                     if Flags.M64 then
                        Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_64);
                     else
                        declare
                           Pc : Pc_Type;
                        begin
                           Reg := Get_Expr_Reg (Stmt);
                           Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32h);
                           --  Note: this does not clobber a reg due to care in
                           --  insns.
                           Emit_Setcc_Reg
                             (Reg, Insns.Ekind_Signed_To_Cc (Kind));
                           --  jne
                           Gen_2 (Opc_Jcc + 2#0101#, 0);
                           Pc := Get_Current_Pc;
                           Gen_Grp1_Insn (Opc2_Grp1_Cmp, Stmt, Sz_32l);
                           Emit_Setcc_Reg
                             (Reg, Insns.Ekind_Unsigned_To_Cc (Kind));
                           Patch_8 (Pc - 1, Unsigned_8 (Get_Current_Pc - Pc));
                           return;
                        end;
                     end if;
                  when Mode_F32
                    | Mode_F64 =>
                     if Abi.Flag_Sse2 then
                        --  comisd %xmm, rm
                        Start_Insn;
                        Init_Modrm_Expr (Get_Expr_Right (Stmt), Sz_32,
                                         Get_Expr_Reg (Left));
                        Gen_SSE_D16_Opc (Op_Mode, 16#2f#);
                        Gen_Mod_Rm_Reg;
                        End_Insn;
                     else
                        --  fcomip st, st(1)
                        Start_Insn;
                        Gen_8 (2#11011_111#);
                        Gen_8 (2#1111_0001#);
                        End_Insn;
                        --  fstp st, st (0)
                        Start_Insn;
                        Gen_8 (2#11011_101#);
                        Gen_8 (2#11_011_000#);
                        End_Insn;
                     end if;
                  when others =>
                     Error_Emit ("emit_insn: cmp", Stmt);
               end case;
               --  Result is in eflags.
               pragma Assert (Get_Expr_Reg (Stmt) in Regs_Cc);
            end;
         when OE_Addrd =>
            pragma Assert (Mode = Abi.Mode_Ptr);
            if Flags.M64
              and then not Insns.Is_External_Object (Get_Addr_Decl (Stmt))
            then
               --  Use RIP relative to load an address.
               Emit_Lea (Stmt);
            else
               Emit_Load_Imm (Stmt, Sz_Ptr);
            end if;
         when OE_Const =>
            case Mode is
               when Mode_B2
                 | Mode_U8
                 | Mode_I8 =>
                  Emit_Load_Imm (Stmt, Sz_8);
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Load_Imm (Stmt, Sz_32);
               when Mode_I64
                 | Mode_U64
                 | Mode_P64 =>
                  if Flags.M64 then
                     Emit_Load_Imm (Stmt, Sz_64);
                  else
                     pragma Assert (Mode /= Mode_P64);
                     Emit_Load_Imm (Stmt, Sz_32l);
                     Emit_Load_Imm (Stmt, Sz_32h);
                  end if;
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
                  Emit_Load_Mem (Stmt, Sz_32);
               when Mode_B2
                 | Mode_U8
                 | Mode_I8 =>
                  Emit_Load_Mem (Stmt, Sz_8);
               when Mode_U64
                 | Mode_I64
                 | Mode_P64 =>
                  if Flags.M64 then
                     Emit_Load_Mem (Stmt, Sz_64);
                  else
                     pragma Assert (Mode /= Mode_P64);
                     Emit_Load_Mem (Stmt, Sz_32l);
                     Emit_Load_Mem (Stmt, Sz_32h);
                  end if;
               when Mode_Fp =>
                  Emit_Load_Fp_Mem (Stmt, Mode);
               when others =>
                  Error_Emit ("emit_insn: indir", Stmt);
            end case;

         when OE_Conv_Ov
            | OE_Conv =>
            --  Call Gen_Conv_FROM
            case Get_Expr_Mode (Get_Expr_Operand (Stmt)) is
               when Mode_U32 =>
                  Gen_Conv_U32 (Stmt, Kind = OE_Conv_Ov);
               when Mode_I32 =>
                  Gen_Conv_I32 (Stmt, Kind = OE_Conv_Ov);
               when Mode_U8 =>
                  Gen_Conv_U8 (Stmt);
               when Mode_B2 =>
                  Gen_Conv_B2 (Stmt);
               when Mode_I64 =>
                  Gen_Conv_I64 (Stmt, Kind = OE_Conv_Ov);
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
                  Emit_Store (Stmt, Sz_32);
               when Mode_B2
                 | Mode_U8
                 | Mode_I8 =>
                  Emit_Store (Stmt, Sz_8);
               when Mode_U64
                 | Mode_I64
                 | Mode_P64 =>
                  if Flags.M64 then
                     Emit_Store (Stmt, Sz_64);
                  else
                     Emit_Store (Stmt, Sz_32l);
                     Emit_Store (Stmt, Sz_32h);
                  end if;
               when Mode_Fp =>
                  if Abi.Flag_Sse2 then
                     Emit_Store_Xmm (Stmt, Mode);
                  else
                     Emit_Store_Fp (Stmt, Mode);
                  end if;
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
            --  Only arguments passed on the stack are represented by OE_Arg.
            --  Arguments passed by registers (for x86-64) are simply
            --  pre-computed.
            case Mode is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Push (Get_Expr_Operand (Stmt), Sz_32);
               when Mode_U64
                 | Mode_I64
                 | Mode_P64 =>
                  if Flags.M64 then
                     Emit_Push (Get_Expr_Operand (Stmt), Sz_64);
                  else
                     Emit_Push (Get_Expr_Operand (Stmt), Sz_32h);
                     Emit_Push (Get_Expr_Operand (Stmt), Sz_32l);
                  end if;
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
                     if Reg in Regs_R64 and then Op_Reg in Regs_Cc then
                        Emit_Setcc (Stmt, Op_Reg);
                     elsif (Reg = R_Eq or Reg = R_Ne)
                       and then Op_Reg in Regs_R64
                     then
                        Emit_Tst (Op_Reg, Sz_8);
                     elsif Reg in Regs_R64 and then Op_Reg in Regs_R64 then
                        Emit_Move (Operand, Sz_Ptr, Reg);
                     else
                        Error_Emit ("emit_insn: move/b2", Stmt);
                     end if;
                  when Mode_U32
                    | Mode_I32 =>
                     Emit_Move (Operand, Sz_32, Reg);
                  when Mode_U64
                    | Mode_I64
                    | Mode_P64 =>
                     pragma Assert (Flags.M64);
                     Emit_Move (Operand, Sz_64, Reg);
                  when Mode_F64
                    | Mode_F32 =>
                     Emit_Move_Xmm (Operand, Mode, Reg);
                  when others =>
                     Error_Emit ("emit_insn: move", Stmt);
               end case;
            end;

         when OE_Alloca =>
            pragma Assert (Mode = Abi.Mode_Ptr);
            Gen_Alloca (Stmt);

         when OE_Set_Stack =>
            Emit_Load_Mem (Stmt, Sz_Ptr);

         when OE_Add
           | OE_Addrl =>
            case Mode is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Lea (Stmt);
               when Mode_U64
                 | Mode_I64
                 | Mode_P64 =>
                  pragma Assert (Flags.M64);
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
                  Emit_Spill (Stmt, Sz_32);
               when Mode_U64
                 | Mode_I64
                 | Mode_P64 =>
                  if Flags.M64 then
                     Emit_Spill (Stmt, Sz_64);
                  else
                     Emit_Spill (Stmt, Sz_32l);
                     Emit_Spill (Stmt, Sz_32h);
                  end if;
               when Mode_F32
                 | Mode_F64 =>
                  Emit_Spill_Xmm (Stmt, Mode);
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
                     Emit_Load (Reg, Expr, Sz_32);
                  when Mode_U64
                    | Mode_I64
                    | Mode_P64 =>
                     if Flags.M64 then
                        Emit_Load (Reg, Expr, Sz_64);
                     else
                        Emit_Load (Reg, Expr, Sz_32l);
                        Emit_Load (Reg, Expr, Sz_32h);
                     end if;
                  when Mode_F32
                    | Mode_F64 =>
                     pragma Assert (Reg in Regs_Xmm);
                     --  movsd
                     Start_Insn;
                     Gen_SSE_Prefix (Mode_F64);
                     Init_Modrm_Mem (Expr, Sz_Fp, Reg);
                     Gen_SSE_Opc (Opc_Movsd_Xmm_M64);
                     Gen_Mod_Rm_Reg;
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
            if Flag_Debug /= Debug_None then
               Dwarf.Set_Line_Stmt (Get_Expr_Line_Number (Stmt));
               Set_Current_Section (Sect_Text);
            end if;
         when others =>
            Error_Emit ("cannot handle insn", Stmt);
      end case;
   end Emit_Insn;

   function Get_Preserved_Regs return O_Reg_Bitmap is
   begin
      if Flags.M64 then
         if Flags.Win64 then
            return Preserved_Regs_Win64;
         else
            return Preserved_Regs_Lin64;
         end if;
      else
         return Preserved_Regs_32;
      end if;
   end Get_Preserved_Regs;

   --  List of registers preserved accross calls.
   Preserved_Regs : constant O_Reg_Bitmap := Get_Preserved_Regs;

   procedure Push_Reg (Reg : Regs_R64) is
   begin
      Gen_Push_Pop_Reg (Opc_Push_Reg, Reg, Sz_Ptr);
   end Push_Reg;

   procedure Pop_Reg (Reg : Regs_R64) is
   begin
      Gen_Push_Pop_Reg (Opc_Pop_Reg, Reg, Sz_Ptr);
   end Pop_Reg;

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
      Has_Fp_Inter : Boolean;
      Alloc_Pc : Pc_Type;
   begin
      --  Switch to .text section and align the function (to avoid the nested
      --  function trick and for performance).
      Set_Current_Section (Sect_Text);
      Gen_Pow_Align (2);

      --  Set symbol.
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

      --  Return address and saved frame pointer are preserved.
      Saved_Regs_Size := 2;
      for R in Preserved_Regs'Range loop
         if Preserved_Regs (R) and Reg_Used (R) then
            Saved_Regs_Size := Saved_Regs_Size + 1;
         end if;
      end loop;
      if Flags.M64 then
         Saved_Regs_Size := Saved_Regs_Size * 8;
      else
         Saved_Regs_Size := Saved_Regs_Size * 4;
      end if;

      --  Compute frame size.
      --  Saved_Regs_Size must be added and substracted as the stack boundary
      --  can be larger than a reg size.
      Frame_Size := Unsigned_32 (Subprg.Stack_Max) + Saved_Regs_Size;
      --  Align.
      Frame_Size := (Frame_Size + X86.Flags.Stack_Boundary - 1)
        and not (X86.Flags.Stack_Boundary - 1);
      --  The bytes for saved regs are already allocated.
      Frame_Size := Frame_Size - Saved_Regs_Size;

      --  Emit prolog.
      --  push %ebp / push %rbp
      Push_Reg (R_Bp);
      --  movl %esp, %ebp / movq %rsp, %rbp
      Start_Insn;
      Gen_Rex (16#48#);
      Gen_8 (Opc_Mov_Rm_Reg + 1);
      Gen_8 (2#11_100_101#);
      End_Insn;

      --  Save int arguments (only on x86-64).
      Has_Fp_Inter := False;
      if Flags.M64 then
         declare
            Inter : O_Dnode;
            R : O_Reg;
         begin
            Inter := Get_Subprg_Interfaces (Subprg.D_Decl);
            while Inter /= O_Dnode_Null loop
               R := Get_Decl_Reg (Inter);
               if R in Regs_R64 then
                  Push_Reg (R);
                  --  Space for arguments was already counted in frame size.
                  --  As the space is allocated by the push, don't allocate it
                  --  later.
                  Frame_Size := Frame_Size - 8;
               elsif R in Regs_Xmm then
                  --  Need to save Xmm registers, but later.
                  Has_Fp_Inter := True;
               else
                  pragma Assert (R = R_None);
                  null;
               end if;
               Inter := Get_Interface_Chain (Inter);
            end loop;
         end;
      end if;

      --  subl XXX, %esp / subq XXX, %rsp
      if Frame_Size /= 0 then
         if X86.Flags.Flag_Alloca_Call and then Frame_Size >= 4096 then
            --  mov stack_size,%eax
            Start_Insn;
            Gen_8 (Opc_Movl_Imm_Reg + To_Reg32 (R_Ax));
            Gen_32 (Frame_Size);
            End_Insn;

            Gen_Call (Chkstk_Symbol);

            if Flags.Win64 then
               Gen_Sub_Sp_Reg (R_Ax);
            end if;
         else
            Gen_Sub_Sp_Imm (Int32 (Frame_Size));
         end if;
      end if;
      Alloc_Pc := Get_Current_Pc;

      --  Save XMM arguments.
      if Flags.M64 and Has_Fp_Inter then
         declare
            Inter : O_Dnode;
            R : O_Reg;
         begin
            Inter := Get_Subprg_Interfaces (Subprg.D_Decl);
            while Inter /= O_Dnode_Null loop
               R := Get_Decl_Reg (Inter);
               if R in Regs_Xmm then
                  Start_Insn;
                  Gen_SSE_Prefix (Mode_F64);
                  Init_Modrm_Offset (R_Bp, Get_Local_Offset (Inter), Sz_Fp, R);
                  Gen_SSE_Opc (Opc_Movsd_M64_Xmm);
                  Gen_Mod_Rm_Reg;
                  End_Insn;
                  --  No need to adjust frame_size, it was already allocated.
               end if;
               Inter := Get_Interface_Chain (Inter);
            end loop;
         end;
      end if;

      if Flag_Profile then
         Gen_Call (Mcount_Symbol);
      end if;

      --  Save preserved registers that are used in the function.
      for R in Preserved_Regs'Range loop
         if Preserved_Regs (R) and Reg_Used (R) then
            Push_Reg (R);
         end if;
      end loop;

      if Flags.Win64 then
         declare
            End_Pc : constant Pc_Type := Get_Current_Pc;
            Nbr_Unw_Code : Unsigned_8;
         begin
            Set_Current_Section (Sect_Xdata);
            Last_Unwind_Off := Get_Current_Pc;
            Prealloc (7*2);
            --  UNWIND_INFO
            Gen_8 (16#01#);            --  Version(3)=1, Flags(5)=0
            pragma Assert (End_Pc - Subprg_Pc < 256);
            Gen_8 (Byte (End_Pc - Subprg_Pc));  --  Size of prolog
            Gen_8 (2);                 --  Nbr unwind opcodes
            Gen_8 (16#05#);            --  FrameReg(4)=ebp, FrameRegOff(4)=0*16
            --  0: push ebp
            Gen_8 (1);       --  Offset: +1
            Gen_8 (16#50#);  --  Op: SAVE_NONVOL, Reg: 5 (ebp)
            --  1: set fp
            Gen_8 (4);       --  Offset: +3
            Gen_8 (16#03#);  --  Op: SET_FP_REG
            Nbr_Unw_Code := 2;
            --  x: alloc frame
            if Frame_Size > 0 then
               Gen_8 (Byte (Alloc_Pc - Subprg_Pc));    --  Offset
               if Frame_Size <= 128 then
                  Gen_8 (16#02# + Byte (Frame_Size - 8) / 8 * 16);
                  Nbr_Unw_Code := Nbr_Unw_Code + 1;
               elsif Frame_Size < 512 * 1024 - 8 then
                  Gen_8 (16#02#);
                  Gen_16 (Frame_Size / 8);
                  Nbr_Unw_Code := Nbr_Unw_Code + 2;
               else
                  Gen_8 (16#12#);
                  Gen_16 ((Frame_Size / 8) mod 16#1_0000#);
                  Gen_16 ((Frame_Size / 8) / 16#1_0000#);
                  Nbr_Unw_Code := Nbr_Unw_Code + 3;
               end if;
            end if;
            --  y: save preserved
            --  TODO

            Patch_8 (Last_Unwind_Off + 2, Nbr_Unw_Code);
            Set_Current_Section (Sect_Text);
         end;
      end if;
   end Emit_Prologue;

   procedure Emit_Epilogue (Subprg : Subprogram_Data_Acc)
   is
      use Ortho_Code.Decls;
      use Ortho_Code.Types;
      use Ortho_Code.Flags;
      use Ortho_Code.X86.Insns;
      Decl : O_Dnode;
      Mode : Mode_Type;
   begin
      --  Restore registers.
      for R in reverse Preserved_Regs'Range loop
         if Preserved_Regs (R) and Reg_Used (R) then
            Pop_Reg (R);
         end if;
      end loop;

      Decl := Subprg.D_Decl;
      if Get_Decl_Kind (Decl) = OD_Function then
         Mode := Get_Type_Mode (Get_Decl_Type (Decl));
         case Mode is
            when Mode_U8
              | Mode_B2 =>
               --  movzx %al,%eax
               Start_Insn;
               Gen_8 (Opc_0f);
               Gen_8 (Opc2_0f_Movzx);
               Gen_8 (2#11_000_000#);
               End_Insn;
            when Mode_U32
              | Mode_I32
              | Mode_U64
              | Mode_I64
              | Mode_P32
              | Mode_P64 =>
               null;
            when  Mode_F32
              | Mode_F64 =>
               if Abi.Flag_Sse2 and not Flags.M64 then
                  --  movsd %xmm0, slot(%ebp)
                  Start_Insn;
                  Gen_SSE_Prefix (Mode);
                  Init_Modrm_Offset
                    (R_Bp, -Int32 (Cur_Subprg.Target.Fp_Slot), Sz_32);
                  Gen_SSE_Opc (Opc_Movsd_M64_Xmm);
                  Gen_Mod_Rm_Opc (2#00_000_000#);
                  End_Insn;
                  --  fldl slot(%ebp) [keep same modrm parameters]
                  Start_Insn;
                  Gen_8 (2#11011_001# + Mode_Fp_To_Mf (Mode));
                  Gen_Mod_Rm_Opc (2#00_000_000#);
                  End_Insn;
               end if;
            when others =>
               raise Program_Error;
         end case;
      end if;

      --  leave; ret;
      Gen_1 (Opc_Leave);
      Gen_1 (Opc_Ret);

      if Flag_Debug /= Debug_None then
         Set_Body_Info (Subprg.D_Body, Int32 (Get_Current_Pc - Subprg_Pc));
      end if;
   end Emit_Epilogue;

   procedure Gen_FDE
   is
      Subprg_Size : Unsigned_32;
   begin
      Subprg_Size := Unsigned_32 (Get_Current_Pc - Subprg_Pc);

      Set_Current_Section (Sect_Eh_Frame);
      Prealloc (20);
      Gen_32 (16);            --  Length
      Gen_32 (Unsigned_32 (Get_Current_Pc));  --  CIE pointer
      Gen_32 (Unsigned_32 (Subprg_Pc));       --  Initial location (.text rel)
      Gen_32 (Subprg_Size);                   --  Function size
      Gen_8 (0);                              --  Length
      Gen_8 (0);
      Gen_8 (0);
      Gen_8 (0);
   end Gen_FDE;

   procedure Gen_Win64_Unwind (Subprg : Subprogram_Data_Acc)
   is
      Start : constant Symbol := Get_Decl_Symbol (Subprg.D_Decl);
      Subprg_Size : constant Unsigned_32 :=
         Unsigned_32 (Get_Current_Pc - Subprg_Pc);
   begin
      Set_Current_Section (Sect_Pdata);
      Prealloc (3 * 4);
      --  RUNTIME_FUNCTION
      --   start, end, info
      Gen_X86_Img_32 (Start, 0);
      Gen_X86_Img_32 (Start, Subprg_Size);
      Gen_X86_Img_32 (Xdata_Sym, Unsigned_32 (Last_Unwind_Off));
   end Gen_Win64_Unwind;

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

      if Flags.Eh_Frame then
         Gen_FDE;
      end if;
      if Flags.Win64 then
         Gen_Win64_Unwind (Subprg);
      end if;
   end Emit_Subprg;

   procedure Emit_Var_Decl (Decl : O_Dnode)
   is
      use Decls;
      Sym : Symbol;
   begin
      Sym := Create_Symbol (Get_Decl_Ident (Decl), False);
      Set_Decl_Info (Decl, To_Int32 (Uns32 (Sym)));
   end Emit_Var_Decl;

   procedure Emit_Var_Zero (Decl : O_Dnode)
   is
      use Decls;
      use Types;
      Sym : constant Symbol := Symbol (To_Uns32 (Get_Decl_Info (Decl)));
      Storage : constant O_Storage := Get_Decl_Storage (Decl);
      Dtype : constant O_Tnode := Get_Decl_Type (Decl);
   begin
      Set_Current_Section (Sect_Bss);
      pragma Assert (Storage = O_Storage_Public
                       or Storage = O_Storage_Private);
      Gen_Pow_Align (Get_Type_Align (Dtype));
      Set_Symbol_Pc (Sym, Storage = O_Storage_Public);
      Gen_Space (Integer_32 (Get_Type_Size (Dtype)));
      Set_Current_Section (Sect_Text);
   end Emit_Var_Zero;

   procedure Emit_Const_Decl (Decl : O_Dnode)
   is
      use Decls;
      Sym : Symbol;
   begin
      Set_Current_Section (Sect_Rodata);
      Sym := Create_Symbol (Get_Decl_Ident (Decl), False);
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
                  Gen_8 (Byte (L));
               when Mode_U32
                 | Mode_I32
                 | Mode_F32
                 | Mode_P32 =>
                  Gen_32 (Unsigned_32 (L));
               when Mode_F64
                 | Mode_I64
                 | Mode_U64
                 | Mode_P64 =>
                  Gen_32 (Unsigned_32 (L));
                  Gen_32 (Unsigned_32 (H));
               when others =>
                  raise Program_Error;
            end case;
         when OC_Address =>
            declare
               Decl : O_Dnode;
               Off : Uns32;
            begin
               Get_Global_Decl_Offset (Get_Const_Global (Val), Decl, Off);
               Gen_Abs (Get_Decl_Symbol (Decl), Integer_32 (To_Int32 (Off)));
            end;
         when OC_Subprg_Address =>
            Gen_Abs (Get_Decl_Symbol (Get_Const_Decl (Val)), 0);
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
         when OC_Zero =>
            for I in 1 .. Get_Type_Size (Get_Const_Type (Val)) loop
               Gen_8 (0);
            end loop;
         when OC_Sizeof
            | OC_Record_Sizeof
            | OC_Alignof
            | OC_Union =>
            raise Program_Error;
      end case;
   end Emit_Const;

   procedure Emit_Init_Value (Decl : O_Dnode; Val : O_Cnode)
   is
      use Decls;
      use Types;
      Sym : constant Symbol := Get_Decl_Symbol (Decl);
      Dtype : constant O_Tnode := Get_Decl_Type (Decl);
   begin
      case Get_Decl_Kind (Decl) is
         when OD_Const =>
            Set_Current_Section (Sect_Rodata);
         when OD_Var =>
            Set_Current_Section (Sect_Rodata);
         when others =>
            raise Syntax_Error;
      end case;

      Gen_Pow_Align (Get_Type_Align (Dtype));
      Set_Symbol_Pc (Sym, Get_Decl_Storage (Decl) = O_Storage_Public);
      Prealloc (Pc_Type (Consts.Get_Const_Size (Val)));
      Emit_Const (Val);

      Set_Current_Section (Sect_Text);
   end Emit_Init_Value;

   procedure Init
   is
      use Ortho_Ident;
      use Ortho_Code.Flags;
   begin
      if Flags.M64 then
         Arch := Arch_X86_64;
      else
         Arch := Arch_X86;
      end if;

      Create_Section (Sect_Text, ".text", Section_Exec + Section_Read);
      Create_Section (Sect_Rodata, ".rodata", Section_Read);
      Create_Section (Sect_Bss, ".bss",
                      Section_Read + Section_Write + Section_Zero);

      Set_Current_Section (Sect_Text);

      if Flag_Profile then
         Mcount_Symbol := Create_Symbol (Get_Identifier ("mcount"), True);
      end if;

      if X86.Flags.Flag_Alloca_Call then
         Chkstk_Symbol := Create_Symbol (Get_Identifier ("___chkstk"), True);
      end if;

      if not Flags.M64 then
         Intrinsics_Symbol (Intrinsic_Mul_Ov_U64) :=
           Create_Symbol (Get_Identifier ("__muldi3"), True);
         Intrinsics_Symbol (Intrinsic_Div_Ov_U64) :=
           Create_Symbol (Get_Identifier ("__mcode_div_ov_u64"), True);
         Intrinsics_Symbol (Intrinsic_Mod_Ov_U64) :=
           Create_Symbol (Get_Identifier ("__mcode_mod_ov_u64"), True);
         Intrinsics_Symbol (Intrinsic_Mul_Ov_I64) :=
           Create_Symbol (Get_Identifier ("__muldi3"), True);
         Intrinsics_Symbol (Intrinsic_Div_Ov_I64) :=
           Create_Symbol (Get_Identifier ("__divdi3"), True);
         Intrinsics_Symbol (Intrinsic_Mod_Ov_I64) :=
           Create_Symbol (Get_Identifier ("__mcode_mod_ov_i64"), True);
         Intrinsics_Symbol (Intrinsic_Rem_Ov_I64) :=
           Create_Symbol (Get_Identifier ("__mcode_rem_ov_i64"), True);
      end if;

      if Debug.Flag_Debug_Asm then
         Dump_Asm := True;
      end if;
      if Debug.Flag_Debug_Hex then
         Debug_Hex := True;
      end if;

      if Flags.Eh_Frame then
         Create_Section (Sect_Eh_Frame, ".eh_frame", 0);
         Set_Current_Section (Sect_Eh_Frame);
         Prealloc (32);

         --  Generate CIE
         Gen_32 (28);  --  Length
         Gen_32 (0);  --  CIE id = 0
         Gen_8 (1);   --  Version = 1
         Gen_8 (Character'Pos ('z'));  --  Augmentation
         Gen_8 (Character'Pos ('R'));  --  Augmentation
         Gen_8 (0);                    --  End of Augmentation
         Gen_8 (1);   --  Code align factor
         if Flags.M64 then
            Dwarf.Gen_Sleb128 (-8); --  Data align factor
            Dwarf.Gen_Uleb128 (16); --  Return address (16 = rip)
         else
            Dwarf.Gen_Sleb128 (-4);
            Dwarf.Gen_Uleb128 (0);  --  TODO
         end if;
         Dwarf.Gen_Uleb128 (1); --  z: length of the remainder of augmentation
         Gen_8 (16#23#);        --  R: pointer encoding: .text relative, udata4

         --  CFIs (call frame instructions)
         --  Initial state: cfa = rsp + 8, rip = -8@cfa
         Gen_8 (16#0c#);  --  DW_CFA_def_cfa
         Gen_8 (16#07#);  --    reg 7 (rsp)
         Gen_8 (16#08#);  --    offset 8
         Gen_8 (16#80# or 16#10#); --  DW_CFA_def_offset reg 16 (rip)
         Gen_8 (16#01#);           --   offset 1 * (-8) = -8
         --  push %rbp, cfa = rsp + 16
         Gen_8 (16#40# or 16#01#); --  DW_CFA_advance_loc +1
         Gen_8 (16#0e#);           --  DW_CFA_def_cfa_offset
         Gen_8 (16#10#);           --   offset 16
         Gen_8 (16#80# or 16#06#); --  DW_CFA_def_offset reg 6 (rbp)
         Gen_8 (16#02#);           --   offset 2 * (-8) = -16
         --  movq %rsp, %rbp, cfa = rbp + 16
         Gen_8 (16#40# or 16#03#); --  DW_CFA_advance_loc +3
         Gen_8 (16#0d#);           --  DW_CFA_def_cfa_register
         Gen_8 (16#06#);           --   reg 6 (rbp)
         Gen_8 (0);                --  nop
         Gen_8 (0);                --  nop
      end if;

      if Flags.Win64 then
         Create_Section (Sect_Pdata, ".pdata", Section_Read);
         Create_Section (Sect_Xdata, ".xdata", Section_Read);
         Set_Current_Section (Sect_Xdata);
         Xdata_Sym := Create_Local_Symbol;
      end if;

      if Flag_Debug /= Debug_None then
         Dwarf.Init;
      end if;

      Set_Current_Section (Sect_Text);
   end Init;

   procedure Finish
   is
      use Ortho_Code.Flags;
   begin
      if Flag_Debug /= Debug_None then
         Set_Current_Section (Sect_Text);
         Dwarf.Finish;
      end if;

      if Flags.Eh_Frame then
         Set_Current_Section (Sect_Eh_Frame);
         Prealloc (4);
         Gen_32 (0);  --  Size = 0 -> end.
      end if;
   end Finish;

end Ortho_Code.X86.Emits;
