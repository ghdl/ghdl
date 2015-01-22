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

   type Fp_Size is (Fp_32, Fp_64);

   Sect_Text : Binary_File.Section_Acc;
   Sect_Rodata : Binary_File.Section_Acc;
   Sect_Bss : Binary_File.Section_Acc;

   Reg_Helper : O_Reg;

   Subprg_Pc : Pc_Type;

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


   procedure Gen_Insn_Sz (B : Byte; Sz : Insn_Size) is
   begin
      case Sz is
         when Sz_8 =>
            Gen_B8 (B);
         when Sz_16 =>
            Gen_B8 (16#66#);
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
            Gen_B8 (16#66#);
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
         when OE_Addrg =>
            if Sz /= Sz_32l then
               raise Program_Error;
            end if;
            Gen_X86_32 (Get_Decl_Symbol (Get_Addr_Object (N)), 0);
         when OE_Add =>
            declare
               P : O_Enode;
               L, R : O_Enode;
               S, C : O_Enode;
               Off : Int32;
            begin
               Off := 0;
               P := N;
               if Sz /= Sz_32l then
                  raise Program_Error;
               end if;
               loop
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
                  if Get_Expr_Mode (C) /= Mode_U32 then
                     raise Program_Error;
                  end if;
                  Off := Off + To_Int32 (Get_Expr_Low (C));

                  exit when Get_Expr_Kind (S) = OE_Addrg;
                  P := S;
                  if Get_Expr_Kind (P) /= OE_Add then
                     raise Program_Error;
                  end if;
               end loop;
               Gen_X86_32 (Get_Decl_Symbol (Get_Addr_Object (S)),
                           Integer_32 (Off));
            end;
         when others =>
            raise Program_Error;
      end case;
   end Gen_Imm;

   Rm_Base : O_Reg;
   Rm_Index : O_Reg;
   Rm_Offset : Int32;
   Rm_Sym : Symbol;
   Rm_Scale : Byte;

   procedure Fill_Sib (N : O_Enode)
   is
      use Ortho_Code.Decls;
      Reg : O_Reg;
   begin
      Reg := Get_Expr_Reg (N);
      if Reg in Regs_R32 then
         if Rm_Base = R_Nil then
            Rm_Base := Reg;
         elsif Rm_Index = R_Nil then
            Rm_Index := Reg;
         else
            raise Program_Error;
         end if;
         return;
      end if;
      case Get_Expr_Kind (N) is
         when OE_Indir =>
            Fill_Sib (Get_Expr_Operand (N));
         when OE_Addrl =>
            declare
               Frame : O_Enode;
            begin
               Frame := Get_Addrl_Frame (N);
               if Frame = O_Enode_Null then
                  Rm_Base := R_Bp;
               else
                  Rm_Base := Get_Expr_Reg (Frame);
               end if;
            end;
            Rm_Offset := Rm_Offset + Get_Local_Offset (Get_Addr_Object (N));
         when OE_Addrg =>
            if Rm_Sym /= Null_Symbol then
               raise Program_Error;
            end if;
            Rm_Sym := Get_Decl_Symbol (Get_Addr_Object (N));
         when OE_Add =>
            Fill_Sib (Get_Expr_Left (N));
            Fill_Sib (Get_Expr_Right (N));
         when OE_Const =>
            Rm_Offset := Rm_Offset + To_Int32 (Get_Expr_Low (N));
         when OE_Shl =>
            if Rm_Index /= R_Nil then
               raise Program_Error;
            end if;
            Rm_Index := Get_Expr_Reg (Get_Expr_Left (N));
            Rm_Scale := Byte (Get_Expr_Low (Get_Expr_Right (N)));
         when others =>
            Error_Emit ("fill_sib", N);
      end case;
   end Fill_Sib;

   function To_Reg32 (R : O_Reg) return Byte is
   begin
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
            if R in Regs_R8 then
               return O_Reg'Pos (R) - O_Reg'Pos (R_Ax);
            else
               raise Program_Error;
            end if;
         when Sz_16 =>
            if R in Regs_R32 then
               return O_Reg'Pos (R) - O_Reg'Pos (R_Ax);
            else
               raise Program_Error;
            end if;
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

   procedure Gen_Sib is
   begin
      if Rm_Base = R_Nil then
         Gen_B8 (Rm_Scale * 2#1_000_000#
                 + To_Reg32 (Rm_Index) * 2#1_000#
                 + 2#101#);
      else
         Gen_B8 (Rm_Scale * 2#1_000_000#
                 + To_Reg32 (Rm_Index) * 2#1_000#
                 + To_Reg32 (Rm_Base));
      end if;
   end Gen_Sib;

   --  Generate an R/M (+ SIB) byte.
   --  R is added to the R/M byte.
   procedure Gen_Rm_Mem (R : Byte; N : O_Enode; Sz : Insn_Size)
   is
      Reg : O_Reg;
   begin
      Reg := Get_Expr_Reg (N);
      Rm_Base := R_Nil;
      Rm_Index := R_Nil;
      if Sz = Sz_32h then
         Rm_Offset := 4;
      else
         Rm_Offset := 0;
      end if;
      Rm_Scale := 0;
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
            Error_Emit ("gen_rm_mem: unhandled reg", N);
      end case;
      if Rm_Index /= R_Nil then
         --  SIB.
         if Rm_Base = R_Nil then
            Gen_B8 (2#00_000_100# + R);
            Rm_Base := R_Bp;
            Gen_Sib;
            Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
         elsif Rm_Sym = Null_Symbol and Rm_Offset = 0 and Rm_Base /= R_Bp then
            Gen_B8 (2#00_000_100# + R);
            Gen_Sib;
         elsif Rm_Sym = Null_Symbol and Rm_Offset <= 127 and Rm_Offset >= -128
         then
            Gen_B8 (2#01_000_100# + R);
            Gen_Sib;
            Gen_B8 (Byte (To_Uns32 (Rm_Offset) and 16#Ff#));
         else
            Gen_B8 (2#10_000_100# + R);
            Gen_Sib;
            Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
         end if;
         return;
      end if;
      case Rm_Base is
         when R_Sp =>
            raise Program_Error;
         when R_Nil =>
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
               Gen_B8 (2#00_000_000# + R + To_Reg32 (Rm_Base));
            elsif Rm_Sym = Null_Symbol
               and Rm_Offset <= 127 and Rm_Offset >= -128
            then
               Gen_B8 (2#01_000_000# + R + To_Reg32 (Rm_Base));
               Gen_B8 (Byte (To_Uns32 (Rm_Offset) and 16#Ff#));
            else
               Gen_B8 (2#10_000_000# + R + To_Reg32 (Rm_Base));
               Gen_X86_32 (Rm_Sym, Integer_32 (Rm_Offset));
            end if;
         when others =>
            raise Program_Error;
      end case;
   end Gen_Rm_Mem;

   procedure Gen_Rm (R : Byte; N : O_Enode; Sz : Insn_Size)
   is
      Reg : O_Reg;
   begin
      Reg := Get_Expr_Reg (N);
      if Reg in Regs_R32 or Reg in Regs_R64 then
         Gen_B8 (2#11_000_000# + R + To_Reg32 (Reg, Sz));
         return;
      else
         Gen_Rm_Mem (R, N, Sz);
      end if;
   end Gen_Rm;

   procedure Emit_Op (Op : Byte; Stmt : O_Enode; Sz : Insn_Size)
   is
      L, R : O_Enode;
      Lr, Rr : O_Reg;
   begin
      L := Get_Expr_Left (Stmt);
      R := Get_Expr_Right (Stmt);
      Lr := Get_Expr_Reg (L);
      Rr := Get_Expr_Reg (R);
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
   end Emit_Op;

   procedure Gen_Into is
   begin
      Start_Insn;
      Gen_B8 (2#1100_1110#);
      End_Insn;
   end Gen_Into;

   procedure Gen_Cdq is
   begin
      Start_Insn;
      Gen_B8 (2#1001_1001#);
      End_Insn;
   end Gen_Cdq;

   procedure Gen_Clear_Edx is
   begin
      --  Xorl edx, edx
      Start_Insn;
      Gen_B8 (2#0011_0001#);
      Gen_B8 (2#11_010_010#);
      End_Insn;
   end Gen_Clear_Edx;

   procedure Gen_Mono_Op (Op : Byte; Val : O_Enode; Sz : Insn_Size) is
   begin
      Start_Insn;
      Gen_Insn_Sz (2#1111_011_0#, Sz);
      Gen_Rm (Op, Val, Sz);
      End_Insn;
   end Gen_Mono_Op;

   procedure Emit_Mono_Op_Stmt (Op : Byte; Stmt : O_Enode; Sz : Insn_Size)
   is
   begin
      Gen_Mono_Op (Op, Get_Expr_Operand (Stmt), Sz);
   end Emit_Mono_Op_Stmt;

   procedure Emit_Load_Imm (Stmt : O_Enode; Sz : Insn_Size)
   is
      Tr : O_Reg;
   begin
      Tr := Get_Expr_Reg (Stmt);
      Start_Insn;
      --  FIXME: handle 0.
      case Sz is
         when Sz_8 =>
            Gen_B8 (2#1011_0_000# + To_Reg32 (Tr, Sz));
         when Sz_16 =>
            Gen_B8 (16#66#);
            Gen_B8 (2#1011_1_000# + To_Reg32 (Tr, Sz));
         when Sz_32l
           | Sz_32h =>
            Gen_B8 (2#1011_1_000# + To_Reg32 (Tr, Sz));
      end case;
      Gen_Imm (Stmt, Sz);
      End_Insn;
   end Emit_Load_Imm;

   function Fp_Size_To_Mf (Sz : Fp_Size) return Byte is
   begin
      case Sz is
         when Fp_32 =>
            return 2#00_0#;
         when Fp_64 =>
            return 2#10_0#;
      end case;
   end Fp_Size_To_Mf;

   procedure Emit_Load_Fp (Stmt : O_Enode; Sz : Fp_Size)
   is
      Sym : Symbol;
      R : O_Reg;
   begin
      Set_Current_Section (Sect_Rodata);
      Gen_Pow_Align (3);
      Prealloc (8);
      Sym := Create_Local_Symbol;
      Set_Symbol_Pc (Sym, False);
      Gen_Le32 (Unsigned_32 (Get_Expr_Low (Stmt)));
      if Sz = Fp_64 then
         Gen_Le32 (Unsigned_32 (Get_Expr_High (Stmt)));
      end if;
      Set_Current_Section (Sect_Text);

      R := Get_Expr_Reg (Stmt);
      case R is
         when R_St0 =>
            Start_Insn;
            Gen_B8 (2#11011_001# + Fp_Size_To_Mf (Sz));
            Gen_B8 (2#00_000_101#);
            Gen_X86_32 (Sym, 0);
            End_Insn;
         when Regs_Xmm =>
            Start_Insn;
            case Sz is
               when Fp_32 =>
                  Gen_B8 (16#F3#);
               when Fp_64 =>
                  Gen_B8 (16#F2#);
            end case;
            Gen_B8 (16#0f#);
            Gen_B8 (16#10#);
            Gen_B8 (2#00_000_101# + To_Reg_Xmm (R) * 2#1_000#);
            Gen_X86_32 (Sym, 0);
            End_Insn;
         when others =>
            raise Program_Error;
      end case;
   end Emit_Load_Fp;

   procedure Emit_Load_Fp_Mem (Stmt : O_Enode; Sz : Fp_Size)
   is
   begin
      Start_Insn;
      Gen_B8 (2#11011_001# + Fp_Size_To_Mf (Sz));
      Gen_Rm_Mem (2#000_000#, Get_Expr_Operand (Stmt), Sz_32l);
      End_Insn;
   end Emit_Load_Fp_Mem;

   procedure Emit_Load_Mem (Stmt : O_Enode; Sz : Insn_Size)
   is
      Tr : O_Reg;
      Val : O_Enode;
   begin
      Tr := Get_Expr_Reg (Stmt);
      Val := Get_Expr_Operand (Stmt);
      case Tr is
         when Regs_R32
           | Regs_R64 =>
            --  mov REG, OP
            Start_Insn;
            Gen_Insn_Sz (2#1000_101_0#, Sz);
            Gen_Rm_Mem (To_Reg32 (Tr, Sz) * 8, Val, Sz);
            End_Insn;
         when R_Eq =>
            --  Cmp OP, 1
            Start_Insn;
            Gen_Insn_Sz_S8 (2#1000_000_0#, Sz);
            Gen_Rm_Mem (2#111_000#, Val, Sz);
            Gen_B8 (1);
            End_Insn;
         when others =>
            Error_Emit ("emit_load_mem", Stmt);
      end case;
   end Emit_Load_Mem;


   procedure Emit_Store (Stmt : O_Enode; Sz : Insn_Size)
   is
      T, R : O_Enode;
      Tr, Rr : O_Reg;
      B : Byte;
   begin
      T := Get_Assign_Target (Stmt);
      R := Get_Expr_Operand (Stmt);
      Tr := Get_Expr_Reg (T);
      Rr := Get_Expr_Reg (R);
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
               Gen_Insn_Sz (2#1100_011_0#, Sz);
               Gen_Rm_Mem (16#00#, T, Sz);
            end if;
            Gen_Imm (R, Sz);
         when Regs_R32
           | Regs_R64 =>
            Gen_Insn_Sz (2#1000_100_0#, Sz);
            Gen_Rm_Mem (To_Reg32 (Rr, Sz) * 8, T, Sz);
         when others =>
            Error_Emit ("emit_store", Stmt);
      end case;
      End_Insn;
   end Emit_Store;

   procedure Emit_Store_Fp (Stmt : O_Enode; Sz : Fp_Size)
   is
   begin
      -- fstp
      Start_Insn;
      Gen_B8 (2#11011_00_1# + Fp_Size_To_Mf (Sz));
      Gen_Rm_Mem (2#011_000#, Get_Assign_Target (Stmt), Sz_32l);
      End_Insn;
   end Emit_Store_Fp;

   procedure Emit_Push_32 (Val : O_Enode; Sz : Insn_Size)
   is
      R : O_Reg;
   begin
      R := Get_Expr_Reg (Val);
      Start_Insn;
      case R is
         when R_Imm =>
            if Is_Imm8 (Val, Sz) then
               Gen_B8 (2#0110_1010#);
               Gen_Imm8 (Val, Sz);
            else
               Gen_B8 (2#0110_1000#);
               Gen_Imm (Val, Sz);
            end if;
         when Regs_R32
           | Regs_R64 =>
            Gen_B8 (2#01010_000# + To_Reg32 (R, Sz));
         when others =>
            Gen_B8 (2#1111_1111#);
            Gen_Rm (2#110_000#, Val, Sz);
      end case;
      End_Insn;
   end Emit_Push_32;

   procedure Emit_Pop_32 (Val : O_Enode; Sz : Insn_Size)
   is
      R : O_Reg;
   begin
      R := Get_Expr_Reg (Val);
      Start_Insn;
      case R is
         when Regs_R32
           | Regs_R64 =>
            Gen_B8 (2#01011_000# + To_Reg32 (R, Sz));
         when others =>
            Gen_B8 (2#1000_1111#);
            Gen_Rm (2#000_000#, Val, Sz);
      end case;
      End_Insn;
   end Emit_Pop_32;

   procedure Emit_Push_Fp (Op : O_Enode; Sz : Fp_Size)
   is
      pragma Unreferenced (Op);
   begin
      Start_Insn;
      --  subl esp, val
      Gen_B8 (2#100000_11#);
      Gen_B8 (2#11_101_100#);
      case Sz is
         when Fp_32 =>
            Gen_B8 (4);
         when Fp_64 =>
            Gen_B8 (8);
      end case;
      End_Insn;
      --  fstp st, (esp)
      Start_Insn;
      Gen_B8 (2#11011_001# + Fp_Size_To_Mf (Sz));
      Gen_B8 (2#00_011_100#);
      Gen_B8 (2#00_100_100#);
      End_Insn;
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
         Gen_B8 (16#0f#);
         Gen_B8 (16#80# + Opc);
         Gen_X86_Pc32 (Sym);
      else
         if Val + 128 < Get_Current_Pc + 4 then
            --  Long jmp.
            Gen_B8 (16#0f#);
            Gen_B8 (16#80# + Opc);
            Gen_Le32 (Unsigned_32 (Val - (Get_Current_Pc + 4)));
         else
            --  short jmp.
            Gen_B8 (16#70# + Opc);
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
         Gen_B8 (16#e9#);
         Gen_X86_Pc32 (Sym);
      else
         if Val + 128 < Get_Current_Pc + 4 then
            --  Long jmp.
            Gen_B8 (16#e9#);
            Gen_Le32 (Unsigned_32 (Val - (Get_Current_Pc + 4)));
         else
            --  short jmp.
            Gen_B8 (16#eb#);
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
      Gen_B8 (16#E8#);
      Gen_X86_Pc32 (Sym);
      End_Insn;
   end Gen_Call;

   procedure Emit_Setup_Frame (Stmt : O_Enode)
   is
      Val : constant Int32 := Get_Stack_Adjust (Stmt);
   begin
      if Val > 0 then
         Start_Insn;
         --  subl esp, val
         Gen_B8 (2#100000_11#);
         Gen_B8 (2#11_101_100#);
         Gen_B8 (Byte (Val));
         End_Insn;
      elsif Val < 0 then
         Start_Insn;
         if -Val <= 127 then
            --  addl esp, val
            Gen_B8 (2#100000_11#);
            Gen_B8 (2#11_000_100#);
            Gen_B8 (Byte (-Val));
         else
            --  addl esp, val
            Gen_B8 (2#100000_01#);
            Gen_B8 (2#11_000_100#);
            Gen_Le32 (Unsigned_32 (-Val));
         end if;
         End_Insn;
      end if;
   end Emit_Setup_Frame;

   procedure Emit_Call (Stmt : O_Enode)
   is
      use Ortho_Code.Decls;
      Subprg : O_Dnode;
      Sym : Symbol;
   begin
      Subprg := Get_Call_Subprg (Stmt);
      Sym := Get_Decl_Symbol (Subprg);
      Gen_Call (Sym);
   end Emit_Call;

   procedure Emit_Intrinsic (Stmt : O_Enode)
   is
      Op : Int32;
   begin
      Op := Get_Intrinsic_Operation (Stmt);
      Start_Insn;
      Gen_B8 (16#E8#);
      Gen_X86_Pc32 (Intrinsics_Symbol (Op));
      End_Insn;

      Start_Insn;
      --  addl esp, val
      Gen_B8 (2#100000_11#);
      Gen_B8 (2#11_000_100#);
      Gen_B8 (16);
      End_Insn;
   end Emit_Intrinsic;

   procedure Emit_Setcc (Dest : O_Enode; Cond : O_Reg)
   is
   begin
      if Cond not in Regs_Cc then
         raise Program_Error;
      end if;
      Start_Insn;
      Gen_B8 (16#0f#);
      Gen_B8 (16#90# + To_Cond (Cond));
      Gen_Rm (2#000_000#, Dest, Sz_8);
      End_Insn;
   end Emit_Setcc;

   procedure Emit_Setcc_Reg (Reg : O_Reg; Cond : O_Reg)
   is
   begin
      if Cond not in Regs_Cc then
         raise Program_Error;
      end if;
      Start_Insn;
      Gen_B8 (16#0f#);
      Gen_B8 (16#90# + To_Cond (Cond));
      Gen_B8 (2#11_000_000# + To_Reg32 (Reg, Sz_8));
      End_Insn;
   end Emit_Setcc_Reg;

   procedure Emit_Tst (Reg : O_Reg; Sz : Insn_Size)
   is
   begin
      Start_Insn;
      Gen_Insn_Sz (2#1000_0100#, Sz);
      Gen_B8 (2#11_000_000# + To_Reg32 (Reg, Sz) * 9);
      End_Insn;
   end Emit_Tst;

   procedure Gen_Cmp_Imm (Reg : O_Reg; Val : Int32; Sz : Insn_Size)
   is
      B : Byte;
   begin
      Start_Insn;
      if Val <= 127 and Val >= -128 then
         B := 2#10#;
      else
         B := 0;
      end if;
      Gen_Insn_Sz (2#1000_0000# + B, Sz);
      Gen_B8 (2#11_111_000# + To_Reg32 (Reg));
      if B = 0 then
         Gen_Le32 (Unsigned_32 (To_Uns32 (Val)));
      else
         Gen_B8 (Byte (To_Uns32 (Val) and 16#Ff#));
      end if;
      End_Insn;
   end Gen_Cmp_Imm;

   procedure Emit_Spill (Stmt : O_Enode; Sz : Insn_Size)
   is
      Reg : O_Reg;
      Expr : O_Enode;
   begin
      Expr := Get_Expr_Operand (Stmt);
      Reg := Get_Expr_Reg (Expr);
      if Reg = R_Spill then
         if Get_Expr_Kind (Expr) = OE_Conv then
            return;
         else
            raise Program_Error;
         end if;
      end if;
      Start_Insn;
      Gen_Insn_Sz (2#1000_1000#, Sz);
      Gen_Rm (To_Reg32 (Reg, Sz) * 8, Stmt, Sz);
      End_Insn;
   end Emit_Spill;

   procedure Emit_Load (Reg : O_Reg; Val : O_Enode; Sz : Insn_Size)
   is
   begin
      Start_Insn;
      Gen_Insn_Sz (2#1000_1010#, Sz);
      Gen_Rm (To_Reg32 (Reg, Sz) * 8, Val, Sz);
      End_Insn;
   end Emit_Load;

   procedure Emit_Lea (Stmt : O_Enode)
   is
      Reg : O_Reg;
   begin
      --  Hack: change the register to use the real address instead of it.
      Reg := Get_Expr_Reg (Stmt);
      Set_Expr_Reg (Stmt, R_Mem);

      Start_Insn;
      Gen_B8 (2#10001101#);
      Gen_Rm_Mem (To_Reg32 (Reg) * 8, Stmt, Sz_32l);
      End_Insn;
      Set_Expr_Reg (Stmt, Reg);
   end Emit_Lea;

   procedure Gen_Umul (Stmt : O_Enode; Sz : Insn_Size)
   is
   begin
      if Get_Expr_Reg (Get_Expr_Left (Stmt)) /= R_Ax then
         raise Program_Error;
      end if;
      Start_Insn;
      Gen_Insn_Sz (16#F6#, Sz);
      Gen_Rm (2#100_000#, Get_Expr_Right (Stmt), Sz);
      End_Insn;
   end Gen_Umul;

   procedure Gen_Mul (Stmt : O_Enode; Sz : Insn_Size)
   is
      Reg : O_Reg;
      Right : O_Enode;
      Reg_R : O_Reg;
   begin
      Reg := Get_Expr_Reg (Stmt);
      Right := Get_Expr_Right (Stmt);
      if Get_Expr_Reg (Get_Expr_Left (Stmt)) /= Reg
        or Sz /= Sz_32l
      then
         raise Program_Error;
      end if;
      Start_Insn;
      if Reg = R_Ax then
         Gen_Insn_Sz (16#F6#, Sz);
         Gen_Rm (2#100_000#, Right, Sz);
      else
         Reg_R := Get_Expr_Reg (Right);
         case Reg_R is
            when R_Imm =>
               if Is_Imm8 (Right, Sz) then
                  Gen_B8 (16#6B#);
                  Gen_B8 (To_Reg32 (Reg, Sz) * 9 or 2#11_000_000#);
                  Gen_Imm8 (Right, Sz);
               else
                  Gen_B8 (16#69#);
                  Gen_B8 (To_Reg32 (Reg, Sz) * 9 or 2#11_000_000#);
                  Gen_Imm (Right, Sz);
               end if;
            when R_Mem
              | R_Spill
              | Regs_R32 =>
               Gen_B8 (16#0F#);
               Gen_B8 (16#AF#);
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
      Start_Insn;
      Gen_B8 (16#70# + To_Cond (Cond));
      Gen_B8 (16#02#);
      End_Insn;
      --  INT 4 (overflow).
      Start_Insn;
      Gen_B8 (16#CD#);
      Gen_B8 (16#04#);
      End_Insn;
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
      Gen_B8 (16#70# + To_Cond (R_Sge));
      Gen_B8 (0);
      End_Insn;
      Pc_Jmp := Get_Current_Pc;
      --  NEG
      Gen_Mono_Op (2#011_000#, Val, Sz_32l);
      if Mode = Mode_I64 then
         --  Propagate carray.
         --  Adc reg,0
         --  neg reg
         Start_Insn;
         Gen_B8 (2#100000_11#);
         Gen_Rm (2#010_000#, Val, Sz_32h);
         Gen_B8 (0);
         End_Insn;
         Gen_Mono_Op (2#011_000#, Val, Sz_32h);
      end if;
      Gen_Into;
      Patch_B8 (Pc_Jmp - 1, Unsigned_8 (Get_Current_Pc - Pc_Jmp));
   end Emit_Abs;

   procedure Gen_Alloca (Stmt : O_Enode)
   is
      Reg : O_Reg;
   begin
      Reg := Get_Expr_Reg (Get_Expr_Operand (Stmt));
      if Reg not in Regs_R32 or else Reg /= Get_Expr_Reg (Stmt) then
         raise Program_Error;
      end if;
      --  Align stack on word.
      --  Add reg, (stack_boundary - 1)
      Start_Insn;
      Gen_B8 (2#1000_0011#);
      Gen_B8 (2#11_000_000# + To_Reg32 (Reg));
      Gen_B8 (Byte (X86.Flags.Stack_Boundary - 1));
      End_Insn;
      --  and reg, ~(stack_boundary - 1)
      Start_Insn;
      Gen_B8 (2#1000_0001#);
      Gen_B8 (2#11_100_000# + To_Reg32 (Reg));
      Gen_Le32 (not (X86.Flags.Stack_Boundary - 1));
      End_Insn;
      if X86.Flags.Flag_Alloca_Call then
         Gen_Call (Chkstk_Symbol);
      else
         --  subl esp, reg
         Start_Insn;
         Gen_B8 (2#0001_1011#);
         Gen_B8 (2#11_100_000# + To_Reg32 (Reg));
         End_Insn;
      end if;
      --  movl reg, esp
      Start_Insn;
      Gen_B8 (2#1000_1001#);
      Gen_B8 (2#11_100_000# + To_Reg32 (Reg));
      End_Insn;
   end Gen_Alloca;

   --  Byte/word to long.
   procedure Gen_Movzx (Reg : Regs_R32; Op : O_Enode; Sz : Insn_Size)
   is
      B : Byte;
   begin
      Start_Insn;
      Gen_B8 (16#0f#);
      case Sz is
         when Sz_8 =>
            B := 0;
         when Sz_16 =>
            B := 1;
         when Sz_32l
           | Sz_32h =>
            raise Program_Error;
      end case;
      Gen_B8 (2#1011_0110# + B);
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
            Gen_B8 (2#1000_0001#);
            Gen_Rm (2#111_000#, Op, Sz_32l);
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
            --  addl %esp, 4
            Start_Insn;
            Gen_B8 (2#100000_11#);
            Gen_B8 (2#11_000_100#);
            Gen_B8 (4);
            End_Insn;
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
            Gen_B8 (2#1000_1001#);
            Gen_B8 (2#11_010_000# + To_Reg32 (Reg_Helper));
            End_Insn;
            --  Sign extend eax.
            Gen_Cdq;
            --  cmp reg_helper, dx
            Start_Insn;
            Gen_B8 (2#0011_1001#);
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
            --  addl %esp, 8
            Start_Insn;
            Gen_B8 (2#100000_11#);
            Gen_B8 (2#11_000_100#);
            Gen_B8 (8);
            End_Insn;
         when others =>
            Error_Emit ("gen_conv_I64", Stmt);
      end case;
   end Gen_Conv_I64;

   --  Convert FP to xxx.
   procedure Gen_Conv_Fp (Stmt : O_Enode) is
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_I32 =>
            --  subl %esp, 4
            Start_Insn;
            Gen_B8 (2#100000_11#);
            Gen_B8 (2#11_101_100#);
            Gen_B8 (4);
            End_Insn;
            --  fistp (%esp)
            Start_Insn;
            Gen_B8 (2#11011_011#);
            Gen_B8 (2#00_011_100#);
            Gen_B8 (2#00_100_100#);
            End_Insn;
            Emit_Pop_32 (Stmt, Sz_32l);
         when Mode_I64 =>
            --  subl %esp, 8
            Start_Insn;
            Gen_B8 (2#100000_11#);
            Gen_B8 (2#11_101_100#);
            Gen_B8 (8);
            End_Insn;
            --  fistp (%esp)
            Start_Insn;
            Gen_B8 (2#11011_111#);
            Gen_B8 (2#00_111_100#);
            Gen_B8 (2#00_100_100#);
            End_Insn;
            Emit_Pop_32 (Stmt, Sz_32l);
            Emit_Pop_32 (Stmt, Sz_32h);
         when others =>
            Error_Emit ("gen_conv_fp", Stmt);
      end case;
   end Gen_Conv_Fp;

   procedure Gen_Emit_Op (Stmt : O_Enode; Cl : Byte; Ch : Byte) is
   begin
      case Get_Expr_Mode (Stmt) is
         when Mode_U32
           | Mode_I32
           | Mode_P32 =>
            Emit_Op (Cl, Stmt, Sz_32l);
         when Mode_I64
           | Mode_U64 =>
            Emit_Op (Cl, Stmt, Sz_32l);
            Emit_Op (Ch, Stmt, Sz_32h);
         when Mode_B2
           | Mode_I8
           | Mode_U8 =>
            Emit_Op (Cl, Stmt, Sz_8);
         when others =>
            Error_Emit ("gen_emit_op", Stmt);
      end case;
   end Gen_Emit_Op;

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
            Gen_Rm_Mem (B_Mem, Right, Sz_32l);
         when others =>
            raise Program_Error;
      end case;
      End_Insn;
   end Gen_Emit_Fp_Op;

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
      Gen_B8 (2#0011_0011#);
      Gen_B8 (2#11_010_000#);
      End_Insn;
      Gen_Cdq;
      --  js
      Start_Insn;
      Gen_B8 (2#0111_1000#);
      Gen_B8 (0);
      End_Insn;
      Pc1 := Get_Current_Pc;
      --  idiv
      Gen_Mono_Op (2#111_000#, Right, Sz_32l);
      --  jmp
      Start_Insn;
      Gen_B8 (2#1110_1011#);
      Gen_B8 (0);
      End_Insn;
      Pc2 := Get_Current_Pc;
      Patch_B8 (Pc1 - 1, Unsigned_8 (Get_Current_Pc - Pc1));
      --  idiv
      Gen_Mono_Op (2#111_000#, Right, Sz_32l);
      --  tstl %edx,%edx
      Start_Insn;
      Gen_B8 (2#1000_0101#);
      Gen_B8 (2#11_010_010#);
      End_Insn;
      --  jz
      Start_Insn;
      Gen_B8 (2#0111_0100#);
      Gen_B8 (0);
      End_Insn;
      Pc3 := Get_Current_Pc;
      --  addl b, %edx
      Start_Insn;
      Gen_B8 (2#00_000_011#);
      Gen_Rm (2#010_000#, Right, Sz_32l);
      End_Insn;
      Patch_B8 (Pc2 - 1, Unsigned_8 (Get_Current_Pc - Pc2));
      Patch_B8 (Pc3 - 1, Unsigned_8 (Get_Current_Pc - Pc3));
   end Emit_Mod;

   procedure Emit_Insn (Stmt : O_Enode)
   is
      use Ortho_Code.Flags;
      Kind : OE_Kind;
      Mode : Mode_Type;
      Reg : O_Reg;
   begin
      Kind := Get_Expr_Kind (Stmt);
      Mode := Get_Expr_Mode (Stmt);
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
               Gen_Emit_Fp_Op (Stmt, 2#000_000#, 2#000_000#);
            else
               Gen_Emit_Op (Stmt, 2#000_000#, 2#010_000#);
               Gen_Check_Overflow (Mode);
            end if;
         when OE_Or =>
            Gen_Emit_Op (Stmt, 2#001_000#, 2#001_000#);
         when OE_And =>
            Gen_Emit_Op (Stmt, 2#100_000#, 2#100_000#);
         when OE_Xor =>
            Gen_Emit_Op (Stmt, 2#110_000#, 2#110_000#);
         when OE_Sub_Ov =>
            if Mode in Mode_Fp then
               Gen_Emit_Fp_Op (Stmt, 2#100_000#, 2#100_000#);
            else
               Gen_Emit_Op (Stmt, 2#101_000#, 2#011_000#);
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
                  Gen_Mono_Op (2#101_000#, Get_Expr_Right (Stmt), Sz_32l);
               when Mode_F32
                 | Mode_F64 =>
                  Gen_Emit_Fp_Op (Stmt, 2#001_000#, 2#001_000#);
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
                  if Get_Expr_Reg (Right) /= R_Cx then
                     raise Program_Error;
                  end if;
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
                  Gen_Mono_Op (2#110_000#, Get_Expr_Right (Stmt), Sz_32l);
               when Mode_I32 =>
                  if Kind = OE_Mod then
                     Emit_Mod (Stmt);
                  else
                     Gen_Cdq;
                     Gen_Mono_Op (2#111_000#, Get_Expr_Right (Stmt), Sz_32l);
                  end if;
               when Mode_F32
                 | Mode_F64 =>
                  if Kind = OE_Div_Ov then
                     Gen_Emit_Fp_Op (Stmt, 2#111_000#, 2#110_000#);
                  else
                     raise Program_Error;
                  end if;
               when others =>
                  Error_Emit ("emit_insn: mod_ov", Stmt);
            end case;

         when OE_Not =>
            case Mode is
               when Mode_B2 =>
                  --  Xor VAL, $1
                  Start_Insn;
                  Gen_B8 (2#1000_0011#);
                  Gen_Rm (2#110_000#, Stmt, Sz_8);
                  Gen_B8 (16#01#);
                  End_Insn;
               when Mode_U8 =>
                  Emit_Mono_Op_Stmt (2#010_000#, Stmt, Sz_8);
               when Mode_U16 =>
                  Emit_Mono_Op_Stmt (2#010_000#, Stmt, Sz_16);
               when Mode_U32 =>
                  Emit_Mono_Op_Stmt (2#010_000#, Stmt, Sz_32l);
               when Mode_U64 =>
                  Emit_Mono_Op_Stmt (2#010_000#, Stmt, Sz_32l);
                  Emit_Mono_Op_Stmt (2#010_000#, Stmt, Sz_32h);
               when others =>
                  Error_Emit ("emit_insn: not", Stmt);
            end case;

         when OE_Neg_Ov =>
            case Mode is
               when Mode_I8 =>
                  Emit_Mono_Op_Stmt (2#011_000#, Stmt, Sz_8);
                  --Gen_Into;
               when Mode_I16 =>
                  Emit_Mono_Op_Stmt (2#011_000#, Stmt, Sz_16);
                  --Gen_Into;
               when Mode_I32 =>
                  Emit_Mono_Op_Stmt (2#011_000#, Stmt, Sz_32l);
                  --Gen_Into;
               when Mode_I64 =>
                  Emit_Mono_Op_Stmt (2#011_000#, Stmt, Sz_32l);
                  -- adcl 0, high
                  Start_Insn;
                  Gen_B8 (2#100000_11#);
                  Gen_Rm (2#010_000#, Get_Expr_Operand (Stmt), Sz_32h);
                  Gen_B8 (0);
                  End_Insn;
                  Emit_Mono_Op_Stmt (2#011_000#, Stmt, Sz_32h);
                  --Gen_Into;
               when Mode_F32
                 | Mode_F64 =>
                  --  fchs
                  Start_Insn;
                  Gen_B8 (2#11011_001#);
                  Gen_B8 (2#1110_0000#);
                  End_Insn;
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
                  --  fabs
                  Start_Insn;
                  Gen_B8 (2#11011_001#);
                  Gen_B8 (2#1110_0001#);
                  End_Insn;
               when others =>
                  Error_Emit ("emit_insn: abs_ov", Stmt);
            end case;

         when OE_Kind_Cmp =>
            case Get_Expr_Mode (Get_Expr_Left (Stmt)) is
               when Mode_U32
                 | Mode_I32
                 | Mode_P32 =>
                  Emit_Op (2#111_000#, Stmt, Sz_32l);
               when Mode_B2
                 | Mode_I8
                 | Mode_U8 =>
                  Emit_Op (2#111_000#, Stmt, Sz_8);
               when Mode_U64 =>
                  declare
                     Pc : Pc_Type;
                  begin
                     Emit_Op (2#111_000#, Stmt, Sz_32h);
                     --  jne
                     Start_Insn;
                     Gen_B8 (2#0111_0101#);
                     Gen_B8 (0);
                     End_Insn;
                     Pc := Get_Current_Pc;
                     Emit_Op (2#111_000#, Stmt, Sz_32l);
                     Patch_B8 (Pc - 1, Unsigned_8 (Get_Current_Pc - Pc));
                  end;
               when Mode_I64 =>
                  declare
                     Pc : Pc_Type;
                  begin
                     Reg := Get_Expr_Reg (Stmt);
                     Emit_Op (2#111_000#, Stmt, Sz_32h);
                     --  Note: this does not clobber a reg due to care in
                     --  insns.
                     Emit_Setcc_Reg (Reg, Ekind_Signed_To_Cc (Kind));
                     --  jne
                     Start_Insn;
                     Gen_B8 (2#0111_0101#);
                     Gen_B8 (0);
                     End_Insn;
                     Pc := Get_Current_Pc;
                     Emit_Op (2#111_000#, Stmt, Sz_32l);
                     Emit_Setcc_Reg (Reg, Ekind_Unsigned_To_Cc (Kind));
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
               when Mode_F32 =>
                  Emit_Load_Fp (Stmt, Fp_32);
               when Mode_F64 =>
                  Emit_Load_Fp (Stmt, Fp_64);
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
               when Mode_F32 =>
                  Emit_Load_Fp_Mem (Stmt, Fp_32);
               when Mode_F64 =>
                  Emit_Load_Fp_Mem (Stmt, Fp_64);
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
               when Mode_F32 =>
                  Emit_Store_Fp (Stmt, Fp_32);
               when Mode_F64 =>
                  Emit_Store_Fp (Stmt, Fp_64);
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
               when Mode_F32 =>
                  Emit_Push_Fp (Get_Expr_Operand (Stmt), Fp_32);
               when Mode_F64 =>
                  Emit_Push_Fp (Get_Expr_Operand (Stmt), Fp_64);
               when others =>
                  Error_Emit ("emit_insn: oe_arg", Stmt);
            end case;
         when OE_Stack_Adjust =>
            Emit_Setup_Frame (Stmt);
         when OE_Call =>
            Emit_Call (Stmt);
         when OE_Intrinsic =>
            Emit_Intrinsic (Stmt);

         when OE_Move =>
            declare
               Operand : O_Enode;
               Op_Reg : O_Reg;
            begin
               Reg := Get_Expr_Reg (Stmt);
               Operand := Get_Expr_Operand (Stmt);
               Op_Reg := Get_Expr_Reg (Operand);
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
                     Gen_Insn_Sz (2#1000_101_0#, Sz_32l);
                     Gen_Rm (To_Reg32 (Reg, Sz_32l) * 8, Operand, Sz_32l);
                     End_Insn;
                  when others =>
                     Error_Emit ("emit_insn: move", Stmt);
               end case;
            end;

         when OE_Alloca =>
            if Mode /= Mode_P32 then
               raise Program_Error;
            end if;
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
               when others =>
                  Error_Emit ("emit_insn: spill", Stmt);
            end case;

         when OE_Reload =>
            declare
               Expr : O_Enode;
            begin
               Reg := Get_Expr_Reg (Stmt);
               Expr := Get_Expr_Operand (Stmt);
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
         Start_Insn;
         Gen_B8 (2#01010_000# + To_Reg32 (Reg, Sz_32l));
         End_Insn;
      end if;
   end Push_Reg_If_Used;

   procedure Pop_Reg_If_Used (Reg : Regs_R32)
   is
      use Ortho_Code.X86.Insns;
   begin
      if Reg_Used (Reg) then
         Start_Insn;
         Gen_B8 (2#01011_000# + To_Reg32 (Reg, Sz_32l));
         End_Insn;
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

      Saved_Regs_Size := Boolean'Pos(Reg_Used (R_Di)) * 4
        + Boolean'Pos(Reg_Used (R_Si)) * 4
        + Boolean'Pos(Reg_Used (R_Bx)) * 4;

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
      Start_Insn;
      Gen_B8 (2#01010_101#);
      End_Insn;
      --  movl %esp, %ebp
      Start_Insn;
      Gen_B8 (2#1000100_1#);
      Gen_B8 (2#11_100_101#);
      End_Insn;
      --  subl XXX, %esp
      if Frame_Size /= 0 then
         if not X86.Flags.Flag_Alloca_Call
            or else Frame_Size <= 4096
         then
            Start_Insn;
            if Frame_Size < 128 then
               Gen_B8 (2#100000_11#);
               Gen_B8 (2#11_101_100#);
               Gen_B8 (Byte (Frame_Size));
            else
               Gen_B8 (2#100000_01#);
               Gen_B8 (2#11_101_100#);
               Gen_Le32 (Frame_Size);
            end if;
            End_Insn;
         else
            --  mov stack_size,%eax
            Start_Insn;
            Gen_B8 (2#1011_1_000#);
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
   begin
      --  Restore registers.
      Pop_Reg_If_Used (R_Bx);
      Pop_Reg_If_Used (R_Si);
      Pop_Reg_If_Used (R_Di);

      Decl := Subprg.D_Decl;
      if Get_Decl_Kind (Decl) = OD_Function then
         case Get_Type_Mode (Get_Decl_Type (Decl)) is
            when Mode_U8
              | Mode_B2 =>
               --  movzx %al,%eax
               Start_Insn;
               Gen_B8 (16#0f#);
               Gen_B8 (2#1011_0110#);
               Gen_B8 (2#11_000_000#);
               End_Insn;
            when Mode_U32
              | Mode_I32
              | Mode_U64
              | Mode_I64
              | Mode_F32
              | Mode_F64
              | Mode_P32 =>
               null;
            when others =>
               raise Program_Error;
         end case;
      end if;

      --  leave
      Start_Insn;
      Gen_B8 (2#1100_1001#);
      End_Insn;

      --  ret
      Start_Insn;
      Gen_B8 (2#1100_0011#);
      End_Insn;

      if Flag_Debug = Debug_Dwarf then
         Set_Body_Info (Subprg.D_Body, Int32 (Get_Current_Pc - Subprg_Pc));
      end if;
   end Emit_Epilogue;

   procedure Emit_Subprg (Subprg : Subprogram_Data_Acc)
   is
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

