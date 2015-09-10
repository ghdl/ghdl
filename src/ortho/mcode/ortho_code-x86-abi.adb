--  X86 ABI definitions.
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
with Ortho_Code.Decls; use Ortho_Code.Decls;
with Ortho_Code.Exprs; use Ortho_Code.Exprs;
with Ortho_Code.Consts;
with Ortho_Code.Debug;
with Ortho_Code.Disps;
with Ortho_Code.Flags;
with Ortho_Code.Dwarf;
with Ortho_Code.X86; use Ortho_Code.X86;
with Ortho_Code.X86.Insns;
with Ortho_Code.X86.Emits;
with Ortho_Code.X86.Flags;
with Binary_File;
with Binary_File.Memory;
with Ada.Text_IO;

package body Ortho_Code.X86.Abi is
   procedure Start_Subprogram (Subprg : O_Dnode; Abi : out O_Abi_Subprg)
   is
      pragma Unreferenced (Subprg);
   begin
      --  First argument is at %ebp + 8
      Abi.Offset := 8;
   end Start_Subprogram;

   procedure New_Interface (Inter : O_Dnode; Abi : in out O_Abi_Subprg)
   is
      Itype : O_Tnode;
      Size : Uns32;
   begin
      Itype := Get_Decl_Type (Inter);
      Size := Get_Type_Size (Itype);
      Size := (Size + 3) and not 3;
      Set_Local_Offset (Inter, Abi.Offset);
      Abi.Offset := Abi.Offset + Int32 (Size);
   end New_Interface;

   procedure Finish_Subprogram (Subprg : O_Dnode; Abi : in out O_Abi_Subprg)
   is
      use Binary_File;
      function To_Int32 is new Ada.Unchecked_Conversion
        (Source => Symbol, Target => Int32);
   begin
      Set_Decl_Info (Subprg,
                     To_Int32 (Create_Symbol (Get_Decl_Ident (Subprg))));
      --  Offset is 8 biased.
      Set_Subprg_Stack (Subprg, Abi.Offset - 8);
   end Finish_Subprogram;

   procedure Link_Stmt (Stmt : O_Enode) is
   begin
      Set_Stmt_Link (Last_Link, Stmt);
      Last_Link := Stmt;
   end Link_Stmt;

   procedure Disp_Subprg (Subprg : O_Dnode);


   Exprs_Mark : Exprs.Mark_Type;
   Decls_Mark : Decls.Mark_Type;
   Consts_Mark : Consts.Mark_Type;
   Types_Mark : Types.Mark_Type;
   Dwarf_Mark : Dwarf.Mark_Type;

   procedure Start_Body (Subprg : O_Dnode)
   is
      pragma Unreferenced (Subprg);
   begin
      if not Debug.Flag_Debug_Keep then
         Mark (Exprs_Mark);
         Mark (Decls_Mark);
         Consts.Mark (Consts_Mark);
         Mark (Types_Mark);
      end if;
   end Start_Body;

   procedure Finish_Body (Subprg : Subprogram_Data_Acc)
   is
      use Ortho_Code.Flags;

      Child : Subprogram_Data_Acc;
   begin
      if Debug.Flag_Debug_Hli then
         Disps.Disp_Subprg (Subprg);
         return;
      end if;

      Insns.Gen_Subprg_Insns (Subprg);

      if Ortho_Code.Debug.Flag_Debug_Body2 then
         Disp_Subprg_Body (1, Subprg.E_Entry);
      end if;

      if Ortho_Code.Debug.Flag_Debug_Code then
         Disp_Subprg (Subprg.D_Body);
      end if;

      Emits.Emit_Subprg (Subprg);

      if Get_Decl_Depth (Subprg.D_Decl) = O_Toplevel
        and then Flag_Debug = Debug_Dwarf
      then
         Dwarf.Emit_Decls_Until (Subprg.D_Body);
         if not Debug.Flag_Debug_Keep then
            Dwarf.Mark (Dwarf_Mark);
         end if;
      end if;

      --  Recurse on nested subprograms.
      Child := Subprg.First_Child;
      while Child /= null loop
         Finish_Body (Child);
         Child := Child.Brother;
      end loop;

      if Get_Decl_Depth (Subprg.D_Decl) = O_Toplevel then
         if Flag_Debug = Debug_Dwarf then
            Dwarf.Emit_Subprg (Subprg.D_Body);
         end if;

         if not Debug.Flag_Debug_Keep then
            Release (Exprs_Mark);
            Release (Decls_Mark);
            Consts.Release (Consts_Mark);
            Release (Types_Mark);
            if Flag_Debug = Debug_Dwarf then
               Dwarf.Release (Dwarf_Mark);
            end if;
         end if;
      end if;
   end Finish_Body;

   procedure Expand_Const_Decl (Decl : O_Dnode) is
   begin
      Emits.Emit_Const_Decl (Decl);
   end Expand_Const_Decl;

   procedure Expand_Var_Decl (Decl : O_Dnode) is
   begin
      Emits.Emit_Var_Decl (Decl);
   end Expand_Var_Decl;

   procedure Expand_Const_Value (Decl : O_Dnode; Val : O_Cnode) is
   begin
      Emits.Emit_Const_Value (Decl, Val);
   end Expand_Const_Value;

   procedure Disp_Label (Label : O_Enode)
   is
      use Ada.Text_IO;
      use Ortho_Code.Debug.Int32_IO;
   begin
      Put ("L");
      Put (Int32 (Label), 0);
   end Disp_Label;

   procedure Disp_Reg (Reg : O_Enode)
   is
      use Ada.Text_IO;
      use Ortho_Code.Debug.Int32_IO;
   begin
      Put ("reg_");
      Put (Int32 (Reg), 0);
      Put ("{");
      Put (Image_Reg (Get_Expr_Reg (Reg)));
      Put ("}");
   end Disp_Reg;

   procedure Disp_Local (Stmt : O_Enode)
   is
      use Ada.Text_IO;
      use Ortho_Code.Debug.Int32_IO;
      Obj : constant O_Dnode := Get_Addr_Object (Stmt);
      Frame : constant O_Enode := Get_Addrl_Frame (Stmt);
   begin
      if Frame = O_Enode_Null then
         Put ("fp");
      else
         Disp_Reg (Frame);
      end if;
      Put (",");
      Put (Get_Local_Offset (Obj), 0);
      Put (" {");
      Disp_Decl_Name (Obj);
      Put (":");
      Debug.Disp_Mode (Types.Get_Type_Mode (Get_Decl_Type (Obj)));
      Put ("}");
   end Disp_Local;

   procedure Disp_Uns32 (Val : Uns32)
   is
      use Ada.Text_IO;
      U2c : constant array (Uns32 range 0 .. 15) of Character
        := "0123456789abcdef";
      V : Uns32 := Val;
   begin
      for I in 0 .. 7 loop
         Put (U2c (Shift_Right (V, 28)));
         V := Shift_Left (V, 4);
      end loop;
   end Disp_Uns32;

   procedure Disp_Const (Stmt : O_Enode)
   is
      use Ada.Text_IO;
   begin
      Put ("[");
      case Get_Expr_Mode (Stmt) is
         when Mode_U64
           | Mode_I64
           | Mode_F64 =>
            Disp_Uns32 (Get_Expr_High (Stmt));
            Put (",");
         when others =>
            null;
      end case;
      Disp_Uns32 (Get_Expr_Low (Stmt));
      Put ("]");
   end Disp_Const;

   procedure Disp_Irm_Code (Stmt : O_Enode)
   is
      use Ortho_Code.Debug.Int32_IO;
      use Ada.Text_IO;
      Reg : O_Reg;
      Kind : OE_Kind;
   begin
      Reg := Get_Expr_Reg (Stmt);
      Kind := Get_Expr_Kind (Stmt);
      case Reg is
         when R_Mem =>
            case Kind is
               when OE_Indir =>
                  Put ('(');
                  Disp_Irm_Code (Get_Expr_Operand (Stmt));
                  Put (')');
--                 when OE_Lit =>
--                    Put ("(&n)");
               when others =>
                  raise Program_Error;
            end case;
         when R_Imm =>
            case Kind is
               when OE_Const =>
                  Disp_Const (Stmt);
               when OE_Addrg =>
                  Put ("&");
                  Disp_Decl_Name (Get_Addr_Object (Stmt));
               when OE_Add =>
                  Disp_Irm_Code (Get_Expr_Left (Stmt));
                  Put ("+");
                  Disp_Irm_Code (Get_Expr_Right (Stmt));
               when others =>
                  raise Program_Error;
            end case;
         when Regs_R32
           | R_Any32
           | R_Any8
           | Regs_R64
           | R_Any64
           | Regs_Cc
           | Regs_Fp
           | Regs_Xmm =>
            Disp_Reg (Stmt);
         when R_Spill =>
            Disp_Reg (Stmt);
            --Disp_Irm_Code (Get_Stmt_Link (Stmt));
         when R_B_Off
           | R_I_Off
           | R_B_I
           | R_Sib =>
            case Kind is
               when OE_Addrl =>
                  Disp_Local (Stmt);
               when OE_Add =>
                  Disp_Irm_Code (Get_Expr_Left (Stmt));
                  Put (" + ");
                  Disp_Irm_Code (Get_Expr_Right (Stmt));
               when others =>
                  raise Program_Error;
            end case;
         when R_I =>
            Disp_Irm_Code (Get_Expr_Left (Stmt));
            Put (" * ");
            case Get_Expr_Low (Get_Expr_Right (Stmt)) is
               when 0 =>
                  Put ('1');
               when 1 =>
                  Put ('2');
               when 2 =>
                  Put ('4');
               when 3 =>
                  Put ('8');
               when others =>
                  Put ('?');
            end case;
         when others =>
            Ada.Text_IO.Put_Line
              ("abi.disp_irm_code: unhandled reg=" & Image_Reg (Reg)
               & ", stmt=" & O_Enode'Image (Stmt));
            raise Program_Error;
      end case;
   end Disp_Irm_Code;

   procedure Disp_Decls (Block : O_Dnode)
   is
      Decl : O_Dnode;
      Last : O_Dnode;
   begin
      Last := Get_Block_Last (Block);
      Disp_Decl (2, Block);
      Decl := Block + 1;
      while Decl <= Last loop
         case Get_Decl_Kind (Decl) is
            when OD_Local =>
               Disp_Decl (2, Decl);
            when OD_Block =>
               --  Skip internal blocks.
               Decl := Get_Block_Last (Decl);
            when others =>
               Disp_Decl (2, Decl);
               null;
         end case;
         Decl := Decl + 1;
      end loop;
   end Disp_Decls;

   procedure Disp_Stmt (Stmt : O_Enode)
   is
      use Ada.Text_IO;
      use Debug.Int32_IO;
      Kind : OE_Kind;
      Mode : Mode_Type;

      procedure Disp_Op_Name (Name : String) is
      begin
         Put (Name);
         Put (":");
         Debug.Disp_Mode (Mode);
         Put (" ");
      end Disp_Op_Name;

      procedure Disp_Reg_Op_Name (Name : String) is
      begin
         Put ("  ");
         Disp_Reg (Stmt);
         Put (" = ");
         Disp_Op_Name (Name);
      end Disp_Reg_Op_Name;

   begin
      Kind := Get_Expr_Kind (Stmt);
      Mode := Get_Expr_Mode (Stmt);

      case Kind is
         when OE_Beg =>
            Put ("  # block start");
            if Get_Block_Has_Alloca (Stmt) then
               Put (" [alloca]");
            end if;
            New_Line;
            Disp_Decls (Get_Block_Decls (Stmt));
         when OE_End =>
            Put_Line ("  # block end");
         when OE_Indir =>
            Disp_Reg_Op_Name ("indir");
            Put ("(");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            Put_Line (")");
         when OE_Alloca =>
            Disp_Reg_Op_Name ("alloca");
            Put ("(");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            Put_Line (")");
         when OE_Kind_Cmp
           | OE_Kind_Dyadic =>
            Disp_Reg_Op_Name ("op");
            Put ("{");
            Put (OE_Kind'Image (Kind));
            Put ("} ");
            Disp_Irm_Code (Get_Expr_Left (Stmt));
            Put (", ");
            Disp_Irm_Code (Get_Expr_Right (Stmt));
            New_Line;
         when OE_Abs_Ov
           | OE_Neg_Ov
           | OE_Not =>
            Disp_Reg_Op_Name ("op");
            Put ("{");
            Put (OE_Kind'Image (Kind));
            Put ("} ");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Const =>
            Disp_Reg_Op_Name ("const");
            Disp_Const (Stmt);
            New_Line;
         when OE_Jump_F =>
            Put ("  jump_f ");
            Disp_Reg (Get_Expr_Operand (Stmt));
            Put (" ");
            Disp_Label (Get_Jump_Label (Stmt));
            New_Line;
         when OE_Jump_T =>
            Put ("  jump_t ");
            Disp_Reg (Get_Expr_Operand (Stmt));
            Put (" ");
            Disp_Label (Get_Jump_Label (Stmt));
            New_Line;
         when OE_Jump =>
            Put ("  jump ");
            Disp_Label (Get_Jump_Label (Stmt));
            New_Line;
         when OE_Label =>
            Disp_Label (Stmt);
            Put_Line (":");
         when OE_Asgn =>
            Put ("  assign:");
            Debug.Disp_Mode (Mode);
            Put (" (");
            Disp_Irm_Code (Get_Assign_Target (Stmt));
            Put (") <- ");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Set_Stack =>
            Put ("  set_stack");
            Put (" <- ");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Spill =>
            Disp_Reg_Op_Name ("spill");
            Disp_Reg (Get_Expr_Operand (Stmt));
            Put (", offset=");
            Put (Int32'Image (Get_Spill_Info (Stmt)));
            New_Line;
         when OE_Reload =>
            Disp_Reg_Op_Name ("reload");
            Disp_Reg (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Arg =>
            Put ("  push ");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Call =>
            if Get_Expr_Mode (Stmt) /= Mode_Nil then
               Disp_Reg_Op_Name ("call");
            else
               Put ("  ");
               Disp_Op_Name ("call");
               Put (" ");
            end if;
            Disp_Decl_Name (Get_Call_Subprg (Stmt));
            New_Line;
         when OE_Stack_Adjust =>
            Put ("  stack_adjust: ");
            Put (Int32'Image (Get_Stack_Adjust (Stmt)));
            New_Line;
         when OE_Intrinsic =>
            Disp_Reg_Op_Name ("intrinsic");
            Put (" ");
            case Get_Intrinsic_Operation (Stmt) is
               when Intrinsic_Mul_Ov_U64 =>
                  Put ("mul_ov_U64");
               when Intrinsic_Div_Ov_U64 =>
                  Put ("div_ov_U64");
               when Intrinsic_Mod_Ov_U64 =>
                  Put ("mod_ov_U64");
               when Intrinsic_Mul_Ov_I64 =>
                  Put ("mul_ov_I64");
               when Intrinsic_Div_Ov_I64 =>
                  Put ("div_ov_I64");
               when Intrinsic_Mod_Ov_I64 =>
                  Put ("mod_ov_I64");
               when Intrinsic_Rem_Ov_I64 =>
                  Put ("rem_ov_I64");
               when others =>
                  Put ("??");
            end case;
            --Disp_Decl_Name (Get_Call_Subprg (Stmt));
            New_Line;
         when OE_Conv =>
            Disp_Reg_Op_Name ("conv");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Move =>
            Disp_Reg_Op_Name ("move");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Ret =>
            Put ("  ret");
            if Get_Expr_Mode (Stmt) /= Mode_Nil then
               Put (" ");
               Disp_Reg (Get_Expr_Operand (Stmt));
            end if;
            New_Line;
         when OE_Case =>
            Disp_Reg_Op_Name ("case");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Case_Expr =>
            Disp_Reg_Op_Name ("case_expr");
            Disp_Irm_Code (Get_Expr_Operand (Stmt));
            New_Line;
         when OE_Leave =>
            Put_Line ("leave");
         when OE_Entry =>
            Put_Line ("entry");
         when OE_Line =>
            Put ("  # line #");
            Put (Get_Expr_Line_Number (Stmt), 0);
            New_Line;
         when OE_Addrl =>
            Disp_Reg_Op_Name ("lea{addrl}");
            Put ("(");
            Disp_Local (Stmt);
            Put (")");
            New_Line;
         when OE_Addrg =>
            Disp_Reg_Op_Name ("lea{addrg}");
            Put ("&");
            Disp_Decl_Name (Get_Addr_Object (Stmt));
            New_Line;
         when OE_Add =>
            Disp_Reg_Op_Name ("lea{add}");
            Put ("(");
            Disp_Irm_Code (Get_Expr_Left (Stmt));
            Put (" + ");
            Disp_Irm_Code (Get_Expr_Right (Stmt));
            Put (")");
            New_Line;
         when OE_Mul =>
            Disp_Reg_Op_Name ("mul");
            Disp_Irm_Code (Get_Expr_Left (Stmt));
            Put (", ");
            Disp_Irm_Code (Get_Expr_Right (Stmt));
            New_Line;
         when OE_Shl =>
            Disp_Reg_Op_Name ("shl");
            Disp_Irm_Code (Get_Expr_Left (Stmt));
            Put (", ");
            Disp_Irm_Code (Get_Expr_Right (Stmt));
            New_Line;
         when OE_Reg =>
            Disp_Reg_Op_Name ("reg");
            New_Line;
         when others =>
            Ada.Text_IO.Put_Line
              ("abi.disp_stmt: unhandled enode " & OE_Kind'Image (Kind));
            raise Program_Error;
      end case;
   end Disp_Stmt;

   procedure Disp_Subprg_Decl (Decl : O_Dnode)
   is
      use Ada.Text_IO;
      Arg : O_Dnode;
   begin
      Put ("subprogram ");
      Disp_Decl_Name (Decl);
      Put_Line (":");
      Arg := Decl + 1;
      while Get_Decl_Kind (Arg) = OD_Interface loop
         Disp_Decl (2, Arg);
         Arg := Arg + 1;
      end loop;
   end Disp_Subprg_Decl;

   procedure Disp_Subprg (Subprg : O_Dnode)
   is
      use Ada.Text_IO;

      Stmt : O_Enode;
   begin
      Disp_Subprg_Decl (Get_Body_Decl (Subprg));

      Stmt := Get_Body_Stmt (Subprg);
      loop
         exit when Stmt = O_Enode_Null;
         Disp_Stmt (Stmt);
         exit when Get_Expr_Kind (Stmt) = OE_Leave;
         Stmt := Get_Stmt_Link (Stmt);
      end loop;
   end Disp_Subprg;

   procedure New_Debug_Filename_Decl (Filename : String)
   is
      use Ortho_Code.Flags;
   begin
      if Flag_Debug = Debug_Dwarf then
         Dwarf.Set_Filename ("", Filename);
      end if;
   end New_Debug_Filename_Decl;

   procedure Init
   is
      use Ortho_Code.Debug;
   begin
      --  Alignment of doubles is platform dependent.
      Mode_Align (Mode_F64) := X86.Flags.Mode_F64_Align;

      if Flag_Debug_Hli then
         Disps.Init;
      else
         Emits.Init;
      end if;
   end Init;

   procedure Finish
   is
      use Ortho_Code.Debug;
   begin
      if Flag_Debug_Hli then
         Disps.Finish;
      else
         Emits.Finish;
      end if;
   end Finish;

--    function Image_Insn (Insn : O_Insn) return String is
--    begin
--       case Insn is
--          when Insn_Nil =>
--             return "nil";
--          when Insn_Imm =>
--             return "imm";
--          when Insn_Base_Off =>
--             return "B+O";
--          when Insn_Loadm =>
--             return "ldm";
--          when Insn_Loadi =>
--             return "ldi";
--          when Insn_Mem =>
--             return "mem";
--          when Insn_Cmp =>
--             return "cmp";
--          when Insn_Op =>
--             return "op ";
--          when Insn_Rop =>
--             return "rop";
--          when Insn_Call =>
--             return "cal";
--          when others =>
--             return "???";
--       end case;
--    end Image_Insn;

   function Image_Reg (Reg : O_Reg) return String is
   begin
      case Reg is
         when R_Nil =>
            return "nil ";
         when R_None =>
            return " -- ";
         when R_Spill =>
            return "spil";
         when R_Mem =>
            return "mem ";
         when R_Imm =>
            return "imm ";
         when R_Irm =>
            return "irm ";
         when R_Rm =>
            return "rm  ";
         when R_Sib =>
            return "sib ";
         when R_B_Off =>
            return "b+o ";
         when R_B_I =>
            return "b+i ";
         when R_I =>
            return "s*i ";
         when R_Ir =>
            return " ir ";
         when R_I_Off =>
            return "i+o ";
         when R_Any32 =>
            return "r32 ";
         when R_Any_Cc =>
            return "cc  ";
         when R_Any8 =>
            return "r8  ";
         when R_Any64 =>
            return "r64 ";

         when R_St0 =>
            return "st0 ";
         when R_Ax =>
            return "ax  ";
         when R_Dx =>
            return "dx  ";
         when R_Cx =>
            return "cx  ";
         when R_Bx =>
            return "bx  ";
         when R_Si =>
            return "si  ";
         when R_Di =>
            return "di  ";
         when R_Sp =>
            return "sp  ";
         when R_Bp =>
            return "bp  ";
         when R_Edx_Eax =>
            return "dxax";
         when R_Ebx_Ecx =>
            return "bxcx";
         when R_Esi_Edi =>
            return "sidi";
         when R_Eq =>
            return "eq? ";
         when R_Ne =>
            return "ne? ";
         when R_Uge =>
            return "uge?";
         when R_Sge =>
            return "sge?";
         when R_Ugt =>
            return "ugt?";
         when R_Sgt =>
            return "sgt?";
         when R_Ule =>
            return "ule?";
         when R_Sle =>
            return "sle?";
         when R_Ult =>
            return "ult?";
         when R_Slt =>
            return "slt?";
         when R_Xmm0 =>
            return "xmm0";
         when R_Xmm1 =>
            return "xmm1";
         when R_Xmm2 =>
            return "xmm2";
         when R_Xmm3 =>
            return "xmm3";
         when others =>
            return "????";
      end case;
   end Image_Reg;

   --  From GCC.
   --  FIXME: these don't handle overflow!
   function Divdi3 (A, B : Long_Integer) return Long_Integer;
   pragma Import (C, Divdi3, "__divdi3");

   function Muldi3 (A, B : Long_Integer) return Long_Integer;
   pragma Import (C, Muldi3, "__muldi3");

   function Moddi3 (A, B : Long_Integer) return Long_Integer;
   pragma Import (C, Moddi3, "__moddi3");

   procedure Chkstk (Sz : Integer);
   pragma Import (C, Chkstk, "__chkstk");

   procedure Link_Intrinsics
   is
   begin
      Binary_File.Memory.Set_Symbol_Address
        (Ortho_Code.X86.Emits.Intrinsics_Symbol
         (Ortho_Code.X86.Intrinsic_Mul_Ov_I64),
         Muldi3'Address);
      Binary_File.Memory.Set_Symbol_Address
        (Ortho_Code.X86.Emits.Intrinsics_Symbol
         (Ortho_Code.X86.Intrinsic_Div_Ov_I64),
         Divdi3'Address);
      Binary_File.Memory.Set_Symbol_Address
        (Ortho_Code.X86.Emits.Intrinsics_Symbol
         (Ortho_Code.X86.Intrinsic_Mod_Ov_I64),
         Moddi3'Address);
      if X86.Flags.Flag_Alloca_Call then
         Binary_File.Memory.Set_Symbol_Address
           (Ortho_Code.X86.Emits.Chkstk_Symbol, Chkstk'Address);
      end if;
   end Link_Intrinsics;
end Ortho_Code.X86.Abi;
