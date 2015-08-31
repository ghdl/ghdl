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
with Interfaces;
with Ada.Text_IO;
with Ortho_Code.Abi;
with Ortho_Code.Decls; use Ortho_Code.Decls;
with Ortho_Code.Types; use Ortho_Code.Types;
with Ortho_Code.Debug;
with Ortho_Code.X86.Flags;

package body Ortho_Code.X86.Insns is
   procedure Link_Stmt (Stmt : O_Enode)
   is
      use Ortho_Code.Abi;
   begin
      Set_Stmt_Link (Last_Link, Stmt);
      Last_Link := Stmt;
      if Debug.Flag_Debug_Insn then
         Disp_Stmt (Stmt);
      end if;
   end Link_Stmt;

   function Get_Reg_Any (Mode : Mode_Type) return O_Reg is
   begin
      case Mode is
         when Mode_I16 .. Mode_I32
           | Mode_U16 .. Mode_U32
           | Mode_P32 =>
            return R_Any32;
         when Mode_I8
           | Mode_U8
           | Mode_B2 =>
            return R_Any8;
         when Mode_U64
           | Mode_I64 =>
            return R_Any64;
         when Mode_F32
           | Mode_F64 =>
            if Abi.Flag_Sse2 then
               return R_Any_Xmm;
            else
               return R_St0;
            end if;
         when Mode_P64
           | Mode_X1
           | Mode_Nil
           | Mode_Blk =>
            raise Program_Error;
      end case;
   end Get_Reg_Any;

   function Get_Reg_Any (Stmt : O_Enode) return O_Reg is
   begin
      return Get_Reg_Any (Get_Expr_Mode (Stmt));
   end Get_Reg_Any;

   --  Stack slot management.
   Stack_Offset : Uns32 := 0;
   Stack_Max : Uns32 := 0;

   --  Count how many bytes have been pushed on the stack, during a call. This
   --  is used to correctly align the stack for nested calls.
   Push_Offset : Uns32 := 0;

   --  STMT is an OE_END statement.
   --  Swap Stack_Offset with Max_Stack of STMT.
   procedure Swap_Stack_Offset (Blk : O_Dnode)
   is
      Prev_Offset : Uns32;
   begin
      Prev_Offset := Get_Block_Max_Stack (Blk);
      Set_Block_Max_Stack (Blk, Stack_Offset);
      Stack_Offset := Prev_Offset;
   end Swap_Stack_Offset;

   procedure Expand_Decls (Block : O_Dnode)
   is
      Last : O_Dnode;
      Decl : O_Dnode;
      Decl_Type : O_Tnode;
   begin
      if Get_Decl_Kind (Block) /= OD_Block then
         raise Program_Error;
      end if;
      Last := Get_Block_Last (Block);
      Decl := Block + 1;
      while Decl <= Last loop
         case Get_Decl_Kind (Decl) is
            when OD_Local =>
               Decl_Type := Get_Decl_Type (Decl);
               Stack_Offset := Do_Align (Stack_Offset, Decl_Type);
               Stack_Offset := Stack_Offset + Get_Type_Size (Decl_Type);
               Set_Local_Offset (Decl, -Int32 (Stack_Offset));
               if Stack_Offset > Stack_Max then
                  Stack_Max := Stack_Offset;
               end if;
            when OD_Type
              | OD_Const
              | OD_Const_Val
              | OD_Var
              | OD_Function
              | OD_Procedure
              | OD_Interface
              | OD_Body
              | OD_Subprg_Ext =>
               null;
            when OD_Block =>
               Decl := Get_Block_Last (Decl);
         end case;
         Decl := Decl + 1;
      end loop;
   end Expand_Decls;

   function Ekind_To_Cc (Stmt : O_Enode; Mode : Mode_Type) return O_Reg
   is
      Kind : OE_Kind;
   begin
      Kind := Get_Expr_Kind (Stmt);
      case Mode is
         when Mode_U8 .. Mode_U64
           | Mode_F32 .. Mode_F64
           | Mode_P32
           | Mode_P64
           | Mode_B2 =>
            return Ekind_Unsigned_To_Cc (Kind);
         when Mode_I8 .. Mode_I64 =>
            return Ekind_Signed_To_Cc (Kind);
         when others =>
            raise Program_Error;
      end case;
   end Ekind_To_Cc;

   --  CC is the result of A CMP B.
   --  Returns the condition for B CMP A.
   function Reverse_Cc (Cc : O_Reg) return O_Reg is
   begin
      case Cc is
         when R_Ult =>
            return R_Ugt;
         when R_Uge =>
            return R_Ule;
         when R_Eq =>
            return R_Eq;
         when R_Ne =>
            return R_Ne;
         when R_Ule =>
            return R_Uge;
         when R_Ugt =>
            return R_Ult;
         when R_Slt =>
            return R_Sgt;
         when R_Sge =>
            return R_Sle;
         when R_Sle =>
            return R_Sge;
         when R_Sgt =>
            return R_Slt;
         when others =>
            raise Program_Error;
      end case;
   end Reverse_Cc;

   --  Get the register in which a result of MODE is returned.
   function Get_Call_Register (Mode : Mode_Type) return O_Reg is
   begin
      case Mode is
         when Mode_U8 .. Mode_U32
           | Mode_I8 .. Mode_I32
           | Mode_P32
           | Mode_B2 =>
            return R_Ax;
         when Mode_U64
           | Mode_I64 =>
            return R_Edx_Eax;
         when Mode_F32
           | Mode_F64 =>
            if Abi.Flag_Sse2 and True then
               --  Note: this shouldn't be enabled as the svr4 ABI specifies
               --  ST0.
               return R_Xmm0;
            else
               return R_St0;
            end if;
         when Mode_Nil =>
            return R_None;
         when Mode_X1
           | Mode_Blk
           | Mode_P64 =>
            raise Program_Error;
      end case;
   end Get_Call_Register;

--    function Ensure_Rm (Stmt : O_Enode) return O_Enode
--    is
--    begin
--       case Get_Expr_Reg (Stmt) is
--          when R_Mem
--            | Regs_Any32 =>
--             return Stmt;
--          when others =>
--             raise Program_Error;
--       end case;
--    end Ensure_Rm;

--    function Ensure_Ireg (Stmt : O_Enode) return O_Enode
--    is
--       Reg : O_Reg;
--    begin
--       Reg := Get_Expr_Reg (Stmt);
--       case Reg is
--          when Regs_Any32
--            | R_Imm =>
--             return Stmt;
--          when others =>
--             raise Program_Error;
--       end case;
--    end Ensure_Ireg;

   function Insert_Move (Expr : O_Enode; Dest : O_Reg) return O_Enode
   is
      N : O_Enode;
   begin
      N := New_Enode (OE_Move, Get_Expr_Mode (Expr), O_Tnode_Null,
                      Expr, O_Enode_Null);
      Set_Expr_Reg (N, Dest);
      Link_Stmt (N);
      return N;
   end Insert_Move;

--     function Insert_Spill (Expr : O_Enode) return O_Enode
--     is
--        N : O_Enode;
--     begin
--        N := New_Enode (OE_Spill, Get_Expr_Mode (Expr), O_Tnode_Null,
--                        Expr, O_Enode_Null);
--        Set_Expr_Reg (N, R_Spill);
--        Link_Stmt (N);
--        return N;
--     end Insert_Spill;

   procedure Error_Gen_Insn (Stmt : O_Enode; Reg : O_Reg)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("gen_insn error: cannot match reg " & Abi.Image_Reg (Reg)
                & " with stmt " & OE_Kind'Image (Get_Expr_Kind (Stmt)));
      raise Program_Error;
   end Error_Gen_Insn;

   procedure Error_Gen_Insn (Stmt : O_Enode; Mode : Mode_Type)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("gen_insn error: cannot match mode " & Mode_Type'Image (Mode)
                & " with stmt " & OE_Kind'Image (Get_Expr_Kind (Stmt))
                & " of mode " & Mode_Type'Image (Get_Expr_Mode (Stmt)));
      raise Program_Error;
   end Error_Gen_Insn;

   pragma No_Return (Error_Gen_Insn);

   Cur_Block : O_Enode;

   type O_Inum is new Int32;
   O_Free : constant O_Inum := 0;
   O_Iroot : constant O_Inum := 1;


   Insn_Num : O_Inum;

   function Get_Insn_Num return O_Inum is
   begin
      Insn_Num := Insn_Num + 1;
      return Insn_Num;
   end Get_Insn_Num;


   type Reg_Info_Type is record
      --  Statement number which use this register.
      --  This is a distance.
      Num : O_Inum;

      --  Statement which produces this value.
      --  Used to have more info on this register (such as mode to allocate
      --   a spill location).
      Stmt : O_Enode;

      --  If set, this register has been used.
      --  All callee-saved registers marked must be saved.
      Used : Boolean;
   end record;

   Init_Reg_Info : constant Reg_Info_Type := (Num => O_Free,
                                              Stmt => O_Enode_Null,
                                              Used => False);
   type Reg32_Info_Array is array (Regs_R32) of Reg_Info_Type;
   Regs : Reg32_Info_Array := (others => Init_Reg_Info);

   Reg_Cc : Reg_Info_Type := Init_Reg_Info;

   type Fp_Stack_Type is mod 8;
   type RegFp_Info_Array is array (Fp_Stack_Type) of Reg_Info_Type;
   Fp_Top : Fp_Stack_Type := 0;
   Fp_Regs : RegFp_Info_Array;

   type Reg_Xmm_Info_Array is array (Regs_Xmm) of Reg_Info_Type;
   Info_Regs_Xmm : Reg_Xmm_Info_Array := (others => Init_Reg_Info);

   function Reg_Used (Reg : Regs_R32) return Boolean is
   begin
      return Regs (Reg).Used;
   end Reg_Used;

   procedure Dump_Reg32_Info (Reg : Regs_R32)
   is
      use Ada.Text_IO;
      use Ortho_Code.Debug.Int32_IO;
      use Abi;
   begin
      Put (Image_Reg (Reg));
      Put (": ");
      Put (Int32 (Regs (Reg).Stmt), 0);
      Put (", num: ");
      Put (Int32 (Regs (Reg).Num), 0);
      --Put (", twin: ");
      --Put (Image_Reg (Regs (Reg).Twin_Reg));
      --Put (", link: ");
      --Put (Image_Reg (Regs (Reg).Link));
      New_Line;
   end Dump_Reg32_Info;

   procedure Dump_Regs
   is
      use Ada.Text_IO;
      use Debug.Int32_IO;
   begin
--        Put ("free_regs: ");
--        Put (Image_Reg (Free_Regs));
--        Put (", to_free_regs: ");
--        Put (Image_Reg (To_Free_Regs));
--        New_Line;

      for I in Regs_R32 loop
         Dump_Reg32_Info (I);
      end loop;
      for I in Fp_Stack_Type loop
         Put ("fp" & Fp_Stack_Type'Image (I));
         Put (": ");
         Put (Int32 (Fp_Regs (I).Stmt), 0);
         New_Line;
      end loop;
   end Dump_Regs;

   pragma Unreferenced (Dump_Regs);

   procedure Error_Reg (Msg : String; Stmt : O_Enode; Reg : O_Reg)
   is
      use Ada.Text_IO;
      use Ortho_Code.Debug.Int32_IO;
   begin
      Put ("error reg: ");
      Put (Msg);
      New_Line;
      Put (" stmt: ");
      Put (Int32 (Stmt), 0);
      Put (", reg: ");
      Put (Abi.Image_Reg (Reg));
      New_Line;
      --Dump_Regs;
      raise Program_Error;
   end Error_Reg;
   pragma No_Return (Error_Reg);

   --  Free_XX
   --  Mark a register as unused.
   procedure Free_R32 (Reg : O_Reg) is
   begin
      if Regs (Reg).Num = O_Free then
         raise Program_Error;
      end if;
      Regs (Reg).Num := O_Free;
   end Free_R32;

   procedure Free_Fp is
   begin
      if Fp_Regs (Fp_Top).Stmt = O_Enode_Null then
         raise Program_Error;
      end if;
      Fp_Regs (Fp_Top).Stmt := O_Enode_Null;
      Fp_Top := Fp_Top + 1;
   end Free_Fp;

   procedure Free_Cc is
   begin
      if Reg_Cc.Num = O_Free then
         raise Program_Error;
      end if;
      Reg_Cc.Num := O_Free;
   end Free_Cc;

   procedure Free_Xmm (Reg : O_Reg) is
   begin
      if Info_Regs_Xmm (Reg).Num = O_Free then
         raise Program_Error;
      end if;
      Info_Regs_Xmm (Reg).Num := O_Free;
   end Free_Xmm;

   --  Allocate a stack slot for spilling.
   procedure Alloc_Spill (N : O_Enode)
   is
      Mode : Mode_Type;
   begin
      Mode := Get_Expr_Mode (N);
      --  Allocate on the stack.
      Stack_Offset := Types.Do_Align (Stack_Offset, Mode);
      Stack_Offset := Stack_Offset + Types.Get_Mode_Size (Mode);
      if Stack_Offset > Stack_Max then
         Stack_Max := Stack_Offset;
      end if;
      Set_Spill_Info (N, -Int32 (Stack_Offset));
   end Alloc_Spill;

   --  Insert a spill statement after ORIG: will save register(s) allocated by
   --  ORIG.
   --  Return the register(s) spilt (There might be several registers if
   --   ORIG uses a R64 register).
   function Insert_Spill (Orig : O_Enode) return O_Reg
   is
      N : O_Enode;
      Mode : Mode_Type;
      Reg_Orig : O_Reg;
   begin
      --  Add a spill statement.
      Mode := Get_Expr_Mode (Orig);
      N := New_Enode (OE_Spill, Mode, O_Tnode_Null, Orig, O_Enode_Null);
      Alloc_Spill (N);

      --  Insert the statement after the one that set the register
      --  being spilled.
      --  That's very important to be able to easily find the spill location,
      --  when it will be reloaded.
      if Orig = Abi.Last_Link then
         Link_Stmt (N);
      else
         Set_Stmt_Link (N, Get_Stmt_Link (Orig));
         Set_Stmt_Link (Orig, N);
      end if;
      Reg_Orig := Get_Expr_Reg (Orig);
      Set_Expr_Reg (N, Reg_Orig);
      Set_Expr_Reg (Orig, R_Spill);
      return Reg_Orig;
   end Insert_Spill;

   procedure Spill_R32 (Reg : Regs_R32)
   is
      Reg_Orig : O_Reg;
   begin
      if Regs (Reg).Num = O_Free then
         --  This register was not allocated.
         raise Program_Error;
      end if;

      Reg_Orig := Insert_Spill (Regs (Reg).Stmt);

      --  Free the register.
      case Reg_Orig is
         when Regs_R32 =>
            if Reg_Orig /= Reg then
               raise Program_Error;
            end if;
            Free_R32 (Reg);
         when Regs_R64 =>
            --  The pair was spilled, so the pair is free.
            Free_R32 (Get_R64_High (Reg_Orig));
            Free_R32 (Get_R64_Low (Reg_Orig));
         when others =>
            raise Program_Error;
      end case;
   end Spill_R32;

   procedure Alloc_R32 (Reg : O_Reg; Stmt : O_Enode; Num : O_Inum) is
   begin
      if Regs (Reg).Num /= O_Free then
         Spill_R32 (Reg);
      end if;
      Regs (Reg) := (Num => Num, Stmt => Stmt, Used => True);
   end Alloc_R32;

   procedure Clobber_R32 (Reg : O_Reg) is
   begin
      if Regs (Reg).Num /= O_Free then
         Spill_R32 (Reg);
      end if;
   end Clobber_R32;

   procedure Alloc_Fp (Stmt : O_Enode)
   is
   begin
      Fp_Top := Fp_Top - 1;

      if Fp_Regs (Fp_Top).Stmt /= O_Enode_Null then
         --  Must spill-out.
         raise Program_Error;
      end if;
      Fp_Regs (Fp_Top).Stmt := Stmt;
   end Alloc_Fp;

   procedure Alloc_R64 (Reg : O_Reg; Stmt : O_Enode; Num : O_Inum)
   is
      Rh, Rl : O_Reg;
   begin
      Rl := Get_R64_Low (Reg);
      Rh := Get_R64_High (Reg);
      if Regs (Rl).Num /= O_Free
        or Regs (Rh).Num /= O_Free
      then
         Spill_R32 (Rl);
      end if;
      Regs (Rh) := (Num => Num, Stmt => Stmt, Used => True);
      Regs (Rl) := (Num => Num, Stmt => Stmt, Used => True);
   end Alloc_R64;

   procedure Alloc_Cc (Stmt : O_Enode; Num : O_Inum) is
   begin
      if Reg_Cc.Num /= O_Free then
         raise Program_Error;
      end if;
      Reg_Cc := (Num => Num, Stmt => Stmt, Used => True);
   end Alloc_Cc;

   procedure Spill_Xmm (Reg : Regs_Xmm)
   is
      Reg_Orig : O_Reg;
   begin
      if Info_Regs_Xmm (Reg).Num = O_Free then
         --  This register was not allocated.
         raise Program_Error;
      end if;

      Reg_Orig := Insert_Spill (Info_Regs_Xmm (Reg).Stmt);

      --  Free the register.
      if Reg_Orig /= Reg then
         raise Program_Error;
      end if;
      Free_Xmm (Reg);
   end Spill_Xmm;

   procedure Alloc_Xmm (Reg : Regs_Xmm; Stmt : O_Enode; Num : O_Inum) is
   begin
      if Info_Regs_Xmm (Reg).Num /= O_Free then
         Spill_Xmm (Reg);
      end if;
      Info_Regs_Xmm (Reg) := (Num => Num, Stmt => Stmt, Used => True);
   end Alloc_Xmm;

   procedure Clobber_Xmm (Reg : Regs_Xmm) is
   begin
      if Info_Regs_Xmm (Reg).Num /= O_Free then
         Spill_Xmm (Reg);
      end if;
   end Clobber_Xmm;
   pragma Unreferenced (Clobber_Xmm);

   function Alloc_Reg (Reg : O_Reg; Stmt : O_Enode; Num : O_Inum) return O_Reg
   is
      Best_Reg : O_Reg;
      Best_Num : O_Inum;
   begin
      case Reg is
         when Regs_R32 =>
            Alloc_R32 (Reg, Stmt, Num);
            return Reg;
         when Regs_R64 =>
            Alloc_R64 (Reg, Stmt, Num);
            return Reg;
         when R_St0 =>
            Alloc_Fp (Stmt);
            return Reg;
         when Regs_Xmm =>
            Alloc_Xmm (Reg, Stmt, Num);
            return Reg;
         when R_Any32 =>
            Best_Num := O_Inum'Last;
            Best_Reg := R_None;
            for I in Regs_R32 loop
               if I not in R_Sp .. R_Bp then
                  if Regs (I).Num = O_Free then
                     Alloc_R32 (I, Stmt, Num);
                     return I;
                  elsif Regs (I).Num <= Best_Num then
                     Best_Reg := I;
                     Best_Num := Regs (I).Num;
                  end if;
               end if;
            end loop;
            Alloc_R32 (Best_Reg, Stmt, Num);
            return Best_Reg;
         when R_Any8 =>
            Best_Num := O_Inum'Last;
            Best_Reg := R_None;
            for I in Regs_R8 loop
               if Regs (I).Num = O_Free then
                  Alloc_R32 (I, Stmt, Num);
                  return I;
               elsif Regs (I).Num <= Best_Num then
                  Best_Reg := I;
                  Best_Num := Regs (I).Num;
               end if;
            end loop;
            Alloc_R32 (Best_Reg, Stmt, Num);
            return Best_Reg;
         when R_Any64 =>
            declare
               Rh, Rl : O_Reg;
            begin
               Best_Num := O_Inum'Last;
               Best_Reg := R_None;
               for I in Regs_R64 loop
                  Rh := Get_R64_High (I);
                  Rl := Get_R64_Low (I);
                  if Regs (Rh).Num = O_Free
                    and then Regs (Rl).Num = O_Free
                  then
                     Alloc_R64 (I, Stmt, Num);
                     return I;
                  elsif Regs (Rh).Num <= Best_Num
                    and Regs (Rl).Num <= Best_Num
                  then
                     Best_Reg := I;
                     Best_Num := O_Inum'Max (Regs (Rh).Num,
                                             Regs (Rl).Num);
                  end if;
               end loop;
               Alloc_R64 (Best_Reg, Stmt, Num);
               return Best_Reg;
            end;
         when R_Any_Xmm =>
            Best_Num := O_Inum'Last;
            Best_Reg := R_None;
            for I in Regs_X86_Xmm loop
               if Info_Regs_Xmm (I).Num = O_Free then
                  Alloc_Xmm (I, Stmt, Num);
                  return I;
               elsif Info_Regs_Xmm (I).Num <= Best_Num then
                  Best_Reg := I;
                  Best_Num := Info_Regs_Xmm (I).Num;
               end if;
            end loop;
            Alloc_Xmm (Best_Reg, Stmt, Num);
            return Best_Reg;
         when others =>
            Error_Reg ("alloc_reg: unknown reg", O_Enode_Null, Reg);
            raise Program_Error;
      end case;
   end Alloc_Reg;

   function Gen_Reload (Spill : O_Enode; Reg : O_Reg; Num : O_Inum)
                       return O_Enode
   is
      N : O_Enode;
      Mode : Mode_Type;
   begin
      --  Add a reload node.
      Mode := Get_Expr_Mode (Spill);
      N := New_Enode (OE_Reload, Mode, O_Tnode_Null, Spill, O_Enode_Null);
      --  Note: this does not use a just-freed register, since
      --  this case only occurs at the first call.
      Set_Expr_Reg (N, Alloc_Reg (Reg, N, Num));
      Link_Stmt (N);
      return N;
   end Gen_Reload;

   function Reload (Expr : O_Enode; Dest : O_Reg; Num : O_Inum) return O_Enode
   is
      Reg : O_Reg;
      Spill : O_Enode;
   begin
      Reg := Get_Expr_Reg (Expr);
      case Reg is
         when R_Spill =>
            --  Restore the register between the statement and the spill.
            Spill := Get_Stmt_Link (Expr);
            Set_Expr_Reg (Expr, Get_Expr_Reg (Spill));
            Set_Expr_Reg (Spill, R_Spill);
            case Dest is
               when R_Mem
                 | R_Irm
                 | R_Rm =>
                  return Spill;
               when Regs_R32
                 | R_Any32
                 | Regs_R64
                 | R_Any64
                 | R_Any8 =>
                  return Gen_Reload (Spill, Dest, Num);
               when R_Sib =>
                  return Gen_Reload (Spill, R_Any32, Num);
               when R_Ir =>
                  return Gen_Reload (Spill, Get_Reg_Any (Expr), Num);
               when others =>
                  Error_Reg ("reload: unhandled dest in spill", Expr, Dest);
            end case;
         when Regs_R32 =>
            case Dest is
               when R_Irm
                 | R_Rm
                 | R_Ir
                 | R_Any32
                 | R_Any8
                 | R_Sib =>
                  return Expr;
               when Regs_R32 =>
                  if Dest = Reg then
                     return Expr;
                  end if;
                  Free_R32 (Reg);
                  Spill := Insert_Move (Expr, Dest);
                  Alloc_R32 (Dest, Spill, Num);
                  return Spill;
               when others =>
                  Error_Reg ("reload: unhandled dest in R32", Expr, Dest);
            end case;
         when Regs_R64 =>
            return Expr;
         when R_St0 =>
            return Expr;
         when Regs_Xmm =>
            return Expr;
         when R_Mem =>
            if Get_Expr_Kind (Expr) = OE_Indir then
               Set_Expr_Operand (Expr,
                                 Reload (Get_Expr_Operand (Expr), R_Sib, Num));
               return Expr;
            else
               raise Program_Error;
            end if;
         when R_B_Off
           | R_B_I
           | R_I_Off
           | R_Sib =>
            case Get_Expr_Kind (Expr) is
               when OE_Add =>
                  Set_Expr_Left
                    (Expr, Reload (Get_Expr_Left (Expr), R_Any32, Num));
                  Set_Expr_Right
                    (Expr, Reload (Get_Expr_Right (Expr), R_Any32, Num));
                  return Expr;
               when OE_Addrl =>
                  Spill := Get_Addrl_Frame (Expr);
                  if Spill /= O_Enode_Null then
                     Set_Addrl_Frame (Expr, Reload (Spill, R_Any32, Num));
                  end if;
                  return Expr;
               when others =>
                  Error_Reg ("reload: unhandle expr in b_off", Expr, Dest);
            end case;
         when R_I =>
            Set_Expr_Left (Expr, Reload (Get_Expr_Left (Expr), R_Any32, Num));
            return Expr;
         when R_Imm =>
            return Expr;
         when others =>
            Error_Reg ("reload: unhandled reg", Expr, Reg);
      end case;
   end Reload;

   procedure Renum_Reg (Reg : O_Reg; Stmt : O_Enode; Num : O_Inum) is
   begin
      case Reg is
         when Regs_R32 =>
            Regs (Reg).Num := Num;
            Regs (Reg).Stmt := Stmt;
         when Regs_Cc =>
            Reg_Cc.Num := Num;
            Reg_Cc.Stmt := Stmt;
         when R_St0 =>
            null;
         when Regs_R64 =>
            declare
               L, H : O_Reg;
            begin
               L := Get_R64_Low (Reg);
               Regs (L).Num := Num;
               Regs (L).Stmt := Stmt;
               H := Get_R64_High (Reg);
               Regs (H).Num := Num;
               Regs (H).Stmt := Stmt;
            end;
         when others =>
            Error_Reg ("renum_reg", Stmt, Reg);
      end case;
   end Renum_Reg;

   procedure Free_Insn_Regs (Insn : O_Enode)
   is
      R : O_Reg;
   begin
      R := Get_Expr_Reg (Insn);
      case R is
         when R_Ax
           | R_Bx
           | R_Cx
           | R_Dx
           | R_Si
           | R_Di =>
            Free_R32 (R);
         when R_Sp
           | R_Bp =>
            null;
         when R_St0 =>
            Free_Fp;
         when Regs_Xmm =>
            Free_Xmm (R);
         when Regs_R64 =>
            Free_R32 (Get_R64_High (R));
            Free_R32 (Get_R64_Low (R));
         when R_Mem =>
            if Get_Expr_Kind (Insn) = OE_Indir then
               Free_Insn_Regs (Get_Expr_Operand (Insn));
            else
               raise Program_Error;
            end if;
         when R_B_Off
           | R_B_I
           | R_I_Off
           | R_Sib =>
            case Get_Expr_Kind (Insn) is
               when OE_Add =>
                  Free_Insn_Regs (Get_Expr_Left (Insn));
                  Free_Insn_Regs (Get_Expr_Right (Insn));
               when OE_Addrl =>
                  if Get_Addrl_Frame (Insn) /= O_Enode_Null then
                     Free_Insn_Regs (Get_Addrl_Frame (Insn));
                  end if;
               when others =>
                  raise Program_Error;
            end case;
         when R_I =>
            Free_Insn_Regs (Get_Expr_Left (Insn));
         when R_Imm =>
            null;
         when R_Spill =>
            null;
         when others =>
            Error_Reg ("free_insn_regs: unknown reg", Insn, R);
      end case;
   end Free_Insn_Regs;

   procedure Insert_Reg (Mode : Mode_Type)
   is
      N : O_Enode;
      Num : O_Inum;
   begin
      Num := Get_Insn_Num;
      N := New_Enode (OE_Reg, Mode, O_Tnode_Null,
                      O_Enode_Null, O_Enode_Null);
      Set_Expr_Reg (N, Alloc_Reg (Get_Reg_Any (Mode), N, Num));
      Link_Stmt (N);
      Free_Insn_Regs (N);
   end Insert_Reg;

   procedure Insert_Arg (Expr : O_Enode)
   is
      N : O_Enode;
   begin
      Free_Insn_Regs (Expr);
      N := New_Enode (OE_Arg, Get_Expr_Mode (Expr), O_Tnode_Null,
                      Expr, O_Enode_Null);
      Set_Expr_Reg (N, R_None);
      Link_Stmt (N);
   end Insert_Arg;

   function Insert_Intrinsic (Stmt : O_Enode; Reg : O_Reg; Num : O_Inum)
                             return O_Enode
   is
      N : O_Enode;
      Op : Int32;
      Mode : Mode_Type;
   begin
      Mode := Get_Expr_Mode (Stmt);
      case Get_Expr_Kind (Stmt) is
         when OE_Mul_Ov =>
            case Mode is
               when Mode_U64 =>
                  Op := Intrinsic_Mul_Ov_U64;
               when Mode_I64 =>
                  Op := Intrinsic_Mul_Ov_I64;
               when others =>
                  raise Program_Error;
            end case;
         when OE_Div_Ov =>
            case Mode is
               when Mode_U64 =>
                  Op := Intrinsic_Div_Ov_U64;
               when Mode_I64 =>
                  Op := Intrinsic_Div_Ov_I64;
               when others =>
                  raise Program_Error;
            end case;
         when OE_Mod =>
            case Mode is
               when Mode_U64 =>
                  Op := Intrinsic_Mod_Ov_U64;
               when Mode_I64 =>
                  Op := Intrinsic_Mod_Ov_I64;
               when others =>
                  raise Program_Error;
            end case;
         when OE_Rem =>
            case Mode is
               when Mode_U64 =>
                  --  For unsigned, MOD == REM.
                  Op := Intrinsic_Mod_Ov_U64;
               when Mode_I64 =>
                  Op := Intrinsic_Rem_Ov_I64;
               when others =>
                  raise Program_Error;
            end case;
         when others =>
            raise Program_Error;
      end case;

      --  Save caller-saved registers.
      Clobber_R32 (R_Ax);
      Clobber_R32 (R_Dx);
      Clobber_R32 (R_Cx);

      N := New_Enode (OE_Intrinsic, Mode, O_Tnode_Null,
                      O_Enode (Op), O_Enode_Null);
      Set_Expr_Reg (N, Alloc_Reg (Reg, N, Num));
      Link_Stmt (N);
      return N;
   end Insert_Intrinsic;

   --  REG is mandatory: the result of STMT must satisfy the REG constraint.
   function Gen_Insn (Stmt : O_Enode; Reg : O_Reg; Pnum : O_Inum)
                     return O_Enode;

   function Gen_Conv_From_Fp_Insn (Stmt : O_Enode;
                                   Reg : O_Reg;
                                   Pnum : O_Inum)
                                  return O_Enode
   is
      Num : O_Inum;
      Left : O_Enode;
   begin
      Left := Get_Expr_Operand (Stmt);
      Num := Get_Insn_Num;
      Left := Gen_Insn (Left, R_St0, Num);
      Free_Insn_Regs (Left);
      Set_Expr_Operand (Stmt, Left);
      case Reg is
         when Regs_R32
           | R_Any32
           | Regs_R64
           | R_Any64 =>
            Set_Expr_Reg (Stmt, Alloc_Reg (Reg, Stmt, Pnum));
         when R_Rm
           | R_Irm
           | R_Ir =>
            Set_Expr_Reg (Stmt, Alloc_Reg (Get_Reg_Any (Stmt), Stmt, Pnum));
         when others =>
            raise Program_Error;
      end case;
      Link_Stmt (Stmt);
      return Stmt;
--                             declare
--                                Spill : O_Enode;
--                             begin
--                                Num := Get_Insn_Num;
--                                Left := Gen_Insn (Left, R_St0, Num);
--                                Set_Expr_Operand (Stmt, Left);
--                                Set_Expr_Reg (Stmt, R_Spill);
--                                Free_Insn_Regs (Left);
--                                Link_Stmt (Stmt);
--                                Spill := Insert_Spill (Stmt);
--                                case Reg is
--                                   when R_Any32
--                                     | Regs_R32 =>
--                                      return Gen_Reload (Spill, Reg, Pnum);
--                                   when R_Ir =>
--                                    return Gen_Reload (Spill, R_Any32, Pnum);
--                                   when R_Rm
--                                     | R_Irm =>
--                                      return Spill;
--                                   when others =>
--                                      Error_Reg
--                                        ("gen_insn:oe_conv(fp)", Stmt, Reg);
--                                end case;
--                             end;
   end Gen_Conv_From_Fp_Insn;

   function Gen_Call (Stmt : O_Enode; Reg : O_Reg; Pnum : O_Inum)
                     return O_Enode
   is
      use Interfaces;
      Left : O_Enode;
      Reg_Res : O_Reg;
      Subprg : O_Dnode;
      Push_Size : Uns32;
      Pad : Uns32;
      Res_Stmt : O_Enode;
   begin
      --  Emit Setup_Frame (to align stack).
      Subprg := Get_Call_Subprg (Stmt);
      Push_Size := Uns32 (Get_Subprg_Stack (Subprg));
      --  Pad the stack if necessary.
      Pad := (Push_Size + Push_Offset) and Uns32 (Flags.Stack_Boundary - 1);
      if Pad /= 0 then
         Pad := Uns32 (Flags.Stack_Boundary) - Pad;
         Link_Stmt (New_Enode (OE_Stack_Adjust, Mode_Nil, O_Tnode_Null,
                               O_Enode (Pad), O_Enode_Null));
      end if;
      --  The stack has been adjusted by Pad bytes.
      Push_Offset := Push_Offset + Pad;

      --  Generate code for arguments (if any).
      Left := Get_Arg_Link (Stmt);
      if Left /= O_Enode_Null then
         Left := Gen_Insn (Left, R_None, Pnum);
      end if;

      --  Clobber registers.
      Clobber_R32 (R_Ax);
      Clobber_R32 (R_Dx);
      Clobber_R32 (R_Cx);
      --  FIXME: fp regs.

      --  Add the call.
      Reg_Res := Get_Call_Register (Get_Expr_Mode (Stmt));
      Set_Expr_Reg (Stmt, Reg_Res);
      Link_Stmt (Stmt);
      Res_Stmt := Stmt;

      if Push_Size + Pad /= 0 then
         Res_Stmt :=
           New_Enode (OE_Stack_Adjust, Get_Expr_Mode (Stmt), O_Tnode_Null,
                      O_Enode (-Int32 (Push_Size + Pad)), O_Enode_Null);
         Set_Expr_Reg (Res_Stmt, Reg_Res);
         Link_Stmt (Res_Stmt);
      end if;

      --  The stack has been restored (just after the call).
      Push_Offset := Push_Offset - (Push_Size + Pad);

      case Reg is
         when R_Any32
           | R_Any64
           | R_Any8
           | R_Irm
           | R_Rm
           | R_Ir
           | R_Sib
           | R_Ax
           | R_St0
           | R_Edx_Eax =>
            Reg_Res := Alloc_Reg (Reg_Res, Res_Stmt, Pnum);
            return Res_Stmt;
         when R_Any_Cc =>
            --  Move to register.
            --  (use the 'test' instruction).
            Alloc_Cc (Res_Stmt, Pnum);
            return Insert_Move (Res_Stmt, R_Ne);
         when R_None =>
            if Reg_Res /= R_None then
               raise Program_Error;
            end if;
            return Res_Stmt;
         when others =>
            Error_Gen_Insn (Stmt, Reg);
      end case;
   end Gen_Call;

   function Gen_Insn (Stmt : O_Enode; Reg : O_Reg; Pnum : O_Inum)
                     return O_Enode
   is
      Kind : constant OE_Kind := Get_Expr_Kind (Stmt);

      Left : O_Enode;
      Right : O_Enode;
      Res : O_Enode;

      Reg1 : O_Reg;
      --      P_Reg : O_Reg;
      Reg_L : O_Reg;
      Reg_Res : O_Reg;

      Num : O_Inum;
   begin
      case Kind is
         when OE_Addrl =>
            Right := Get_Addrl_Frame (Stmt);
            if Right /= O_Enode_Null then
               Num := Get_Insn_Num;
               Right := Gen_Insn (Right, R_Any32, Num);
               Set_Addrl_Frame (Stmt, Right);
            else
               Num := O_Free;
            end if;
            case Reg is
               when R_Sib =>
                  Set_Expr_Reg (Stmt, R_B_Off);
                  return Stmt;
               when R_Irm
                 | R_Ir =>
                  if Right /= O_Enode_Null then
                     Free_Insn_Regs (Right);
                  end if;
                  Set_Expr_Reg (Stmt, Alloc_Reg (R_Any32, Stmt, Pnum));
                  Link_Stmt (Stmt);
                  return Stmt;
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
         when OE_Addrg =>
            case Reg is
               when R_Sib
                 | R_Irm
                 | R_Ir =>
                  Set_Expr_Reg (Stmt, R_Imm);
                  return Stmt;
               when R_Any32
                 | Regs_R32 =>
                  Set_Expr_Reg (Stmt, Reg);
                  Link_Stmt (Stmt);
                  return Stmt;
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
         when OE_Indir =>
            Left := Get_Expr_Operand (Stmt);
            case Reg is
               when R_Irm
                 | R_Rm =>
                  Left := Gen_Insn (Left, R_Sib, Pnum);
                  Set_Expr_Reg (Stmt, R_Mem);
                  Set_Expr_Operand (Stmt, Left);
               when R_Ir
                 | R_Sib
                 | R_I_Off =>
                  Num := Get_Insn_Num;
                  Left := Gen_Insn (Left, R_Sib, Num);
                  Reg1 := Get_Reg_Any (Stmt);
                  if Reg1 = R_Any64 then
                     Reg1 := Alloc_Reg (Reg1, Stmt, Pnum);
                     Free_Insn_Regs (Left);
                  else
                     Free_Insn_Regs (Left);
                     Reg1 := Alloc_Reg (Reg1, Stmt, Pnum);
                  end if;
                  Set_Expr_Reg (Stmt, Reg1);
                  Set_Expr_Operand (Stmt, Left);
                  Link_Stmt (Stmt);
               when Regs_R32
                 | R_Any32
                 | R_Any8
                 | Regs_Fp =>
                  Num := Get_Insn_Num;
                  Left := Gen_Insn (Left, R_Sib, Num);
                  Free_Insn_Regs (Left);
                  Set_Expr_Reg (Stmt, Alloc_Reg (Reg, Stmt, Pnum));
                  Set_Expr_Operand (Stmt, Left);
                  Link_Stmt (Stmt);
               when Regs_R64
                 | R_Any64 =>
                  --  Avoid overwritting:
                  --  Eg: axdx = indir (ax)
                  --      axdx = indir (ax+dx)
                  Num := Get_Insn_Num;
                  Left := Gen_Insn (Left, R_Sib, Num);
                  Set_Expr_Reg (Stmt, Alloc_Reg (Reg, Stmt, Pnum));
                  Left := Reload (Left, R_Sib, Num);
                  Free_Insn_Regs (Left);
                  Set_Expr_Operand (Stmt, Left);
                  Link_Stmt (Stmt);
               when R_Any_Cc =>
                  Num := Get_Insn_Num;
                  Left := Gen_Insn (Left, R_Sib, Num);
                  --  Generate a cmp $1, XX
                  Set_Expr_Reg (Stmt, R_Eq);
                  Set_Expr_Operand (Stmt, Left);
                  Free_Insn_Regs (Left);
                  Link_Stmt (Stmt);
                  Alloc_Cc (Stmt, Pnum);
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
            return Stmt;
         when OE_Conv_Ptr =>
            --  Delete nops.
            return Gen_Insn (Get_Expr_Operand (Stmt), Reg, Pnum);
         when OE_Const =>
            case Get_Expr_Mode (Stmt) is
               when Mode_U8 .. Mode_U32
                 | Mode_I8 .. Mode_I32
                 | Mode_P32
                 | Mode_B2 =>
                  case Reg is
                     when R_Imm
                       | Regs_Imm32 =>
                        Set_Expr_Reg (Stmt, R_Imm);
                     when Regs_R32
                       | R_Any32
                       | R_Any8 =>
                        Set_Expr_Reg (Stmt, Alloc_Reg (Reg, Stmt, Pnum));
                        Link_Stmt (Stmt);
                     when R_Rm =>
                        Set_Expr_Reg
                          (Stmt, Alloc_Reg (Get_Reg_Any (Stmt), Stmt, Pnum));
                        Link_Stmt (Stmt);
                     when R_Any_Cc =>
                        Num := Get_Insn_Num;
                        Set_Expr_Reg (Stmt, Alloc_Reg (R_Any8, Stmt, Num));
                        Link_Stmt (Stmt);
                        Free_Insn_Regs (Stmt);
                        Right := Insert_Move (Stmt, R_Ne);
                        Alloc_Cc (Right, Pnum);
                        return Right;
                     when others =>
                        Error_Gen_Insn (Stmt, Reg);
                  end case;
               when Mode_F32
                 | Mode_F64 =>
                  case Reg is
                     when R_Ir
                       | R_Irm
                       | R_Rm
                       | R_St0 =>
                        Num := Get_Insn_Num;
                        if Reg = R_St0 or not Abi.Flag_Sse2 then
                           Reg1 := R_St0;
                        else
                           Reg1 := R_Any_Xmm;
                        end if;
                        Set_Expr_Reg (Stmt, Alloc_Reg (Reg1, Stmt, Num));
                        Link_Stmt (Stmt);
                     when others =>
                        raise Program_Error;
                  end case;
               when Mode_U64
                 | Mode_I64 =>
                  case Reg is
                     when R_Irm
                       | R_Ir
                       | R_Rm =>
                        Set_Expr_Reg (Stmt, R_Imm);
                     when R_Mem =>
                        Set_Expr_Reg (Stmt, R_Mem);
                     when Regs_R64
                       | R_Any64 =>
                        Set_Expr_Reg (Stmt, Alloc_Reg (Reg, Stmt, Pnum));
                        Link_Stmt (Stmt);
                     when others =>
                        raise Program_Error;
                  end case;
               when others =>
                  raise Program_Error;
            end case;
            return Stmt;
         when OE_Alloca =>
            --  Roughly speaking, emited code is: (MASK is a constant).
            --  VAL := (VAL + MASK) & ~MASK
            --  SP := SP - VAL
            --  res <- SP
            Left := Get_Expr_Operand (Stmt);
            case Reg is
               when R_Ir
                 | R_Irm
                 | R_Any32 =>
                  Num := Get_Insn_Num;
                  if X86.Flags.Flag_Alloca_Call then
                     Reg_L := R_Ax;
                  else
                     Reg_L := R_Any32;
                  end if;
                  Left := Gen_Insn (Left, Reg_L, Num);
                  Set_Expr_Operand (Stmt, Left);
                  Link_Stmt (Left);
                  Free_Insn_Regs (Left);
                  Set_Expr_Reg (Stmt, Alloc_Reg (Reg_L, Stmt, Pnum));
                  Link_Stmt (Stmt);
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
            return Stmt;

         when OE_Kind_Cmp =>
            --  Return LEFT cmp RIGHT, ie compute RIGHT - LEFT
            Num := Get_Insn_Num;
            Left := Get_Expr_Left (Stmt);
            Reg_L := Get_Reg_Any (Left);
            Left := Gen_Insn (Left, Reg_L, Num);

            Right := Get_Expr_Right (Stmt);
            case Get_Expr_Mode (Right) is
               when Mode_F32
                 | Mode_F64 =>
                  Reg1 := R_St0;
               when others =>
                  Reg1 := R_Irm;
            end case;
            Right := Gen_Insn (Right, Reg1, Num);

            --  FIXME: what about if right was spilled out of FP regs ?
            --  (it is reloaded in reverse).
            Left := Reload (Left, Reg_L, Num);

            Set_Expr_Right (Stmt, Right);
            Set_Expr_Left (Stmt, Left);

            Link_Stmt (Stmt);

            Reg_Res := Ekind_To_Cc (Stmt, Get_Expr_Mode (Left));
            case Get_Expr_Mode (Left) is
               when Mode_F32
                 | Mode_F64 =>
                  Reg_Res := Reverse_Cc (Reg_Res);
               when Mode_I64 =>
                  --  I64 is a little bit special...
                  Reg_Res := Get_R64_High (Get_Expr_Reg (Left));
                  if Reg_Res not in Regs_R8 then
                     Reg_Res := R_Nil;
                     for I in Regs_R8 loop
                        if Regs (I).Num = O_Free then
                           Reg_Res := I;
                           exit;
                        end if;
                     end loop;
                     if Reg_Res = R_Nil then
                        --  FIXME: to be handled.
                        --  Can this happen ?
                        raise Program_Error;
                     end if;
                  end if;

                  Free_Insn_Regs (Left);
                  Free_Insn_Regs (Right);

                  Set_Expr_Reg (Stmt, Reg_Res);
                  case Reg is
                     when R_Any_Cc =>
                        Right := Insert_Move (Stmt, R_Ne);
                        Alloc_Cc (Right, Pnum);
                        return Right;
                     when R_Any8
                       | Regs_R8
                       | R_Irm
                       | R_Ir
                       | R_Rm =>
                        Reg_Res := Alloc_Reg (Reg_Res, Stmt, Pnum);
                        return Stmt;
                     when others =>
                        Error_Gen_Insn (Stmt, Reg);
                  end case;
               when others =>
                  null;
            end case;
            Set_Expr_Reg (Stmt, Reg_Res);

            Free_Insn_Regs (Left);
            Free_Insn_Regs (Right);

            case Reg is
               when R_Any_Cc =>
                  Alloc_Cc (Stmt, Pnum);
                  return Stmt;
               when R_Any8
                 | Regs_R8 =>
                  Res := Insert_Move (Stmt, R_Any8);
                  Reg_Res := Alloc_Reg (Reg, Res, Pnum);
                  Set_Expr_Reg (Res, Reg_Res);
                  return Res;
               when R_Irm
                 | R_Ir
                 | R_Rm =>
                  Res := Insert_Move (Stmt, R_Any32);
                  Reg_Res := Alloc_Reg (R_Any8, Res, Pnum);
                  Set_Expr_Reg (Res, Reg_Res);
                  return Res;
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
         when OE_Add =>
            declare
               R_L : O_Reg;
               R_R : O_Reg;
            begin
               Left := Gen_Insn (Get_Expr_Left (Stmt), R_Sib, Pnum);
               Right := Gen_Insn (Get_Expr_Right (Stmt), R_Sib, Pnum);
               Left := Reload (Left, R_Sib, Pnum);
               Set_Expr_Right (Stmt, Right);
               Set_Expr_Left (Stmt, Left);
               R_L := Get_Expr_Reg (Left);
               R_R := Get_Expr_Reg (Right);
               --  Results can be: Reg, R_B_Off, R_Sib, R_Imm, R_B_I
               case R_L is
                  when R_Any32
                    | Regs_R32 =>
                     case R_R is
                        when R_Imm =>
                           Set_Expr_Reg (Stmt, R_B_Off);
                        when R_B_Off
                          | R_I
                          | R_I_Off =>
                           Set_Expr_Reg (Stmt, R_Sib);
                        when R_Any32
                          | Regs_R32 =>
                           Set_Expr_Reg (Stmt, R_B_I);
                        when others =>
                           Error_Gen_Insn (Stmt, R_R);
                     end case;
                  when R_Imm =>
                     case R_R is
                        when R_Imm =>
                           Set_Expr_Reg (Stmt, R_Imm);
                        when R_Any32
                          | Regs_R32
                          | R_B_Off =>
                           Set_Expr_Reg (Stmt, R_B_Off);
                        when R_I
                          | R_I_Off =>
                           Set_Expr_Reg (Stmt, R_I_Off);
                        when others =>
                           Error_Gen_Insn (Stmt, R_R);
                     end case;
                  when R_B_Off =>
                     case R_R is
                        when R_Imm =>
                           Set_Expr_Reg (Stmt, R_B_Off);
                        when R_Any32
                          | Regs_R32
                          | R_I =>
                           Set_Expr_Reg (Stmt, R_Sib);
                        when others =>
                           Error_Gen_Insn (Stmt, R_R);
                     end case;
                  when R_I_Off =>
                     case R_R is
                        when R_Imm =>
                           Set_Expr_Reg (Stmt, R_I_Off);
                        when R_Any32
                          | Regs_R32 =>
                           Set_Expr_Reg (Stmt, R_Sib);
                        when others =>
                           Error_Gen_Insn (Stmt, R_R);
                     end case;
                  when R_I =>
                     case R_R is
                        when R_Imm
                          | Regs_R32
                          | R_B_Off =>
                           Set_Expr_Reg (Stmt, R_Sib);
                        when others =>
                           Error_Gen_Insn (Stmt, R_R);
                     end case;
                  when R_Sib
                    | R_B_I =>
                     if R_R = R_Imm then
                        Set_Expr_Reg (Stmt, R_Sib);
                     else
                        Num := Get_Insn_Num;
                        Free_Insn_Regs (Left);
                        Set_Expr_Reg (Left, Alloc_Reg (R_Any32, Left, Num));
                        Link_Stmt (Left);
                        case R_R is
                           when R_Any32
                             | Regs_R32
                             | R_I =>
                              Set_Expr_Reg (Stmt, R_B_I);
                           when others =>
                              Error_Gen_Insn (Stmt, R_R);
                        end case;
                     end if;
                  when others =>
                     Error_Gen_Insn (Stmt, R_L);
               end case;

               case Reg is
                  when R_Sib =>
                     null;
                  when R_Ir
                    | R_Irm =>
                     if Get_Expr_Reg (Stmt) /= R_Imm then
                        Set_Expr_Reg (Stmt, Alloc_Reg (R_Any32, Stmt, Pnum));
                        Free_Insn_Regs (Left);
                        Free_Insn_Regs (Right);
                        Link_Stmt (Stmt);
                     end if;
                  when R_Any32
                    | Regs_R32 =>
                     Set_Expr_Reg (Stmt, Alloc_Reg (Reg, Stmt, Pnum));
                     Link_Stmt (Stmt);
                  when others =>
                     Error_Gen_Insn (Stmt, Reg);
               end case;
            end;
            return Stmt;
         when OE_Mul =>
            Num := Get_Insn_Num;
            Left := Gen_Insn (Get_Expr_Left (Stmt), R_Ax, Num);
            Set_Expr_Left (Stmt, Left);

            Right := Gen_Insn (Get_Expr_Right (Stmt), R_Any32, Num);
            if Get_Expr_Kind (Right) /= OE_Const then
               raise Program_Error;
            end if;
            Set_Expr_Right (Stmt, Right);

            Free_Insn_Regs (Left);
            Free_Insn_Regs (Right);
            Clobber_R32 (R_Dx);
            Set_Expr_Reg (Stmt, Alloc_Reg (R_Ax, Stmt, Pnum));
            case Reg is
               when R_Sib
                 | R_B_Off =>
                  null;
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
            Link_Stmt (Stmt);
            return Stmt;
         when OE_Shl =>
            Num := Get_Insn_Num;
            Right := Get_Expr_Right (Stmt);
            if Get_Expr_Kind (Right) /= OE_Const then
               Right := Gen_Insn (Right, R_Cx, Num);
            else
               Right := Gen_Insn (Right, R_Imm, Num);
            end if;
            Left := Get_Expr_Left (Stmt);
            Reg1 := Get_Reg_Any (Stmt);
            Left := Gen_Insn (Left, Reg1, Pnum);
            if Get_Expr_Kind (Right) /= OE_Const then
               Right := Reload (Right, R_Cx, Num);
            end if;
            Left := Reload (Left, Reg1, Pnum);
            Set_Expr_Left (Stmt, Left);
            Set_Expr_Right (Stmt, Right);
            if Reg = R_Sib
              and then Get_Expr_Kind (Right) = OE_Const
              and then Get_Expr_Low (Right) in 0 .. 3
            then
               --  Becomes the index of the SIB.
               Set_Expr_Reg (Stmt, R_I);
            else
               Reg_Res := Get_Expr_Reg (Left);
               Set_Expr_Reg (Stmt, Reg_Res);
               Renum_Reg (Reg_Res, Stmt, Pnum);
               Link_Stmt (Stmt);
               Free_Insn_Regs (Right);
            end if;
            return Stmt;

         when OE_Add_Ov
           | OE_Sub_Ov
           | OE_And
           | OE_Xor
           | OE_Or =>
            --  Accepted is: R with IMM or R/M
            Num := Get_Insn_Num;
            Right := Get_Expr_Right (Stmt);
            Left := Get_Expr_Left (Stmt);
            case Reg is
               when R_Irm
                 | R_Rm
                 | R_Ir
                 | R_Sib =>
                  Right := Gen_Insn (Right, R_Irm, Num);
                  Reg1 := Get_Reg_Any (Stmt);
                  Left := Gen_Insn (Left, Reg1, Num);
                  Right := Reload (Right, R_Irm, Num);
                  Left := Reload (Left, Reg1, Num);
                  Reg_Res := Get_Expr_Reg (Left);
               when R_Any_Cc =>
                  Right := Gen_Insn (Right, R_Irm, Num);
                  Left := Gen_Insn (Left, R_Any8, Num);
                  Reg_Res := R_Ne;
                  Alloc_Cc (Stmt, Num);
                  Free_Insn_Regs (Left);
               when R_Any32
                 | Regs_R32
                 | R_Any8
                 | R_Any64
                 | Regs_R64
                 | Regs_Fp =>
                  Right := Gen_Insn (Right, R_Irm, Num);
                  Left := Gen_Insn (Left, Reg, Num);
                  Right := Reload (Right, R_Irm, Num);
                  Left := Reload (Left, Reg, Num);
                  Reg_Res := Get_Expr_Reg (Left);
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
            Set_Expr_Right (Stmt, Right);
            Set_Expr_Left (Stmt, Left);
            Set_Expr_Reg (Stmt, Reg_Res);
            Renum_Reg (Reg_Res, Stmt, Pnum);
            Link_Stmt (Stmt);
            Free_Insn_Regs (Right);
            return Stmt;

         when OE_Mod
           | OE_Rem
           | OE_Mul_Ov
           | OE_Div_Ov =>
            declare
               Mode : Mode_Type;
            begin
               Num := Get_Insn_Num;
               Mode := Get_Expr_Mode (Stmt);
               Left := Get_Expr_Left (Stmt);
               Right := Get_Expr_Right (Stmt);
               case Mode is
                  when Mode_I32
                    | Mode_U32
                    | Mode_I16
                    | Mode_U16 =>
                     Left := Gen_Insn (Left, R_Ax, Num);
                     Right := Gen_Insn (Right, R_Rm, Num);
                     Left := Reload (Left, R_Ax, Num);
                     case Kind is
                        when OE_Div_Ov
                          | OE_Rem
                          | OE_Mod =>
                           --  Be sure EDX is free.
                           Reg_Res := Alloc_Reg (R_Dx, Stmt, Pnum);
                        when others =>
                           Reg_Res := R_Nil;
                     end case;
                     Right := Reload (Right, R_Rm, Num);
                     Set_Expr_Right (Stmt, Right);
                     Set_Expr_Left (Stmt, Left);
                     Free_Insn_Regs (Left);
                     Free_Insn_Regs (Right);
                     if Reg_Res /= R_Nil then
                        Free_R32 (Reg_Res);
                     end if;
                     if Kind = OE_Div_Ov or Kind = OE_Mul_Ov then
                        Reg_Res := R_Ax;
                        Clobber_R32 (R_Dx);
                     else
                        Reg_Res := R_Dx;
                        Clobber_R32 (R_Ax);
                     end if;
                     Set_Expr_Reg (Stmt, Alloc_Reg (Reg_Res, Stmt, Pnum));
                     Link_Stmt (Stmt);
                     return Reload (Stmt, Reg, Pnum);
                  when Mode_U64
                    | Mode_I64 =>
                     --  FIXME: align stack
                     Insert_Arg (Gen_Insn (Right, R_Irm, Num));
                     Insert_Arg (Gen_Insn (Left, R_Irm, Num));
                     return Insert_Intrinsic (Stmt, R_Edx_Eax, Pnum);
                  when Mode_F32
                    | Mode_F64 =>
                     Left := Gen_Insn (Left, R_St0, Num);
                     Right := Gen_Insn (Right, R_Rm, Num);
                     Set_Expr_Left (Stmt, Left);
                     Set_Expr_Right (Stmt, Right);
                     Free_Insn_Regs (Right);
                     Free_Insn_Regs (Left);
                     Set_Expr_Reg (Stmt, Alloc_Reg (R_St0, Stmt, Pnum));
                     Link_Stmt (Stmt);
                     return Stmt;
                  when others =>
                     Error_Gen_Insn (Stmt, Mode);
               end case;
            end;

         when OE_Not
           | OE_Abs_Ov
           | OE_Neg_Ov =>
            Left := Get_Expr_Operand (Stmt);
            case Reg is
               when R_Any32
                 | Regs_R32
                 | R_Any64
                 | Regs_R64
                 | R_Any8
                 | R_St0 =>
                  Reg_Res := Reg;
               when R_Any_Cc =>
                  if Kind /= OE_Not then
                     raise Program_Error;
                  end if;
                  Left := Gen_Insn (Left, R_Any_Cc, Pnum);
                  Set_Expr_Operand (Stmt, Left);
                  Reg_Res := Inverse_Cc (Get_Expr_Reg (Left));
                  Free_Cc;
                  Set_Expr_Reg (Stmt, Reg_Res);
                  Alloc_Cc (Stmt, Pnum);
                  return Stmt;
               when R_Irm
                 | R_Rm
                 | R_Ir =>
                  Reg_Res := Get_Reg_Any (Get_Expr_Mode (Left));
               when others =>
                  Error_Gen_Insn (Stmt, Reg);
            end case;
            Left := Gen_Insn (Left, Reg_Res, Pnum);
            Set_Expr_Operand (Stmt, Left);
            Reg_Res := Get_Expr_Reg (Left);
            Free_Insn_Regs (Left);
            Set_Expr_Reg (Stmt, Alloc_Reg (Reg_Res, Stmt, Pnum));
            Link_Stmt (Stmt);
            return Stmt;
         when OE_Conv =>
            Left := Get_Expr_Operand (Stmt);
            declare
               --  Operand mode
               O_Mode : constant Mode_Type := Get_Expr_Mode (Left);

               --  Result mode
               R_Mode : constant Mode_Type := Get_Expr_Mode (Stmt);

               Reg_Op : O_Reg;
            begin
               --  Simple case: no conversion.
               --  FIXME: should be handled by EXPR and convert to NOP.
               if Get_Expr_Mode (Left) = Get_Expr_Mode (Stmt) then
                  --  A no-op.
                  return Gen_Insn (Left, Reg, Pnum);
               end if;

               --  By default, can work on reg or memory.
               Reg_Op := R_Rm;

               case R_Mode is
                  when Mode_B2 =>
                     --  To B2
                     case O_Mode is
                        when Mode_U32
                          | Mode_I32 =>
                           --  Detect for bound.
                           null;
                        when Mode_I64 =>
                           --  Work on registers.
                           Reg_Op := R_Any64;
                        when others =>
                           Error_Gen_Insn (Stmt, O_Mode);
                     end case;
                  when Mode_U8 =>
                     --  To U8
                     case O_Mode is
                        when Mode_U16
                          | Mode_U32
                          | Mode_I32 =>
                           --  Detect for bound.
                           null;
                        when Mode_I64 =>
                           --  Work on registers.
                           Reg_Op := R_Any64;
                        when others =>
                           Error_Gen_Insn (Stmt, O_Mode);
                     end case;
                  when Mode_U32 =>
                     --  To U32
                     case O_Mode is
                        when Mode_I32 =>
                           --  Detect for bound.
                           null;
                        when Mode_B2
                          | Mode_U8
                          | Mode_U16 =>
                           --  Zero extend.
                           null;
                        when others =>
                           Error_Gen_Insn (Stmt, O_Mode);
                     end case;
                  when Mode_I32 =>
                     --  To I32
                     case O_Mode is
                        when Mode_U8
                          | Mode_I8
                          | Mode_B2
                          | Mode_U16
                          | Mode_U32 =>
                           --  Zero extend
                           --  Detect for bound (U32).
                           null;
                        when Mode_I64 =>
                           --  Detect for bound (U32)
                           Num := Get_Insn_Num;
                           Left := Gen_Insn (Left, R_Edx_Eax, Num);
                           Free_Insn_Regs (Left);
                           Set_Expr_Operand (Stmt, Left);
                           case Reg is
                              when R_Ax
                                | R_Any32
                                | R_Rm
                                | R_Irm
                                | R_Ir =>
                                 Set_Expr_Reg
                                   (Stmt, Alloc_Reg (R_Ax, Stmt, Num));
                              when others =>
                                 raise Program_Error;
                           end case;
                           Insert_Reg (Mode_U32);
                           Link_Stmt (Stmt);
                           return Stmt;
                        when Mode_F64
                          | Mode_F32 =>
                           return Gen_Conv_From_Fp_Insn (Stmt, Reg, Pnum);
                        when others =>
                           Error_Gen_Insn (Stmt, O_Mode);
                     end case;
                  when Mode_I64 =>
                     --  To I64
                     case O_Mode is
                        when Mode_I32
                          | Mode_U32
                          | Mode_U8
                          | Mode_B2 =>
                           --  Zero or Sign extend.
                           Num := Get_Insn_Num;
                           Left := Gen_Insn (Left, R_Ax, Num);
                           Set_Expr_Operand (Stmt, Left);
                           Free_Insn_Regs (Left);
                           case Reg is
                              when R_Edx_Eax
                                | R_Any64
                                | R_Rm
                                | R_Irm
                                | R_Ir =>
                                 Set_Expr_Reg
                                   (Stmt, Alloc_Reg (R_Edx_Eax, Stmt, Pnum));
                              when others =>
                                 raise Program_Error;
                           end case;
                           Link_Stmt (Stmt);
                           return Stmt;
                        when Mode_F64
                          | Mode_F32 =>
                           return Gen_Conv_From_Fp_Insn (Stmt, Reg, Pnum);
                        when others =>
                           Error_Gen_Insn (Stmt, O_Mode);
                     end case;
                  when Mode_F64 =>
                     --  To F64
                     case O_Mode is
                        when Mode_I32
                          | Mode_I64 =>
                           null;
                        when others =>
                           Error_Gen_Insn (Stmt, O_Mode);
                     end case;
                  when others =>
                     Error_Gen_Insn (Stmt, O_Mode);
               end case;
               Left := Gen_Insn (Left, Reg_Op, Pnum);
               Set_Expr_Operand (Stmt, Left);
               case Reg is
                  when R_Irm
                    | R_Rm
                    | R_Ir
                    | R_Sib
                    | R_Any32
                    | Regs_R32
                    | R_Any64
                    | R_Any8
                    | Regs_R64
                    | Regs_Fp =>
                     Free_Insn_Regs (Left);
                     Set_Expr_Reg
                       (Stmt, Alloc_Reg (Get_Reg_Any (Stmt), Stmt, Pnum));
                  when others =>
                     Error_Gen_Insn (Stmt, Reg);
               end case;
               Link_Stmt (Stmt);
               return Stmt;
            end;
         when OE_Arg =>
            if Reg /= R_None then
               raise Program_Error;
            end if;
            Left := Get_Arg_Link (Stmt);
            if Left /= O_Enode_Null then
               --  Recurse on next argument, so the first argument is pushed
               --  the last one.
               Left := Gen_Insn (Left, R_None, Pnum);
            end if;

            Left := Get_Expr_Operand (Stmt);
            case Get_Expr_Mode (Left) is
               when Mode_F32 .. Mode_F64 =>
                  --  fstp instruction.
                  Reg_Res := R_St0;
               when others =>
                  --  Push instruction.
                  Reg_Res := R_Irm;
            end case;
            Left := Gen_Insn (Left, Reg_Res, Pnum);
            Set_Expr_Operand (Stmt, Left);
            Push_Offset := Push_Offset +
              Do_Align (Get_Mode_Size (Get_Expr_Mode (Left)), Mode_U32);
            Link_Stmt (Stmt);
            Free_Insn_Regs (Left);
            return Stmt;
         when OE_Call =>
            return Gen_Call (Stmt, Reg, Pnum);
         when OE_Case_Expr =>
            Left := Get_Expr_Operand (Stmt);
            Set_Expr_Reg (Stmt, Alloc_Reg (Get_Expr_Reg (Left), Stmt, Pnum));
            return Stmt;
         when OE_Get_Stack =>
            Set_Expr_Reg (Stmt, R_Sp);
            return Stmt;
         when OE_Get_Frame =>
            Set_Expr_Reg (Stmt, R_Bp);
            return Stmt;
         when others =>
            Ada.Text_IO.Put_Line
              ("gen_insn: unhandled enode " & OE_Kind'Image (Kind));
            raise Program_Error;
      end case;
   end Gen_Insn;

   procedure Assert_Free_Regs (Stmt : O_Enode) is
   begin
      for I in Regs_R32 loop
         if Regs (I).Num /= O_Free then
            Error_Reg ("gen_insn_stmt: reg is not free", Stmt, I);
         end if;
      end loop;
      for I in Fp_Stack_Type loop
         if Fp_Regs (I).Stmt /= O_Enode_Null then
            Error_Reg ("gen_insn_stmt: reg is not free", Stmt, R_St0);
         end if;
      end loop;
   end Assert_Free_Regs;

   procedure Gen_Insn_Stmt (Stmt : O_Enode)
   is
      Kind : OE_Kind;

      Left : O_Enode;
      Right : O_Enode;
      P_Reg : O_Reg;
      Num : O_Inum;

      Prev_Stack_Offset : Uns32;
   begin
      Insn_Num := O_Iroot;
      Num := Get_Insn_Num;
      Prev_Stack_Offset := Stack_Offset;

      Kind := Get_Expr_Kind (Stmt);
      case Kind is
         when OE_Asgn =>
            Left := Gen_Insn (Get_Expr_Operand (Stmt), R_Ir, Num);
            Right := Gen_Insn (Get_Assign_Target (Stmt), R_Sib, Num);
            Left := Reload (Left, R_Ir, Num);
            --Right := Reload (Right, R_Sib, Num);
            Set_Expr_Operand (Stmt, Left);
            Set_Assign_Target (Stmt, Right);
            Link_Stmt (Stmt);
            Free_Insn_Regs (Left);
            Free_Insn_Regs (Right);
         when OE_Set_Stack =>
            Left := Gen_Insn (Get_Expr_Operand (Stmt), R_Rm, Num);
            Set_Expr_Operand (Stmt, Left);
            Set_Expr_Reg (Stmt, R_Sp);
            Link_Stmt (Stmt);
         when OE_Jump_F
           | OE_Jump_T =>
            Left := Gen_Insn (Get_Expr_Operand (Stmt), R_Any_Cc, Num);
            Set_Expr_Operand (Stmt, Left);
            Link_Stmt (Stmt);
            Free_Cc;
         when OE_Beg =>
            declare
               Block_Decl : O_Dnode;
            begin
               Cur_Block := Stmt;
               Block_Decl := Get_Block_Decls (Cur_Block);
               Set_Block_Max_Stack (Block_Decl, Stack_Offset);
               Expand_Decls (Block_Decl);
            end;
            Link_Stmt (Stmt);
         when OE_End =>
            Swap_Stack_Offset (Get_Block_Decls (Cur_Block));
            Cur_Block := Get_Block_Parent (Cur_Block);
            Link_Stmt (Stmt);
         when OE_Jump
           | OE_Label =>
            Link_Stmt (Stmt);
         when OE_Leave =>
            Link_Stmt (Stmt);
         when OE_Call =>
            Link_Stmt (Gen_Call (Stmt, R_None, Num));
         when OE_Ret =>
            Left := Get_Expr_Operand (Stmt);
            P_Reg := Get_Call_Register (Get_Expr_Mode (Stmt));
            Left := Gen_Insn (Left, P_Reg, Num);
            Set_Expr_Operand (Stmt, Left);
            Link_Stmt (Stmt);
            Free_Insn_Regs (Left);
         when OE_Case =>
            Left := Gen_Insn (Get_Expr_Operand (Stmt),
                              Get_Reg_Any (Get_Expr_Mode (Stmt)),
                              Num);
            Set_Expr_Operand (Stmt, Left);
            Set_Expr_Reg (Stmt, Get_Expr_Reg (Left));
            Link_Stmt (Stmt);
            Free_Insn_Regs (Left);
         when OE_Line =>
            Set_Expr_Reg (Stmt, R_None);
            Link_Stmt (Stmt);
         when OE_BB =>
            --  Keep BB.
            Link_Stmt (Stmt);
         when others =>
            Ada.Text_IO.Put_Line
              ("gen_insn_stmt: unhandled enode " & OE_Kind'Image (Kind));
            raise Program_Error;
      end case;

      --  Free any spill stack slots.
      case Kind is
         when OE_Beg
           | OE_End =>
            null;
         when others =>
            Stack_Offset := Prev_Stack_Offset;
      end case;

      --  Check all registers are free.
      if Debug.Flag_Debug_Assert then
         Assert_Free_Regs (Stmt);
      end if;
   end Gen_Insn_Stmt;

   procedure Gen_Subprg_Insns (Subprg : Subprogram_Data_Acc)
   is
      First : O_Enode;
      Stmt : O_Enode;
      N_Stmt : O_Enode;
   begin
      if Debug.Flag_Debug_Insn then
         declare
            Inter : O_Dnode;
         begin
            Disp_Decl (1, Subprg.D_Decl);
            Inter := Get_Subprg_Interfaces (Subprg.D_Decl);
            while Inter /= O_Dnode_Null loop
               Disp_Decl (2, Inter);
               Inter := Get_Interface_Chain (Inter);
            end loop;
         end;
      end if;

      for I in Regs_R32 loop
         Regs (I).Used := False;
      end loop;

      Stack_Max := 0;
      Stack_Offset := 0;
      First := Subprg.E_Entry;
      Expand_Decls (Subprg.D_Body + 1);
      Abi.Last_Link := First;

      --  Generate instructions.
      --  Skip OE_Entry.
      Stmt := Get_Stmt_Link (First);
      loop
         N_Stmt := Get_Stmt_Link (Stmt);
         Gen_Insn_Stmt (Stmt);
         exit when Get_Expr_Kind (Stmt) = OE_Leave;
         Stmt := N_Stmt;
      end loop;

      --  Keep stack depth for this subprogram.
      Subprg.Stack_Max := Stack_Max;

      --  Sanity check: there must be no remaining pushed bytes.
      if Push_Offset /= 0 then
         raise Program_Error with "gen_subprg_insn: push_offset not 0";
      end if;
   end Gen_Subprg_Insns;

end Ortho_Code.X86.Insns;
