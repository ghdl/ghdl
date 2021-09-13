--  Mcode back-end for ortho - Expressions and control handling.
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
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Tables;
with Ortho_Code.Types; use Ortho_Code.Types;
with Ortho_Code.Consts; use Ortho_Code.Consts;
with Ortho_Code.Decls; use Ortho_Code.Decls;
with Ortho_Code.Debug; use Ortho_Code.Debug;
with Ortho_Code.Abi; use Ortho_Code.Abi;
with Ortho_Code.Disps;
with Ortho_Code.Opts;
with Ortho_Code.Flags;

package body Ortho_Code.Exprs is

   type Enode_Pad is mod 256;

   type Enode_Common is record
      Kind : OE_Kind; --  about 1 byte (6 bits)
      Reg : O_Reg; --  1 byte
      Mode : Mode_Type; -- 4 bits
      Ref : Boolean;
      Flag1 : Boolean;
      Flag2 : Boolean;
      Flag3 : Boolean;
      Pad : Enode_Pad;
      Arg1 : O_Enode;
      Arg2 : O_Enode;
      Info : Int32;
   end record;
   pragma Pack (Enode_Common);
   for Enode_Common'Size use 4*32;
   for Enode_Common'Alignment use 4;

   package Enodes is new Tables
     (Table_Component_Type => Enode_Common,
      Table_Index_Type => O_Enode,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   function Get_Expr_Kind (Enode : O_Enode) return OE_Kind is
   begin
      return Enodes.Table (Enode).Kind;
   end Get_Expr_Kind;

   function Get_Expr_Mode (Enode : O_Enode) return Mode_Type is
   begin
      return Enodes.Table (Enode).Mode;
   end Get_Expr_Mode;

   function Get_Enode_Type (Enode : O_Enode) return O_Tnode is
   begin
      return O_Tnode (Enodes.Table (Enode).Info);
   end Get_Enode_Type;

   function Get_Expr_Reg (Enode : O_Enode) return O_Reg is
   begin
      return Enodes.Table (Enode).Reg;
   end Get_Expr_Reg;

   procedure Set_Expr_Reg (Enode : O_Enode; Reg : O_Reg) is
   begin
      Enodes.Table (Enode).Reg := Reg;
   end Set_Expr_Reg;

   function Get_Expr_Operand (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg1;
   end Get_Expr_Operand;

   procedure Set_Expr_Operand (Enode : O_Enode; Val : O_Enode) is
   begin
      Enodes.Table (Enode).Arg1 := Val;
   end Set_Expr_Operand;

   function Get_Expr_Left (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg1;
   end Get_Expr_Left;

   function Get_Expr_Right (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg2;
   end Get_Expr_Right;

   procedure Set_Expr_Left (Enode : O_Enode; Val : O_Enode) is
   begin
      Enodes.Table (Enode).Arg1 := Val;
   end Set_Expr_Left;

   procedure Set_Expr_Right (Enode : O_Enode; Val : O_Enode) is
   begin
      Enodes.Table (Enode).Arg2 := Val;
   end Set_Expr_Right;

   function Get_Expr_Low (Cst : O_Enode) return Uns32 is
   begin
      return To_Uns32 (Int32 (Enodes.Table (Cst).Arg1));
   end Get_Expr_Low;

   function Get_Expr_High (Cst : O_Enode) return Uns32 is
   begin
      return To_Uns32 (Int32 (Enodes.Table (Cst).Arg2));
   end Get_Expr_High;

   function Get_Assign_Target (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg2;
   end Get_Assign_Target;

   procedure Set_Assign_Target (Enode : O_Enode; Targ : O_Enode) is
   begin
      Enodes.Table (Enode).Arg2 := Targ;
   end Set_Assign_Target;

   function Get_Expr_Lit (Lit : O_Enode) return O_Cnode is
   begin
      return O_Cnode (Enodes.Table (Lit).Arg1);
   end Get_Expr_Lit;

   function Get_Conv_Type (Enode : O_Enode) return O_Tnode is
   begin
      return O_Tnode (Enodes.Table (Enode).Arg2);
   end Get_Conv_Type;

   --  Leave node corresponding to the entry.
   function Get_Entry_Leave (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg1;
   end Get_Entry_Leave;

   procedure Set_Entry_Leave (Enode : O_Enode; Leave : O_Enode) is
   begin
      Enodes.Table (Enode).Arg1 := Leave;
   end Set_Entry_Leave;

   function Get_Jump_Label (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg2;
   end Get_Jump_Label;

   procedure Set_Jump_Label (Enode : O_Enode; Label : O_Enode) is
   begin
      Enodes.Table (Enode).Arg2 := Label;
   end Set_Jump_Label;

   function Get_Addr_Object (Enode : O_Enode) return O_Lnode is
   begin
      return O_Lnode (Enodes.Table (Enode).Arg1);
   end Get_Addr_Object;

   function Get_Addr_Decl (Enode : O_Enode) return O_Dnode is
   begin
      return O_Dnode (Enodes.Table (Enode).Arg1);
   end Get_Addr_Decl;

   function Get_Addrl_Frame (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg2;
   end Get_Addrl_Frame;

   procedure Set_Addrl_Frame (Enode : O_Enode; Frame : O_Enode) is
   begin
      Enodes.Table (Enode).Arg2 := Frame;
   end Set_Addrl_Frame;

   function Get_Call_Subprg (Enode : O_Enode) return O_Dnode is
   begin
      return O_Dnode (Enodes.Table (Enode).Arg1);
   end Get_Call_Subprg;

   function Get_Stack_Adjust (Enode : O_Enode) return Int32 is
   begin
      return Int32 (Enodes.Table (Enode).Arg1);
   end Get_Stack_Adjust;

   procedure Set_Stack_Adjust (Enode : O_Enode; Off : Int32) is
   begin
      Enodes.Table (Enode).Arg1 := O_Enode (Off);
   end Set_Stack_Adjust;

   function Get_Arg_Link (Enode : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Enode).Arg2;
   end Get_Arg_Link;

   function Get_Block_Decls (Blk : O_Enode) return O_Dnode is
   begin
      return O_Dnode (Enodes.Table (Blk).Arg2);
   end Get_Block_Decls;

   function Get_Block_Parent (Blk : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Blk).Arg1;
   end Get_Block_Parent;

   function Get_Block_Has_Alloca (Blk : O_Enode) return Boolean is
   begin
      return Enodes.Table (Blk).Flag1;
   end Get_Block_Has_Alloca;

   procedure Set_Block_Has_Alloca (Blk : O_Enode; Flag : Boolean) is
   begin
      Enodes.Table (Blk).Flag1 := Flag;
   end Set_Block_Has_Alloca;

   function Get_End_Beg (Blk : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Blk).Arg1;
   end Get_End_Beg;

   function Get_Label_Info (Label : O_Enode) return Int32 is
   begin
      return Int32 (Enodes.Table (Label).Arg2);
   end Get_Label_Info;

   procedure Set_Label_Info (Label : O_Enode; Info : Int32) is
   begin
      Enodes.Table (Label).Arg2 := O_Enode (Info);
   end Set_Label_Info;

   function Get_Label_Block (Label : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Label).Arg1;
   end Get_Label_Block;

   function Get_Spill_Info (Spill : O_Enode) return Int32 is
   begin
      return Int32 (Enodes.Table (Spill).Arg2);
   end Get_Spill_Info;

   procedure Set_Spill_Info (Spill : O_Enode; Info : Int32) is
   begin
      Enodes.Table (Spill).Arg2 := O_Enode (Info);
   end Set_Spill_Info;

   --  Get the statement link.
   function Get_Stmt_Link (Stmt : O_Enode) return O_Enode is
   begin
      return O_Enode (Enodes.Table (Stmt).Info);
   end Get_Stmt_Link;

   procedure Set_Stmt_Link (Stmt : O_Enode; Next : O_Enode) is
   begin
      Enodes.Table (Stmt).Info := Int32 (Next);
   end Set_Stmt_Link;

   function Get_BB_Next (Stmt : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Stmt).Arg1;
   end Get_BB_Next;
   pragma Unreferenced (Get_BB_Next);

   procedure Set_BB_Next (Stmt : O_Enode; Next : O_Enode) is
   begin
      Enodes.Table (Stmt).Arg1 := Next;
   end Set_BB_Next;

   function Get_BB_Number (Stmt : O_Enode) return Int32 is
   begin
      return Int32 (Enodes.Table (Stmt).Arg2);
   end Get_BB_Number;

   function Get_Loop_Level (Stmt : O_Enode) return Int32 is
   begin
      return Int32 (Enodes.Table (Stmt).Arg1);
   end Get_Loop_Level;

   procedure Set_Loop_Level (Stmt : O_Enode; Level : Int32) is
   begin
      Enodes.Table (Stmt).Arg1 := O_Enode (Level);
   end Set_Loop_Level;

   procedure Set_Case_Branch (C : O_Enode; Branch : O_Enode) is
   begin
      Enodes.Table (C).Arg2 := Branch;
   end Set_Case_Branch;

   procedure Set_Case_Branch_Choice (Branch : O_Enode; Choice : O_Enode) is
   begin
      Enodes.Table (Branch).Arg1 := Choice;
   end Set_Case_Branch_Choice;

   function Get_Case_Branch_Choice (Branch : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Branch).Arg1;
   end Get_Case_Branch_Choice;

   procedure Set_Case_Choice_Link (Choice : O_Enode; N_Choice : O_Enode) is
   begin
      Enodes.Table (Choice).Info := Int32 (N_Choice);
   end Set_Case_Choice_Link;

   function Get_Case_Choice_Link (Choice : O_Enode) return O_Enode is
   begin
      return O_Enode (Enodes.Table (Choice).Info);
   end Get_Case_Choice_Link;

   function Get_Ref_Field (Ref : O_Enode) return O_Fnode is
   begin
      return O_Fnode (Enodes.Table (Ref).Arg2);
   end Get_Ref_Field;

   function Get_Ref_Index (Ref : O_Enode) return O_Enode is
   begin
      return Enodes.Table (Ref).Arg2;
   end Get_Ref_Index;

   function Get_Expr_Line_Number (Stmt : O_Enode) return Int32 is
   begin
      return Int32 (Enodes.Table (Stmt).Arg1);
   end Get_Expr_Line_Number;

   function Get_Intrinsic_Operation (Stmt : O_Enode) return Int32 is
   begin
      return Int32 (Enodes.Table (Stmt).Arg1);
   end Get_Intrinsic_Operation;

   Last_Stmt : O_Enode := O_Enode_Null;

   procedure Link_Stmt (Stmt : O_Enode) is
   begin
      --  Expect a real statement.
      pragma Assert (Stmt /= O_Enode_Null);

      --  Must be withint a subprogram.
      pragma Assert (Last_Stmt /= O_Enode_Null);

      Set_Stmt_Link (Last_Stmt, Stmt);
      Last_Stmt := Stmt;
   end Link_Stmt;

   function New_Enode (Kind : OE_Kind;
                       Rtype : O_Tnode;
                       Arg1 : O_Enode;
                       Arg2 : O_Enode) return O_Enode
   is
      Mode : Mode_Type;
   begin
      Mode := Get_Type_Mode (Rtype);
      Enodes.Append (Enode_Common'(Kind => Kind,
                                   Reg => 0,
                                   Mode => Mode,
                                   Ref => False,
                                   Flag1 => False,
                                   Flag2 => False,
                                   Flag3 => False,
                                   Pad => 0,
                                   Arg1 => Arg1,
                                   Arg2 => Arg2,
                                   Info => Int32 (Rtype)));
      return Enodes.Last;
   end New_Enode;

   function New_Enode (Kind : OE_Kind;
                       Mode : Mode_Type;
                       Rtype : O_Tnode;
                       Arg1 : O_Enode;
                       Arg2 : O_Enode) return O_Enode
   is
   begin
      Enodes.Append (Enode_Common'(Kind => Kind,
                                   Reg => 0,
                                   Mode => Mode,
                                   Ref => False,
                                   Flag1 => False,
                                   Flag2 => False,
                                   Flag3 => False,
                                   Pad => 0,
                                   Arg1 => Arg1,
                                   Arg2 => Arg2,
                                   Info => Int32 (Rtype)));
      return Enodes.Last;
   end New_Enode;

   procedure New_Enode_Stmt (Kind : OE_Kind; Arg1 : O_Enode; Arg2 : O_Enode)
   is
   begin
      Enodes.Append (Enode_Common'(Kind => Kind,
                                   Reg => 0,
                                   Mode => Mode_Nil,
                                   Ref => False,
                                   Flag1 => False,
                                   Flag2 => False,
                                   Flag3 => False,
                                   Pad => 0,
                                   Arg1 => Arg1,
                                   Arg2 => Arg2,
                                   Info => 0));
      Link_Stmt (Enodes.Last);
   end New_Enode_Stmt;

   procedure New_Enode_Stmt
     (Kind : OE_Kind; Mode : Mode_Type; Arg1 : O_Enode; Arg2 : O_Enode)
   is
   begin
      Enodes.Append (Enode_Common'(Kind => Kind,
                                   Reg => 0,
                                   Mode => Mode,
                                   Ref => False,
                                   Flag1 => False,
                                   Flag2 => False,
                                   Flag3 => False,
                                   Pad => 0,
                                   Arg1 => Arg1,
                                   Arg2 => Arg2,
                                   Info => 0));
      Link_Stmt (Enodes.Last);
   end New_Enode_Stmt;

   Bb_Num : Int32 := 0;
   Last_Bb : O_Enode := O_Enode_Null;

   procedure Create_BB is
   begin
      New_Enode_Stmt (OE_BB, Mode_Nil, O_Enode_Null, O_Enode (Bb_Num));
      if Last_Bb /= O_Enode_Null then
         Set_BB_Next (Last_Bb, Enodes.Last);
      end if;
      Last_Bb := Enodes.Last;
      Bb_Num := Bb_Num + 1;
   end Create_BB;

   procedure Start_BB is
   begin
      if Flags.Flag_Opt_BB then
         Create_BB;
      end if;
   end Start_BB;
   pragma Inline (Start_BB);

   procedure Check_Ref (E : O_Enode) is
   begin
      if Enodes.Table (E).Ref then
         raise Syntax_Error;
      end if;
      Enodes.Table (E).Ref := True;
   end Check_Ref;

   procedure Check_Ref (E : O_Lnode) is
   begin
      Check_Ref (O_Enode (E));
   end Check_Ref;

   procedure Check_Value_Type (Val : O_Enode; Vtype : O_Tnode) is
   begin
      if Get_Enode_Type (Val) /= Vtype then
         raise Syntax_Error;
      end if;
   end Check_Value_Type;

   function New_Const_U32 (Val : Uns32; Vtype : O_Tnode) return O_Enode
   is
   begin
      return New_Enode (OE_Const, Vtype,
                        O_Enode (To_Int32 (Val)), O_Enode_Null);
   end New_Const_U32;

   Last_Decl : O_Dnode := 2;
   Cur_Block : O_Enode := O_Enode_Null;

   procedure Start_Declare_Stmt
   is
      Res : O_Enode;
   begin
      New_Enode_Stmt (OE_Beg, Cur_Block, O_Enode_Null);
      Res := Enodes.Last;
      Enodes.Table (Res).Arg2 := O_Enode
        (Ortho_Code.Decls.Start_Declare_Stmt);
      Cur_Block := Res;
   end Start_Declare_Stmt;

   function New_Stack (Rtype : O_Tnode) return O_Enode is
   begin
      return New_Enode (OE_Get_Stack, Rtype, O_Enode_Null, O_Enode_Null);
   end New_Stack;

   procedure New_Stack_Restore (Blk : O_Enode)
   is
      Save_Asgn : O_Enode;
      Save_Var : O_Dnode;
   begin
      Save_Asgn := Get_Stmt_Link (Blk);
      Save_Var := Get_Addr_Decl (Get_Assign_Target (Save_Asgn));
      New_Enode_Stmt (OE_Set_Stack, New_Value (New_Obj (Save_Var)),
                      O_Enode_Null);
   end New_Stack_Restore;

   procedure Finish_Declare_Stmt
   is
      Parent : O_Dnode;
   begin
      if Get_Block_Has_Alloca (Cur_Block) then
         New_Stack_Restore (Cur_Block);
      end if;
      New_Enode_Stmt (OE_End, Cur_Block, O_Enode_Null);
      Cur_Block := Get_Block_Parent (Cur_Block);
      if Cur_Block = O_Enode_Null then
         Parent := O_Dnode_Null;
      else
         Parent := Get_Block_Decls (Cur_Block);
      end if;
      Ortho_Code.Decls.Finish_Declare_Stmt (Parent);
   end Finish_Declare_Stmt;

   function New_Label return O_Enode is
   begin
      return New_Enode (OE_Label, Mode_Nil, O_Tnode_Null,
                        Cur_Block, O_Enode_Null);
   end New_Label;

   procedure Start_Subprogram_Body (Func : O_Dnode)
   is
      Start : O_Enode;
      D_Body : O_Dnode;
      Data : Subprogram_Data_Acc;
   begin
      if Cur_Subprg = null then
         Abi.Start_Body (Func);
      end if;

      Start := New_Enode (OE_Entry, Mode_Nil, O_Tnode_Null,
                          Last_Stmt, O_Enode_Null);
      D_Body := Decls.Start_Subprogram_Body (Func, Start);

      --  Create the corresponding decl.
      Enodes.Table (Start).Arg2 := O_Enode (D_Body);

      --  Create the data record.
      Data := new Subprogram_Data'(Parent => Cur_Subprg,
                                   First_Child => null,
                                   Last_Child => null,
                                   Brother => null,
                                   Depth => Get_Decl_Depth (Func),
                                   D_Decl => Func,
                                   E_Entry => Start,
                                   D_Body => D_Body,
                                   Exit_Label => O_Enode_Null,
                                   Last_Stmt => O_Enode_Null,
                                   Stack_Max => 0,
                                   Target => (others => <>));

      if not Flag_Debug_Hli then
         Data.Exit_Label := New_Label;
      end if;

      --  Link the record.
      if Cur_Subprg = null then
         --  A top-level subprogram.
         if First_Subprg = null then
            First_Subprg := Data;
         else
            Last_Subprg.Brother := Data;
         end if;
         Last_Subprg := Data;
      else
         --  A nested subprogram.
         if Cur_Subprg.First_Child = null then
            Cur_Subprg.First_Child := Data;
         else
            Cur_Subprg.Last_Child.Brother := Data;
         end if;
         Cur_Subprg.Last_Child := Data;

         --  Also save last_stmt.
         Cur_Subprg.Last_Stmt := Last_Stmt;
      end if;

      Cur_Subprg := Data;
      Last_Stmt := Start;

      Start_Declare_Stmt;

      --  Create a basic block for the beginning of the subprogram.
      Start_BB;

      --  Disp declarations.
      if Cur_Subprg.Parent = null then
         if Ortho_Code.Debug.Flag_Debug_Code then
            while Last_Decl <= D_Body loop
               case Get_Decl_Kind (Last_Decl) is
                  when OD_Block =>
                     --  Skip blocks.
                     Disp_Decl (1, Last_Decl);
                     Last_Decl := Get_Block_Last (Last_Decl) + 1;
                  when others =>
                     Disp_Decl (1, Last_Decl);
                     Last_Decl := Last_Decl + 1;
               end case;
            end loop;
         end if;
      end if;
   end Start_Subprogram_Body;

   procedure Finish_Subprogram_Body
   is
      Parent : Subprogram_Data_Acc;
   begin
      Finish_Declare_Stmt;

      --  Create a new basic block for the epilog.
      Start_BB;

      if not Flag_Debug_Hli then
         Link_Stmt (Cur_Subprg.Exit_Label);
      end if;

      New_Enode_Stmt (OE_Leave, O_Enode_Null, O_Enode_Null);

      --  Save last statement.
      Cur_Subprg.Last_Stmt := Enodes.Last;
      --  Set Leave of Entry.
      Set_Entry_Leave (Cur_Subprg.E_Entry, Enodes.Last);

      Decls.Finish_Subprogram_Body;

      Parent := Cur_Subprg.Parent;

      if Flags.Flag_Optimize then
         Opts.Optimize_Subprg (Cur_Subprg);
      end if;

      if Parent = null then
         --  This is a top-level subprogram.
         if Ortho_Code.Debug.Flag_Disp_Code then
            Disps.Disp_Subprg (Cur_Subprg);
         end if;
         if Ortho_Code.Debug.Flag_Dump_Code then
            Disp_Subprg_Body (1, Cur_Subprg.E_Entry);
         end if;
         if not Ortho_Code.Debug.Flag_Debug_Dump then
            Abi.Finish_Body;
         end if;
      end if;

      --  Restore Cur_Subprg.
      Cur_Subprg := Parent;

      --  Restore Last_Stmt.
      if Cur_Subprg = null then
         Last_Stmt := O_Enode_Null;
      else
         Last_Stmt := Cur_Subprg.Last_Stmt;
      end if;
   end Finish_Subprogram_Body;

   function Get_Inner_Alloca (Label : O_Enode) return O_Enode
   is
      Res : O_Enode := O_Enode_Null;
      Blk : O_Enode;
      Last_Blk : constant O_Enode := Get_Label_Block (Label);
   begin
      Blk := Cur_Block;
      while Blk /= Last_Blk loop
         if Get_Block_Has_Alloca (Blk) then
            Res := Blk;
         end if;
         Blk := Get_Block_Parent (Blk);
      end loop;
      return Res;
   end Get_Inner_Alloca;

   procedure Emit_Jmp (Code : OE_Kind; Expr : O_Enode; Label : O_Enode)
   is
   begin
      --  Discard jump after jump.
      if Code /= OE_Jump or else Get_Expr_Kind (Last_Stmt) /= OE_Jump then
         New_Enode_Stmt (Code, Expr, Label);
      end if;
   end Emit_Jmp;


   --  If there is stack allocated memory to be freed, free it.
   --  Then jump to LABEL.
   procedure New_Allocb_Jump (Label : O_Enode)
   is
      Inner_Alloca : O_Enode;
   begin
      Inner_Alloca := Get_Inner_Alloca (Label);
      if Inner_Alloca /= O_Enode_Null then
         New_Stack_Restore (Inner_Alloca);
      end if;
      Emit_Jmp (OE_Jump, O_Enode_Null, Label);
   end New_Allocb_Jump;

   function New_Lit (Lit : O_Cnode) return O_Enode
   is
      L_Type : constant O_Tnode := Get_Const_Type (Lit);
   begin
      if Flag_Debug_Hli then
         return New_Enode (OE_Lit, L_Type, O_Enode (Lit), O_Enode_Null);
      else
         case Get_Const_Kind (Lit) is
            when OC_Signed
               | OC_Unsigned
               | OC_Float
               | OC_Null
               | OC_Lit =>
               declare
                  H, L : Uns32;
               begin
                  Get_Const_Bytes (Lit, H, L);
                  return New_Enode
                    (OE_Const, L_Type,
                     O_Enode (To_Int32 (L)), O_Enode (To_Int32 (H)));
               end;
            when OC_Address =>
               raise Syntax_Error;
            when OC_Subprg_Address =>
               return New_Enode (OE_Addrd, L_Type,
                                 O_Enode (Get_Const_Decl (Lit)), O_Enode_Null);
            when OC_Array
               | OC_Record
               | OC_Record_Sizeof
               | OC_Union
               | OC_Sizeof
               | OC_Alignof
               | OC_Zero =>
               raise Syntax_Error;
         end case;
      end if;
   end New_Lit;

   function Is_Expr_S32 (Cst : O_Enode) return Boolean is
   begin
      pragma Assert (Get_Expr_Kind (Cst) = OE_Const);
      return Shift_Right_Arithmetic (Get_Expr_Low (Cst), 32)
        = Get_Expr_High (Cst);
   end Is_Expr_S32;

   function Get_Static_Chain (Depth : O_Depth) return O_Enode
   is
      Cur_Depth : O_Depth := Cur_Subprg.Depth;
      Subprg : Subprogram_Data_Acc;
      Res : O_Enode;
   begin
      if Depth = Cur_Depth then
         return New_Enode (OE_Get_Frame, Abi.Mode_Ptr, O_Tnode_Ptr,
                           O_Enode_Null, O_Enode_Null);
      else
         Subprg := Cur_Subprg;
         Res := O_Enode_Null;
         loop
            --  The static chain is the first interface of the subprogram.
            Res := New_Enode (OE_Addrl, Abi.Mode_Ptr, O_Tnode_Ptr,
                              O_Enode (Get_Subprg_Interfaces (Subprg.D_Decl)),
                              Res);
            Res := New_Enode (OE_Indir, O_Tnode_Ptr, Res, O_Enode_Null);
            Cur_Depth := Cur_Depth - 1;
            if Cur_Depth = Depth then
               return Res;
            end if;
            Subprg := Subprg.Parent;
         end loop;
      end if;
   end Get_Static_Chain;

   function New_Obj (Obj : O_Dnode) return O_Lnode
   is
      O_Type : O_Tnode;
      Kind : OE_Kind;
      Chain : O_Enode;
      Depth : O_Depth;
   begin
      O_Type := Get_Decl_Type (Obj);
      case Get_Decl_Kind (Obj) is
         when OD_Local
           | OD_Interface =>
            Kind := OE_Addrl;
            --  Local declarations are 1 deeper than their subprogram.
            Depth := Get_Decl_Depth (Obj) - 1;
            if Depth /= Cur_Subprg.Depth then
               Chain := Get_Static_Chain (Depth);
            else
               Chain := O_Enode_Null;
            end if;
         when OD_Var
           | OD_Const =>
            Kind := OE_Addrd;
            Chain := O_Enode_Null;
         when others =>
            raise Program_Error;
      end case;
      return O_Lnode (New_Enode (Kind, Abi.Mode_Ptr, O_Type,
                                 O_Enode (Obj), Chain));
   end New_Obj;

   function New_Dyadic_Op (Kind : ON_Dyadic_Op_Kind; Left, Right : O_Enode)
                          return O_Enode
   is
      L_Type : O_Tnode;
   begin
      L_Type := Get_Enode_Type (Left);
      if Flag_Debug_Assert then
         if L_Type /= Get_Enode_Type (Right) then
            raise Syntax_Error;
         end if;
         if Get_Type_Mode (L_Type) = Mode_Blk then
            raise Syntax_Error;
         end if;
         Check_Ref (Left);
         Check_Ref (Right);
      end if;

      return New_Enode (OE_Kind'Val (ON_Op_Kind'Pos (Kind)),
                        L_Type, Left, Right);
   end New_Dyadic_Op;

   function New_Monadic_Op (Kind : ON_Monadic_Op_Kind; Operand : O_Enode)
                           return O_Enode
   is
      O_Type : O_Tnode;
   begin
      O_Type := Get_Enode_Type (Operand);

      if Flag_Debug_Assert then
         if Get_Type_Mode (O_Type) = Mode_Blk then
            raise Syntax_Error;
         end if;
         Check_Ref (Operand);
      end if;

      return New_Enode (OE_Kind'Val (ON_Op_Kind'Pos (Kind)), O_Type,
                        Operand, O_Enode_Null);
   end New_Monadic_Op;

   function New_Compare_Op
     (Kind : ON_Compare_Op_Kind; Left, Right : O_Enode; Ntype : O_Tnode)
     return O_Enode
   is
      Res : O_Enode;
   begin
      if Flag_Debug_Assert then
         if Get_Enode_Type (Left) /= Get_Enode_Type (Right) then
            raise Syntax_Error;
         end if;
         if Get_Expr_Mode (Left) = Mode_Blk then
            raise Syntax_Error;
         end if;
         if Get_Type_Kind (Ntype) /= OT_Boolean then
            raise Syntax_Error;
         end if;
         Check_Ref (Left);
         Check_Ref (Right);
      end if;

      Res := New_Enode (OE_Kind'Val (ON_Op_Kind'Pos (Kind)), Ntype,
                        Left, Right);
      if Flag_Debug_Hli then
         return New_Enode (OE_Typed, Ntype, Res, O_Enode (Ntype));
      else
         return Res;
      end if;
   end New_Compare_Op;

   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Enode is
   begin
      return New_Const_U32 (Get_Type_Size (Atype), Rtype);
   end New_Sizeof;

   function New_Offsetof (Field : O_Fnode; Rtype : O_Tnode) return O_Enode is
   begin
      return New_Const_U32 (Get_Field_Offset (Field), Rtype);
   end New_Offsetof;

   function Is_Pow2 (V : Uns32) return Boolean is
   begin
      return (V and -V) = V;
   end Is_Pow2;

   function Extract_Pow2 (V : Uns32) return Uns32 is
   begin
      for I in Natural range 0 .. 31 loop
         if V = Shift_Left (1, I) then
            return Uns32 (I);
         end if;
      end loop;
      raise Program_Error;
   end Extract_Pow2;

   function New_Index_Slice_Element
     (Arr : O_Lnode; Index : O_Enode; Res_Type : O_Tnode; El_Type : O_Tnode)
     return O_Lnode
   is
      In_Type : O_Tnode;
      Sz : O_Enode;
      El_Size : Uns32;
   begin
      In_Type := Get_Enode_Type (Index);

      if Flag_Debug_Assert then
         Check_Ref (Index);
         Check_Ref (Arr);
      end if;

      --  result := arr + index * sizeof (element).
      El_Size := Get_Type_Size (El_Type);
      if El_Size = 1 then
         Sz := Index;
      elsif Get_Expr_Kind (Index) = OE_Const then
         --  FIXME: may recycle previous index?
         Sz := New_Const_U32 (Get_Expr_Low (Index) * El_Size, In_Type);
      else
         if Is_Pow2 (El_Size) and then El_Size /= 0 then
            Sz := New_Const_U32 (Extract_Pow2 (El_Size), In_Type);
            Sz := New_Enode (OE_Shl, In_Type, Index, Sz);
         else
            Sz := New_Const_U32 (El_Size, In_Type);
            Sz := New_Enode (OE_Mul, In_Type, Index, Sz);
         end if;
      end if;
      return O_Lnode (New_Enode (OE_Add, Abi.Mode_Ptr, Res_Type,
                                 O_Enode (Arr), Sz));
   end New_Index_Slice_Element;

   function New_Hli_Index_Slice
     (Kind : OE_Kind; Res_Type : O_Tnode; Arr : O_Lnode; Index : O_Enode)
     return O_Lnode
   is
   begin
      if Flag_Debug_Assert then
         Check_Ref (Index);
         Check_Ref (Arr);
      end if;
      return O_Lnode (New_Enode (Kind, Res_Type, O_Enode (Arr), Index));
   end New_Hli_Index_Slice;

   --  Get an element of an array.
   --  INDEX must be of the type of the array index.
   function New_Indexed_Element (Arr : O_Lnode; Index : O_Enode)
                                return O_Lnode
   is
      El_Type : O_Tnode;
   begin
      El_Type := Get_Type_Array_Element (Get_Enode_Type (O_Enode (Arr)));

      if Flag_Debug_Hli then
         return New_Hli_Index_Slice (OE_Index_Ref, El_Type, Arr, Index);
      else
         return New_Index_Slice_Element (Arr, Index, El_Type, El_Type);
      end if;
   end New_Indexed_Element;

   --  Get a slice of an array; this is equivalent to a conversion between
   --  an array or an array subtype and an array subtype.
   --  RES_TYPE must be an array_sub_type whose base type is the same as the
   --  base type of ARR.
   --  INDEX must be of the type of the array index.
   function New_Slice (Arr : O_Lnode; Res_Type : O_Tnode; Index : O_Enode)
                      return O_Lnode
   is
   begin
      if Flag_Debug_Hli then
         return New_Hli_Index_Slice (OE_Slice_Ref, Res_Type, Arr, Index);
      else
         return New_Index_Slice_Element
           (Arr, Index, Res_Type, Get_Type_Array_Element (Res_Type));
      end if;
   end New_Slice;

   function New_Selected_Element (Rec : O_Lnode; El : O_Fnode)
                                 return O_Lnode
   is
      Offset : Uns32;
      Off : O_Enode;
      Res_Type : O_Tnode;
   begin
      if Flag_Debug_Assert then
         Check_Ref (Rec);
      end if;

      Res_Type := Get_Field_Type (El);
      if Flag_Debug_Hli then
         return O_Lnode (New_Enode (OE_Record_Ref, Res_Type,
                                    O_Enode (Rec), O_Enode (El)));
      else
         Offset := Get_Field_Offset (El);
         if Offset = 0 then
            return O_Lnode (New_Enode (OE_Conv_Ptr, Abi.Mode_Ptr, Res_Type,
                                       O_Enode (Rec), O_Enode (Res_Type)));
         else
            Off := New_Enode (OE_Const, Mode_U32, O_Tnode_Null,
                              O_Enode (Offset), O_Enode_Null);

            return O_Lnode (New_Enode (OE_Add, Abi.Mode_Ptr, Res_Type,
                                       O_Enode (Rec), Off));
         end if;
      end if;
   end New_Selected_Element;

   function New_Access_Element (Acc : O_Enode) return O_Lnode
   is
      Acc_Type : O_Tnode;
      Res_Type : O_Tnode;
   begin
      Acc_Type := Get_Enode_Type (Acc);

      if Flag_Debug_Assert then
         if Get_Type_Kind (Acc_Type) /= OT_Access then
            raise Syntax_Error;
         end if;
         Check_Ref (Acc);
      end if;

      Res_Type := Get_Type_Access_Type (Acc_Type);
      if Flag_Debug_Hli then
         return O_Lnode (New_Enode (OE_Access_Ref, Abi.Mode_Ptr, Res_Type,
                                    Acc, O_Enode_Null));
      else
         return O_Lnode (New_Enode (OE_Conv_Ptr, Abi.Mode_Ptr, Res_Type,
                                    Acc, O_Enode (Res_Type)));
      end if;
   end New_Access_Element;

   function New_Convert_Ov (Val : O_Enode; Rtype : O_Tnode) return O_Enode is
   begin
      if Flag_Debug_Assert then
         Check_Ref (Val);
      end if;

      return New_Enode (OE_Conv_Ov, Rtype, Val, O_Enode (Rtype));
   end New_Convert_Ov;

   function New_Convert (Val : O_Enode; Rtype : O_Tnode) return O_Enode is
   begin
      if Flag_Debug_Assert then
         Check_Ref (Val);
      end if;

      return New_Enode (OE_Conv, Rtype, Val, O_Enode (Rtype));
   end New_Convert;

   function New_Unchecked_Address (Lvalue : O_Lnode; Atype : O_Tnode)
                                  return O_Enode is
   begin
      if Flag_Debug_Assert then
         if Get_Type_Kind (Atype) /= OT_Access then
            raise Syntax_Error;
         end if;
         Check_Ref (Lvalue);
      end if;

      return New_Enode (OE_Conv_Ptr, Abi.Mode_Ptr, Atype,
                        O_Enode (Lvalue), O_Enode (Atype));
   end New_Unchecked_Address;

   function New_Address (Lvalue : O_Lnode; Atype : O_Tnode) return O_Enode is
   begin
      if Flag_Debug_Assert then
         if Get_Type_Kind (Atype) /= OT_Access then
            raise Syntax_Error;
         end if;
         if Get_Base_Type (Get_Enode_Type (O_Enode (Lvalue)))
           /= Get_Base_Type (Get_Type_Access_Type (Atype))
         then
            raise Syntax_Error;
         end if;
         Check_Ref (Lvalue);
      end if;

      return New_Enode (OE_Conv_Ptr, Abi.Mode_Ptr, Atype,
                        O_Enode (Lvalue), O_Enode (Atype));
   end New_Address;

   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
                                   return O_Enode is
   begin
      raise Program_Error;
      return O_Enode_Null;
   end New_Subprogram_Address;

   function New_Value (Lvalue : O_Lnode) return O_Enode
   is
      V_Type : O_Tnode;
   begin
      V_Type := Get_Enode_Type (O_Enode (Lvalue));

      if Flag_Debug_Assert then
         Check_Ref (Lvalue);
      end if;

      return New_Enode (OE_Indir, V_Type, O_Enode (Lvalue), O_Enode_Null);
   end New_Value;

   function New_Alloca (Rtype : O_Tnode; Size : O_Enode) return O_Enode
   is
      Save_Var : O_Dnode;
      Stmt : O_Enode;
      St_Type : O_Tnode;
   begin
      if Flag_Debug_Assert then
         Check_Ref (Size);
         if Get_Type_Kind (Rtype) /= OT_Access then
            raise Syntax_Error;
         end if;
         if Get_Type_Kind (Get_Enode_Type (Size)) /= OT_Unsigned then
            raise Syntax_Error;
         end if;
      end if;

      if not Get_Block_Has_Alloca (Cur_Block) then
         Set_Block_Has_Alloca (Cur_Block, True);
         if Stack_Ptr_Type /= O_Tnode_Null then
            St_Type := Stack_Ptr_Type;
         else
            St_Type := Rtype;
         end if;
         --  Add a decl.
         New_Var_Decl (Save_Var, O_Ident_Nul, O_Storage_Local, St_Type);
         --  Add insn to save stack ptr.
         Stmt := New_Enode (OE_Asgn, St_Type,
                            New_Stack (St_Type),
                            O_Enode (New_Obj (Save_Var)));
         if Cur_Block = Last_Stmt then
            Set_Stmt_Link (Last_Stmt, Stmt);
            Last_Stmt := Stmt;
         else
            Set_Stmt_Link (Stmt, Get_Stmt_Link (Cur_Block));
            Set_Stmt_Link (Cur_Block, Stmt);
         end if;
      end if;

      return New_Enode (OE_Alloca, Rtype, Size, O_Enode (Rtype));
   end New_Alloca;

   procedure Start_Association (Assocs : out O_Assoc_List; Subprg : O_Dnode)
   is
      Depth : O_Depth;
      Arg : O_Enode;
      First_Inter : O_Dnode;
   begin
      First_Inter := Get_Subprg_Interfaces (Subprg);
      if Get_Decl_Storage (Subprg) = O_Storage_Local then
         Depth := Get_Decl_Depth (Subprg);
         Arg := New_Enode (OE_Arg, Abi.Mode_Ptr, O_Tnode_Ptr,
                           Get_Static_Chain (Depth - 1), O_Enode_Null);
         First_Inter := Get_Interface_Chain (First_Inter);
      else
         Arg := O_Enode_Null;
      end if;
      Assocs := (Subprg => Subprg,
                 First_Arg => Arg,
                 Last_Arg => Arg,
                 Next_Inter => First_Inter);
   end Start_Association;

   procedure New_Association (Assocs : in out O_Assoc_List; Val : O_Enode)
   is
      V_Type : O_Tnode;
      Mode : Mode_Type;
      N_Mode : Mode_Type;
      Res : O_Enode;
   begin
      V_Type := Get_Enode_Type (Val);

      if Flag_Debug_Assert then
         if Assocs.Next_Inter = O_Dnode_Null then
            --  More assocs than interfaces.
            raise Syntax_Error;
         end if;
         Check_Value_Type (Val, Get_Decl_Type (Assocs.Next_Inter));
         Check_Ref (Val);
      end if;

      --  Follow the C convention call: no parameters shorter than int.
      Mode := Get_Type_Mode (V_Type);
      case Mode is
         when Mode_B2
           | Mode_U8
           | Mode_U16 =>
            N_Mode := Mode_U32;
         when Mode_I8
           | Mode_I16 =>
            N_Mode := Mode_I32;
         when Mode_P32
           | Mode_U32
           | Mode_I32
           | Mode_U64
           | Mode_I64
           | Mode_P64
           | Mode_F32
           | Mode_F64 =>
            N_Mode := Mode;
         when Mode_Blk
           | Mode_Nil
           | Mode_X1 =>
            raise Program_Error;
      end case;
      if N_Mode /= Mode and not Flag_Debug_Hli then
         Res := New_Enode (OE_Conv_Ov, N_Mode, V_Type, Val, O_Enode (V_Type));
      else
         Res := Val;
      end if;
      Res := New_Enode (OE_Arg, N_Mode, V_Type, Res, O_Enode_Null);
      if Assocs.Last_Arg /= O_Enode_Null then
         Enodes.Table (Assocs.Last_Arg).Arg2 := Res;
      else
         Assocs.First_Arg := Res;
      end if;
      Assocs.Last_Arg := Res;
      Assocs.Next_Inter := Get_Interface_Chain (Assocs.Next_Inter);
   end New_Association;

   function New_Function_Call (Assocs : O_Assoc_List) return O_Enode
   is
      F_Type : O_Tnode;
   begin
      if Flag_Debug_Assert then
         if Assocs.Next_Inter /= O_Dnode_Null then
            --  Not enough assocs.
            raise Syntax_Error;
         end if;
      end if;

      F_Type := Get_Decl_Type (Assocs.Subprg);
      return New_Enode (OE_Call, F_Type,
                        O_Enode (Assocs.Subprg), Assocs.First_Arg);
   end New_Function_Call;

   procedure New_Procedure_Call (Assocs : in out O_Assoc_List) is
   begin
      if Flag_Debug_Assert then
         if Assocs.Next_Inter /= O_Dnode_Null then
            --  Not enough assocs.
            raise Syntax_Error;
         end if;
      end if;
      New_Enode_Stmt (OE_Call, O_Enode (Assocs.Subprg), Assocs.First_Arg);
   end New_Procedure_Call;

   procedure New_Assign_Stmt (Target : O_Lnode; Value : O_Enode)
   is
      V_Type : O_Tnode;
   begin
      V_Type := Get_Enode_Type (Value);

      if Flag_Debug_Assert then
         Check_Value_Type (Value, Get_Enode_Type (O_Enode (Target)));
         Check_Ref (Value);
         Check_Ref (Target);
      end if;

      New_Enode_Stmt (OE_Asgn, Get_Type_Mode (V_Type),
                      Value, O_Enode (Target));
   end New_Assign_Stmt;

   procedure New_Return_Stmt (Value : O_Enode)
   is
      V_Type : O_Tnode;
   begin
      V_Type := Get_Enode_Type (Value);

      if Flag_Debug_Assert then
         Check_Ref (Value);
         Check_Value_Type (Value, Get_Decl_Type (Cur_Subprg.D_Decl));
      end if;

      New_Enode_Stmt (OE_Ret, Get_Type_Mode (V_Type), Value, O_Enode_Null);
      if not Flag_Debug_Hli then
         New_Allocb_Jump (Cur_Subprg.Exit_Label);
      end if;
   end New_Return_Stmt;

   procedure New_Return_Stmt is
   begin
      if Flag_Debug_Assert then
         if Get_Decl_Kind (Cur_Subprg.D_Decl) /= OD_Procedure then
            raise Syntax_Error;
         end if;
      end if;

      if not Flag_Debug_Hli then
         New_Allocb_Jump (Cur_Subprg.Exit_Label);
      else
         New_Enode_Stmt (OE_Ret, Mode_Nil, O_Enode_Null, O_Enode_Null);
      end if;
   end New_Return_Stmt;


   procedure Start_If_Stmt (Block : out O_If_Block; Cond : O_Enode) is
   begin
      if Flag_Debug_Assert then
         if Get_Expr_Mode (Cond) /= Mode_B2 then
            --  COND must be a boolean.
            raise Syntax_Error;
         end if;
         Check_Ref (Cond);
      end if;

      if not Flag_Lower_Stmt then
         New_Enode_Stmt (OE_If, Cond, O_Enode_Null);
         Block := (Label_End => O_Enode_Null,
                   Label_Next => Last_Stmt);
      else
         Block := (Label_End => O_Enode_Null,
                   Label_Next => New_Label);
         Emit_Jmp (OE_Jump_F, Cond, Block.Label_Next);
         Start_BB;
      end if;
   end Start_If_Stmt;

   procedure New_Else_Stmt (Block : in out O_If_Block) is
   begin
      if not Flag_Lower_Stmt then
         New_Enode_Stmt (OE_Else, O_Enode_Null, O_Enode_Null);
      else
         if Block.Label_End = O_Enode_Null then
            Block.Label_End := New_Label;
         end if;
         Emit_Jmp (OE_Jump, O_Enode_Null, Block.Label_End);
         Start_BB;
         Link_Stmt (Block.Label_Next);
         Block.Label_Next := O_Enode_Null;
      end if;
   end New_Else_Stmt;

   procedure Finish_If_Stmt (Block : in out O_If_Block) is
   begin
      if not Flag_Lower_Stmt then
         New_Enode_Stmt (OE_Endif, O_Enode_Null, O_Enode_Null);
      else
         --  Create a badic-block after the IF.
         Start_BB;
         if Block.Label_Next /= O_Enode_Null then
            Link_Stmt (Block.Label_Next);
         end if;
         if Block.Label_End /= O_Enode_Null then
            Link_Stmt (Block.Label_End);
         end if;
      end if;
   end Finish_If_Stmt;

   procedure Start_Loop_Stmt (Label : out O_Snode) is
   begin
      if not Flag_Lower_Stmt then
         New_Enode_Stmt (OE_Loop, O_Enode_Null, O_Enode_Null);
         Label := (Label_Start => Last_Stmt,
                   Label_End => O_Enode_Null);
      else
         --  Create a basic-block at the beginning of the loop.
         Start_BB;
         Label.Label_Start := New_Label;
         Link_Stmt (Label.Label_Start);
         Label.Label_End := New_Label;
      end if;
   end Start_Loop_Stmt;

   procedure Finish_Loop_Stmt (Label : in out O_Snode)
   is
   begin
      if not Flag_Lower_Stmt then
         New_Enode_Stmt (OE_Eloop, Label.Label_Start, O_Enode_Null);
      else
         Emit_Jmp (OE_Jump, O_Enode_Null, Label.Label_Start);
         Start_BB;
         Link_Stmt (Label.Label_End);
      end if;
   end Finish_Loop_Stmt;

   procedure New_Exit_Stmt (L : O_Snode)
   is
   begin
      if not Flag_Lower_Stmt then
         New_Enode_Stmt (OE_Exit, O_Enode_Null, L.Label_Start);
      else
         New_Allocb_Jump (L.Label_End);
      end if;
   end New_Exit_Stmt;

   procedure New_Next_Stmt (L : O_Snode)
   is
   begin
      if not Flag_Lower_Stmt then
         New_Enode_Stmt (OE_Next, O_Enode_Null, L.Label_Start);
      else
         New_Allocb_Jump (L.Label_Start);
      end if;
   end New_Next_Stmt;

   procedure Start_Case_Stmt (Block : out O_Case_Block; Value : O_Enode)
   is
      V_Type : O_Tnode;
      Mode : Mode_Type;
      Start : O_Enode;
   begin
      V_Type := Get_Enode_Type (Value);
      Mode := Get_Type_Mode (V_Type);

      if Flag_Debug_Assert then
         Check_Ref (Value);
         case Mode is
            when Mode_U8 .. Mode_U64
              | Mode_I8 .. Mode_I64
              | Mode_B2 =>
               null;
            when others =>
               raise Syntax_Error;
         end case;
      end if;

      New_Enode_Stmt (OE_Case, Mode, Value, O_Enode_Null);
      Start := Enodes.Last;
      if Flag_Debug_Hli then
         Block := (Expr => Start,
                   Expr_Type => V_Type,
                   Last_Node => O_Enode_Null,
                   Label_End => O_Enode_Null,
                   Label_Branch => Start);
      else
         Block := (Expr => Start,
                   Expr_Type => V_Type,
                   Last_Node => Start,
                   Label_End => New_Label,
                   Label_Branch => O_Enode_Null);
      end if;
   end Start_Case_Stmt;

   procedure Start_Choice (Block : in out O_Case_Block)
   is
      B : O_Enode;
   begin
      if Flag_Debug_Hli then
         B := New_Enode (OE_Case_Branch, Mode_Nil, O_Tnode_Null,
                         O_Enode_Null, O_Enode_Null);
         Link_Stmt (B);
         --  Link it.
         Set_Case_Branch (Block.Label_Branch, B);
         Block.Label_Branch := B;
      else
         --  Jump to the end of the case statement.
         --  If there is already a branch open, this is ok
         --   (do not fall-through).
         --  If there is no branch open, then this is the default choice
         --   (nothing to do).
         Emit_Jmp (OE_Jump, O_Enode_Null, Block.Label_End);

         --  Create a label for the code of this branch.
         Block.Label_Branch := New_Label;
      end if;
   end Start_Choice;

   procedure Insert_Choice_Stmt (Block : in out O_Case_Block; Stmt : O_Enode)
   is
      Prev : O_Enode;
   begin
      Prev := Get_Stmt_Link (Block.Last_Node);
      Set_Stmt_Link (Block.Last_Node, Stmt);
      Block.Last_Node := Stmt;
      if Prev = O_Enode_Null then
         Last_Stmt := Stmt;
      else
         Set_Stmt_Link (Stmt, Prev);
      end if;
   end Insert_Choice_Stmt;

   procedure Emit_Choice_Jmp (Block : in out O_Case_Block;
                              Code : OE_Kind; Expr : O_Enode; Label : O_Enode)
   is
      Jmp : O_Enode;
   begin
      Jmp := New_Enode (Code, Mode_Nil, O_Tnode_Null, Expr, Label);
      Insert_Choice_Stmt (Block, Jmp);
   end Emit_Choice_Jmp;

   --  Create a node containing the value of the case expression.
   function New_Case_Expr (Block : O_Case_Block) return O_Enode is
   begin
      return New_Enode (OE_Case_Expr, Block.Expr_Type,
                        Block.Expr, O_Enode_Null);
   end New_Case_Expr;

   procedure New_Hli_Choice (Block : in out O_Case_Block;
                             Hi, Lo : O_Enode)
   is
      Res : O_Enode;
   begin
      Res := New_Enode (OE_Case_Choice, Mode_Nil, O_Tnode_Null, Hi, Lo);
      if Block.Label_End = O_Enode_Null then
         Set_Case_Branch_Choice (Block.Label_Branch, Res);
      else
         Set_Case_Choice_Link (Block.Label_End, Res);
      end if;
      Block.Label_End := Res;
   end New_Hli_Choice;

   procedure New_Expr_Choice (Block : in out O_Case_Block; Expr : O_Cnode)
   is
      Res : O_Enode;
   begin
      if Flag_Debug_Hli then
         New_Hli_Choice (Block, New_Lit (Expr), O_Enode_Null);
      else
         Res := New_Enode (OE_Eq, Mode_B2, O_Tnode_Null,
                           New_Case_Expr (Block), New_Lit (Expr));
         Emit_Choice_Jmp (Block, OE_Jump_T, Res, Block.Label_Branch);
      end if;
   end New_Expr_Choice;

   procedure New_Range_Choice (Block : in out O_Case_Block;
                               Low, High : O_Cnode)
   is
      E1 : O_Enode;
      E2 : O_Enode;
      Label : O_Enode;
   begin
      if Flag_Debug_Hli then
         New_Hli_Choice (Block, New_Lit (Low), New_Lit (High));
      else
         --  Internal label.
         Label := New_Label;
         E1 := New_Enode (OE_Lt, Mode_B2, O_Tnode_Null,
                          New_Case_Expr (Block), New_Lit (Low));
         Emit_Choice_Jmp (Block, OE_Jump_T, E1, Label);
         E2 := New_Enode (OE_Le, Mode_B2, O_Tnode_Null,
                          New_Case_Expr (Block), New_Lit (High));
         Emit_Choice_Jmp (Block, OE_Jump_T, E2, Block.Label_Branch);
         Insert_Choice_Stmt (Block, Label);
      end if;
   end New_Range_Choice;

   procedure New_Default_Choice (Block : in out O_Case_Block) is
   begin
      if Flag_Debug_Hli then
         New_Hli_Choice (Block, O_Enode_Null, O_Enode_Null);
      else
         --  Jump to the code.
         Emit_Choice_Jmp (Block, OE_Jump, O_Enode_Null, Block.Label_Branch);
      end if;
   end New_Default_Choice;

   procedure Finish_Choice (Block : in out O_Case_Block) is
   begin
      if Flag_Debug_Hli then
         Block.Label_End := O_Enode_Null;
      else
         --  Put the label of the branch.
         Start_BB;
         Link_Stmt (Block.Label_Branch);
      end if;
   end Finish_Choice;

   procedure Finish_Case_Stmt (Block : in out O_Case_Block) is
   begin
      if Flag_Debug_Hli then
         New_Enode_Stmt (OE_Case_End, O_Enode_Null, O_Enode_Null);
      else
         --  Jump to the end of the case statement.
         --  Note: this is not required, since the next instruction is the
         --   label.
         --  Emit_Jmp (OE_Jump, O_Enode_Null, Block.Label_End);

         --  Put the label of the end of the case.
         Start_BB;
         Link_Stmt (Block.Label_End);
         Block.Label_End := O_Enode_Null;
      end if;
   end Finish_Case_Stmt;

   procedure New_Debug_Line_Stmt (Line : Natural) is
   begin
      New_Enode_Stmt (OE_Line, O_Enode (Line), O_Enode_Null);
   end New_Debug_Line_Stmt;

   procedure Debug_Expr (N : O_Enode)
   is
      use Ada.Text_IO;
      use Ortho_Code.Debug.Int32_IO;
      Indent : constant Count := Col;
   begin
      Put (Int32 (N), 0);
      Set_Col (Indent + 7);
      Disp_Mode (Get_Expr_Mode (N));
      Put ("  ");
      Put (OE_Kind'Image (Get_Expr_Kind (N)));
      Set_Col (Indent + 28);
--       Put (Abi.Image_Insn (Get_Expr_Insn (N)));
--       Put ("  ");
      Put (Abi.Image_Reg (Get_Expr_Reg (N)));
      Put ("  ");
      Put (Int32 (Enodes.Table (N).Arg1), 7);
      Put (Int32 (Enodes.Table (N).Arg2), 7);
      Put (Enodes.Table (N).Info, 7);
      New_Line;
   end Debug_Expr;

   procedure Disp_Subprg_Body (Indent : Natural; Subprg : O_Enode)
   is
      use Ada.Text_IO;
      N : O_Enode;
      N_Indent : Natural;
   begin
      N := Subprg;
      if Get_Expr_Kind (N) /= OE_Entry then
         raise Program_Error;
      end if;
      --  Display the entry.
      Set_Col (Count (Indent));
      Debug_Expr (N);
      --  Display the subprogram, binding.
      N_Indent := Indent;-- + 1;
      N := N + 1;
      loop
         case Get_Expr_Kind (N) is
            when OE_Entry =>
               N := Get_Entry_Leave (N) + 1;
            when OE_Leave =>
               Set_Col (Count (Indent));
               Debug_Expr (N);
               exit;
            when others =>
               Set_Col (Count (N_Indent));
               Debug_Expr (N);
               case Get_Expr_Kind (N) is
                  when OE_Beg =>
                     Disp_Block (N_Indent + 2,
                                 O_Dnode (Enodes.Table (N).Arg2));
                     N_Indent := N_Indent + 1;
                  when OE_End =>
                     N_Indent := N_Indent - 1;
                  when others =>
                     null;
               end case;
               N := N + 1;
         end case;
      end loop;
   end Disp_Subprg_Body;

   procedure Disp_All_Enode is
   begin
      for I in Enodes.First .. Enodes.Last loop
         Debug_Expr (I);
      end loop;
   end Disp_All_Enode;

   Max_Enode : O_Enode := O_Enode_Null;

   procedure Mark (M : out Mark_Type) is
   begin
      M.Enode := Enodes.Last;
   end Mark;

   procedure Release (M : Mark_Type) is
   begin
      Max_Enode := O_Enode'Max (Max_Enode, Enodes.Last);
      Enodes.Set_Last (M.Enode);
   end Release;

   procedure Disp_Stats
   is
      use Ada.Text_IO;
   begin
      Max_Enode := O_Enode'Max (Max_Enode, Enodes.Last);
      Put ("Number of Enodes:" & O_Enode'Image (Enodes.Last));
      Put (", max:" & O_Enode'Image (Max_Enode));
      New_Line;
   end Disp_Stats;

   procedure Free_Subprogram_Data (Data : in out Subprogram_Data_Acc)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Subprogram_Data, Subprogram_Data_Acc);
      Ch, N_Ch : Subprogram_Data_Acc;
   begin
      Ch := Data.First_Child;
      while Ch /= null loop
         N_Ch := Ch.Brother;
         Free_Subprogram_Data (Ch);
         Ch := N_Ch;
      end loop;
      Free (Data);
   end Free_Subprogram_Data;

   procedure Finish is
   begin
      Enodes.Free;
      Free_Subprogram_Data (First_Subprg);
   end Finish;
end Ortho_Code.Exprs;
