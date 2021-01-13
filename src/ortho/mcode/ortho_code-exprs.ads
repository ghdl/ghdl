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
with Ortho_Code.Abi;

package Ortho_Code.Exprs is
   type OE_Kind is
     (
      OE_Nil,

      --  Dyadic operations.
      --  ARG1 is left, ARG2 is right.
      OE_Add_Ov,
      OE_Sub_Ov,
      OE_Mul_Ov,
      OE_Div_Ov,
      OE_Rem,
      OE_Mod,

      OE_And,
      OE_Or,
      OE_Xor,

      --  Monadic operations.
      --  ARG1 is expression.
      OE_Not,
      OE_Neg_Ov,
      OE_Abs_Ov,

      --  Comparaison.
      --  ARG1 is left, ARG2 is right.
      OE_Eq,
      OE_Neq,
      OE_Le,
      OE_Lt,
      OE_Ge,
      OE_Gt,

      --  Without checks, for addresses.
      OE_Add,
      OE_Mul,
      OE_Shl, --  Left shift

      --  A literal.
      --  ARG1 is low part, ARG2 is high part.
      OE_Const,

      --  Address of a local variable/parameter.
      --  ARG1 is object.
      --  ARG2 is the frame pointer or O_Enode_Null for current frame pointer.
      OE_Addrl,
      --  Address of a declaration.
      --  ARG1 is the declaration.
      OE_Addrd,

      --  Pointer dereference.
      --  ARG1 is operand.
      OE_Indir,

      --  Conversion.
      --  ARG1 is expression.
      --  ARG2: type
      OE_Conv_Ptr,
      OE_Conv_Ov,
      OE_Conv,

      --  Typed expression.
      OE_Typed,

      --  Local memory allocation.
      --  ARG1 is size (in bytes).
      OE_Alloca,

      --  Statements.

      --  Subrogram entry.
      --  ARG1 is the corresponding Leave (used to skip inner subprograms).
      --  ARG2 is unused.
      OE_Entry,
      --  Subprogram exit.
      --  ARG1 and ARG2 are unused.
      OE_Leave,

      --  Declaration blocks.
      --  ARG1: parent
      --  ARG2: corresponding declarations.
      OE_Beg,
      --  ARG1: corresponding beg
      --  ARG2: unsused.
      OE_End,

      --  Assignment.
      --  ARG1 is value, ARG2 is target (address).
      OE_Asgn,

      --  Subprogram calls.
      --  ARG1 is value
      --  ARG2 is link to the next argument.
      OE_Arg,
      --  ARG1 is subprogram
      --  ARG2 is arguments.
      OE_Call,
      --  ARG1 is intrinsic operation.
      OE_Intrinsic,

      --  Modify the stack pointer value, to align the stack before pushing
      --  arguments, or to free the stack.
      --  ARG1 is the signed offset.
      OE_Stack_Adjust,

      --  Return ARG1 (if not mode_nil) from current subprogram.
      --  ARG1: expression.
      OE_Ret,

      --  Line number (for debugging).
      --  ARG1: line number
      OE_Line,

      --  High level instructions.

      --  Basic block.
      --  ARG1: next BB
      --  ARG2: number
      OE_BB,

      --  ARG1 is the literal.
      OE_Lit,
      --  ARG1: value
      --  ARG2: first branch (HLI only).
      OE_Case,
      --  ARG1: the corresponding OE_Case
      OE_Case_Expr,
      --  ARG1: left bound
      --  ARG2: right bound
      --  LINK: choice link
      OE_Case_Choice,
      --  ARG1: choice link
      --  ARG2: next branch
      OE_Case_Branch,
      --  End of case.
      OE_Case_End,

      --  ARG1: the condition
      --  ARG2: the else/endif
      OE_If,
      OE_Else,
      OE_Endif,

      --  ARG1: loop level.
      OE_Loop,
      --  ARG1: loop.
      OE_Eloop,
      --  ARG2: loop.
      OE_Next,
      OE_Exit,

      --  ARG1: the record
      --  ARG2: the field
      OE_Record_Ref,

      --  ARG1: the expression.
      OE_Access_Ref,

      --  ARG1: the array
      --  ARG2: the index
      OE_Index_Ref,
      OE_Slice_Ref,

      --  Low level instructions.

      --  Label.
      --  ARG1: current block (used for alloca), only during tree building.
      --  ARG2: user info (generally used to store symbol).
      OE_Label,

      --  Jump to ARG2.
      OE_Jump,

      --  Jump to ARG2 if ARG1 is true/false.
      OE_Jump_T,
      OE_Jump_F,

      --  Used internally only.
      --  ARG2 is info/target, ARG1 is expression (if any).
      OE_Spill,
      OE_Reload,
      OE_Move,

      --  Alloca/allocb handling.
      OE_Get_Stack,
      OE_Set_Stack,

      --  Get current frame pointer.
      OE_Get_Frame,

      --  Additionnal reg
      OE_Reg
      );
   for OE_Kind'Size use 8;

   subtype OE_Kind_Dyadic is OE_Kind range OE_Add_Ov .. OE_Xor;
   subtype OE_Kind_Cmp is OE_Kind range OE_Eq .. OE_Gt;

   --  BE representation of an instruction.
   type O_Insn is mod 256;

   type Subprogram_Data;
   type Subprogram_Data_Acc is access Subprogram_Data;

   type Subprogram_Data is record
      --  Parent or null if top-level subprogram.
      Parent : Subprogram_Data_Acc;

      --  Block in which this subprogram is declared, or o_dnode_null if
      --  top-level subprogram.
      --Parent_Block : O_Dnode;

      --  First and last child, or null if no children.
      First_Child : Subprogram_Data_Acc;
      Last_Child : Subprogram_Data_Acc;

      --  Next subprogram at the same depth level.
      Brother : Subprogram_Data_Acc;

      --  Depth of the subprogram.
      Depth : O_Depth;

      --  Dnode for the declaration.
      D_Decl : O_Dnode;

      --  Enode for the Entry.
      E_Entry : O_Enode;

      --  Dnode for the Body.
      D_Body : O_Dnode;

      --  Label just before leave.
      Exit_Label : O_Enode;

      --  Last statement of this subprogram.
      Last_Stmt : O_Enode;

      --  Static maximum stack use.
      Stack_Max : Uns32;

      --  Target specific data.
      Target : Abi.Target_Subprg;
   end record;

   --  Data for the current subprogram.
   Cur_Subprg : Subprogram_Data_Acc := null;

   --  First and last (top-level) subprogram.
   First_Subprg : Subprogram_Data_Acc := null;
   Last_Subprg : Subprogram_Data_Acc := null;

   --  Type of the stack pointer - for OE_Get_Stack and OE_Set_Stack.
   --  Can be set by back-ends.
   Stack_Ptr_Type : O_Tnode := O_Tnode_Null;

   --  Create a new node.
   --  Should be used only by back-end to add internal nodes.
   function New_Enode (Kind : OE_Kind;
                       Mode : Mode_Type;
                       Rtype : O_Tnode;
                       Arg1 : O_Enode;
                       Arg2 : O_Enode) return O_Enode;

   --  Get the kind of ENODE.
   function Get_Expr_Kind (Enode : O_Enode) return OE_Kind;
   pragma Inline (Get_Expr_Kind);

   --  Get the mode of ENODE.
   function Get_Expr_Mode (Enode : O_Enode) return Mode_Type;
   pragma Inline (Get_Expr_Mode);

   --  Get/Set the register of ENODE.
   function Get_Expr_Reg (Enode : O_Enode) return O_Reg;
   procedure Set_Expr_Reg (Enode : O_Enode; Reg : O_Reg);
   pragma Inline (Get_Expr_Reg);
   pragma Inline (Set_Expr_Reg);

   --  Get the operand of an unary expression.
   function Get_Expr_Operand (Enode : O_Enode) return O_Enode;
   procedure Set_Expr_Operand (Enode : O_Enode; Val : O_Enode);

   --  Get left/right operand of a binary expression.
   function Get_Expr_Left (Enode : O_Enode) return O_Enode;
   function Get_Expr_Right (Enode : O_Enode) return O_Enode;
   procedure Set_Expr_Left (Enode : O_Enode; Val : O_Enode);
   procedure Set_Expr_Right (Enode : O_Enode; Val : O_Enode);

   --  Get the low and high part of an OE_CONST node.
   function Get_Expr_Low (Cst : O_Enode) return Uns32;
   function Get_Expr_High (Cst : O_Enode) return Uns32;

   --  Help for OE_CONST: return True iff the value is a signed 32 bit value.
   function Is_Expr_S32 (Cst : O_Enode) return Boolean;

   --  Get target of the assignment.
   function Get_Assign_Target (Enode : O_Enode) return O_Enode;
   procedure Set_Assign_Target (Enode : O_Enode; Targ : O_Enode);

   --  For OE_Lit: get the literal.
   function Get_Expr_Lit (Lit : O_Enode) return O_Cnode;

   --  Type of a OE_Conv/OE_Nop/OE_Typed/OE_Alloca
   --  Used only for display/debugging purposes.
   function Get_Conv_Type (Enode : O_Enode) return O_Tnode;

   --  Leave node corresponding to the entry.
   function Get_Entry_Leave (Enode : O_Enode) return O_Enode;

   --  Get the label of a jump/ret
   function Get_Jump_Label (Enode : O_Enode) return O_Enode;
   procedure Set_Jump_Label (Enode : O_Enode; Label : O_Enode);

   --  Get the declaration of addrl,addrp,addrs
   function Get_Addr_Decl (Enode : O_Enode) return O_Dnode;

   --  Get the object of addrg
   function Get_Addr_Object (Enode : O_Enode) return O_Lnode;

   --  Get the computed frame for the object.
   --  If O_Enode_Null, then use current frame.
   function Get_Addrl_Frame (Enode : O_Enode) return O_Enode;
   procedure Set_Addrl_Frame (Enode : O_Enode; Frame : O_Enode);

   --  Return the stack adjustment. For positive values, this is the amount of
   --  bytes to allocate on the stack before pushing arguments, so that the
   --  stack pointer stays aligned. For negtive values, this is the amount of
   --  bytes to release on the stack.
   function Get_Stack_Adjust (Enode : O_Enode) return Int32;
   procedure Set_Stack_Adjust (Enode : O_Enode; Off : Int32);

   --  Get the subprogram called by ENODE.
   function Get_Call_Subprg (Enode : O_Enode) return O_Dnode;

   --  Get the first argument of a call, or the next argument of an arg.
   function Get_Arg_Link (Enode : O_Enode) return O_Enode;

   --  Get the declaration chain of a Beg statement.
   function Get_Block_Decls (Blk : O_Enode) return O_Dnode;

   --  Get the parent of the block.
   function Get_Block_Parent (Blk : O_Enode) return O_Enode;

   --  Get the corresponding beg.
   function Get_End_Beg (Blk : O_Enode) return O_Enode;

   --  True if the block contains an alloca insn.
   function Get_Block_Has_Alloca (Blk : O_Enode) return Boolean;

   --  Set the next branch of a case/case_branch.
   procedure Set_Case_Branch (C : O_Enode; Branch : O_Enode);

   --  Set the first choice of a case branch.
   procedure Set_Case_Branch_Choice (Branch : O_Enode; Choice : O_Enode);
   function Get_Case_Branch_Choice (Branch : O_Enode) return O_Enode;

   --  Set the choice link of a case choice.
   procedure Set_Case_Choice_Link (Choice : O_Enode; N_Choice : O_Enode);
   function Get_Case_Choice_Link (Choice : O_Enode) return O_Enode;

   --  Get/Set the max stack size for the end block BLKE.
   --function Get_Block_Max_Stack (Blke : O_Enode) return Int32;
   --procedure Set_Block_Max_Stack (Blke : O_Enode; Max : Int32);

   --  Get the field of an o_record_ref node.
   function Get_Ref_Field (Ref : O_Enode) return O_Fnode;

   --  Get the index of an OE_Index_Ref or OE_Slice_Ref node.
   function Get_Ref_Index (Ref : O_Enode) return O_Enode;

   --  Get/Set the info field of a label.
   function Get_Label_Info (Label : O_Enode) return Int32;
   procedure Set_Label_Info (Label : O_Enode; Info : Int32);

   --  Get the info of a spill.
   function Get_Spill_Info (Spill : O_Enode) return Int32;
   procedure Set_Spill_Info (Spill : O_Enode; Info : Int32);

   --  Get the statement link.
   function Get_Stmt_Link (Stmt : O_Enode) return O_Enode;
   procedure Set_Stmt_Link (Stmt : O_Enode; Next : O_Enode);

   --  Get the line number of an OE_Line statement.
   function Get_Expr_Line_Number (Stmt : O_Enode) return Int32;

   --  Get the operation of an intrinsic.
   function Get_Intrinsic_Operation (Stmt : O_Enode) return Int32;

   --  Get the basic block label (uniq number).
   function Get_BB_Number (Stmt : O_Enode) return Int32;

   --  For OE_Loop, set loop level (an integer).
   --  Reserved for back-end in HLI mode only.
   function Get_Loop_Level (Stmt : O_Enode) return Int32;
   procedure Set_Loop_Level (Stmt : O_Enode; Level : Int32);

   --  Start a subprogram body.
   --  Note: the declaration may have an external storage, in this case it
   --  becomes public.
   procedure Start_Subprogram_Body (Func : O_Dnode);

   --  Finish a subprogram body.
   procedure Finish_Subprogram_Body;

   --  Translate a scalar literal into an expression.
   function New_Lit (Lit : O_Cnode) return O_Enode;

   --  Translate an object (var, const or interface) into an lvalue.
   function New_Obj (Obj : O_Dnode) return O_Lnode;

   --  Create a dyadic operation.
   --  Left and right nodes must have the same type.
   --  Binary operation is allowed only on boolean types.
   --  The result is of the type of the operands.
   function New_Dyadic_Op (Kind : ON_Dyadic_Op_Kind; Left, Right : O_Enode)
     return O_Enode;

   --  Create a monadic operation.
   --  Result is of the type of operand.
   function New_Monadic_Op (Kind : ON_Monadic_Op_Kind; Operand : O_Enode)
     return O_Enode;

   --  Create a comparaison operator.
   --  NTYPE is the type of the result and must be a boolean type.
   function New_Compare_Op
     (Kind : ON_Compare_Op_Kind; Left, Right : O_Enode; Ntype : O_Tnode)
     return O_Enode;

      --  Returns the size in bytes of ATYPE.  The result is a literal of
   --  unsigned type RTYPE
   --  ATYPE cannot be an unconstrained array type.
   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Enode;

   --  Returns the offset of FIELD in its record.  The result is a literal
   --  of unsigned type RTYPE.
   function New_Offsetof (Field : O_Fnode; Rtype : O_Tnode) return O_Enode;

   --  Get an element of an array.
   --  INDEX must be of the type of the array index.
   function New_Indexed_Element (Arr : O_Lnode; Index : O_Enode)
     return O_Lnode;

   --  Get a slice of an array; this is equivalent to a conversion between
   --  an array or an array subtype and an array subtype.
   --  RES_TYPE must be an array_sub_type whose base type is the same as the
   --  base type of ARR.
   --  INDEX must be of the type of the array index.
   function New_Slice (Arr : O_Lnode; Res_Type : O_Tnode; Index : O_Enode)
     return O_Lnode;

   --  Get an element of a record.
   --  Type of REC must be a record type.
   function New_Selected_Element (Rec : O_Lnode; El : O_Fnode)
     return O_Lnode;

   --  Reference an access.
   --  Type of ACC must be an access type.
   function New_Access_Element (Acc : O_Enode) return O_Lnode;

   --  Do a conversion.
   --  Allowed conversions are:
   --  FIXME: to write.
   function New_Convert_Ov (Val : O_Enode; Rtype : O_Tnode) return O_Enode;
   function New_Convert (Val : O_Enode; Rtype : O_Tnode) return O_Enode;

   --  Get the address of LVALUE.
   --  ATYPE must be a type access whose designated type is the type of LVALUE.
   --  FIXME: what about arrays.
   function New_Address (Lvalue : O_Lnode; Atype : O_Tnode) return O_Enode;

   --  Same as New_Address but without any restriction.
   function New_Unchecked_Address (Lvalue : O_Lnode; Atype : O_Tnode)
     return O_Enode;

   --  Get the address of a subprogram.
   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
     return O_Enode;

   --  Get the value of an Lvalue.
   function New_Value (Lvalue : O_Lnode) return O_Enode;

   --  Return a pointer of type RTPE to SIZE bytes allocated on the stack.
   function New_Alloca (Rtype : O_Tnode; Size : O_Enode) return O_Enode;

   type O_Assoc_List is limited private;

   --  Create a function call or a procedure call.
   procedure Start_Association (Assocs : out O_Assoc_List; Subprg : O_Dnode);
   procedure New_Association (Assocs : in out O_Assoc_List; Val : O_Enode);
   function New_Function_Call (Assocs : O_Assoc_List) return O_Enode;
   procedure New_Procedure_Call (Assocs : in out O_Assoc_List);

   --  Assign VALUE to TARGET, type must be the same or compatible.
   --  FIXME: what about slice assignment?
   procedure New_Assign_Stmt (Target : O_Lnode; Value : O_Enode);

   --  Exit from the subprogram and return VALUE.
   procedure New_Return_Stmt (Value : O_Enode);
   --  Exit from the subprogram, which doesn't return value.
   procedure New_Return_Stmt;

   type O_If_Block is limited private;

   --  Build an IF statement.
   procedure Start_If_Stmt (Block : out O_If_Block; Cond : O_Enode);
   procedure New_Else_Stmt (Block : in out O_If_Block);
   procedure Finish_If_Stmt (Block : in out O_If_Block);

   type O_Snode is private;
   O_Snode_Null : constant O_Snode;

   --  Create a infinite loop statement.
   procedure Start_Loop_Stmt (Label : out O_Snode);
   procedure Finish_Loop_Stmt (Label : in out O_Snode);

   --  Exit from a loop stmt or from a for stmt.
   procedure New_Exit_Stmt (L : O_Snode);
   --  Go to the start of a loop stmt or of a for stmt.
   --  Loops/Fors between L and the current points are exited.
   procedure New_Next_Stmt (L : O_Snode);

   --  Case statement.
   --  VALUE is the selector and must be a discrete type.
   type O_Case_Block is limited private;
   procedure Start_Case_Stmt (Block : out O_Case_Block; Value : O_Enode);
   procedure Start_Choice (Block : in out O_Case_Block);
   procedure New_Expr_Choice (Block : in out O_Case_Block; Expr : O_Cnode);
   procedure New_Range_Choice (Block : in out O_Case_Block;
                               Low, High : O_Cnode);
   procedure New_Default_Choice (Block : in out O_Case_Block);
   procedure Finish_Choice (Block : in out O_Case_Block);
   procedure Finish_Case_Stmt (Block : in out O_Case_Block);

   procedure Start_Declare_Stmt;
   procedure Finish_Declare_Stmt;

   procedure New_Debug_Line_Stmt (Line : Natural);

   procedure Disp_Subprg_Body (Indent : Natural; Subprg : O_Enode);
   procedure Disp_All_Enode;
   procedure Disp_Stats;

   type Mark_Type is limited private;
   procedure Mark (M : out Mark_Type);
   procedure Release (M : Mark_Type);

   procedure Finish;
private
   type O_Assoc_List is record
      --  Subprogram being called.
      Subprg : O_Dnode;
      --  First and last argument statement.
      First_Arg : O_Enode;
      Last_Arg : O_Enode;
      --  Interface for the next association.
      Next_Inter : O_Dnode;
   end record;

   type O_Case_Block is record
      --  Expression for the selection.
      Expr : O_Enode;

      --  Type of expression.
      --  Used to perform checks.
      Expr_Type : O_Tnode;

      --  Choice code and branch code is not mixed (anymore).
      --  Therefore, code to perform choices is inserted.
      --  Last node of the choice code.
      Last_Node : O_Enode;

      --  Label at the end of the case statement.
      --  used to jump from the end of a branch to the end of the statement.
      Label_End : O_Enode;

      --  Label of the branch code.
      Label_Branch : O_Enode;
   end record;

   type O_If_Block is record
      Label_End : O_Enode;
      Label_Next : O_Enode;
   end record;

   type O_Snode is record
      Label_Start : O_Enode;
      Label_End : O_Enode;
   end record;
   O_Snode_Null : constant O_Snode := (Label_Start => O_Enode_Null,
                                       Label_End => O_Enode_Null);

   type Mark_Type is record
      Enode : O_Enode;
   end record;
end Ortho_Code.Exprs;
