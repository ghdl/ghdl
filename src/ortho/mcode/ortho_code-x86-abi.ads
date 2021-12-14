--  X86 ABI definitions.
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
with Ortho_Code.Types; use Ortho_Code.Types;
with Ortho_Code.X86.Flags;

package Ortho_Code.X86.Abi is
   type O_Abi_Subprg is private;

   procedure Init;
   procedure Finish;

   Mode_Align : Mode_Align_Array :=
     (Mode_U8 | Mode_I8 => 0,
      Mode_U16 | Mode_I16 => 1,
      Mode_U32 | Mode_I32 | Mode_F32 | Mode_P32 => 2,
      Mode_U64 | Mode_I64 => 2 + Boolean'Pos (Flags.M64),
      Mode_F64 => 2, -- 2 for SVR4-ABI and Darwin, 3 for Windows.
      Mode_P64 => 3,
      Mode_Blk | Mode_X1 | Mode_Nil => 0,
      Mode_B2 => 0);

   --  A long and complex expression for: flags.M64 ? Mode_P64 : Mode_P32.
   Mode_Ptr : constant Mode_Type := Mode_Type'Val
     (Boolean'Pos (Flags.M64) * Mode_Type'Pos (Mode_P64)
        + Boolean'Pos (not Flags.M64) * Mode_Type'Pos (Mode_P32));

   Flag_Type_Completer : constant Boolean := False;
   Flag_Lower_Stmt : constant Boolean := True;

   --  If True, use SSE/SSE2 instructions instead of FPU one.  The code is
   --  still compliant with the ABI (ie FP values are returned in st0).
   Flag_Sse2 : constant Boolean := True;

   --  Procedures to layout a subprogram declaration.
   procedure Start_Subprogram (Subprg : O_Dnode; Abi : out O_Abi_Subprg);
   procedure New_Interface (Inter : O_Dnode; Abi : in out O_Abi_Subprg);
   procedure Finish_Subprogram (Subprg : O_Dnode; Abi : in out O_Abi_Subprg);

   --  Only called for top-level subprograms.
   procedure Start_Body (Subprg : O_Dnode);
   --  Finish compilation of a body (body is Cur_Subprg).
   procedure Finish_Body;

   procedure Expand_Const_Decl (Decl : O_Dnode);
   procedure Expand_Var_Decl (Decl : O_Dnode);

   --  Create a variable with a nul default value.
   procedure Expand_Var_Zero (Decl : O_Dnode);

   --  Set the initial value of a constant or a variable.
   procedure Expand_Init_Value (Decl : O_Dnode; Val : O_Cnode);

   procedure New_Debug_Filename_Decl (Filename : String);

   Last_Link : O_Enode;
   procedure Link_Stmt (Stmt : O_Enode);

   --  Disp SUBPRG (subprg declaration) as a declaration (name and interfaces).
   procedure Disp_Subprg_Decl (Decl : O_Dnode);

   procedure Disp_Stmt (Stmt : O_Enode);

   --function Image_Insn (Insn : O_Insn) return String;
   function Image_Reg (Reg : O_Reg) return String;

   --  Link in memory intrinsics symbols.
   procedure Link_Intrinsics;

   --  Register unwinding info for JIT.
   procedure Register_Unwind;
   procedure Unregister_Unwind;

   --  Target specific data for subprograms.
   type Target_Subprg is record
      Fp_Slot : Uns32 := 0;
   end record;
private
   --  Target specific data for O_Inter_List.
   type O_Abi_Subprg is record
      --  For x86: offset of the next argument in the stack.
      Offset : Int32 := 0;
      --  For x86-64: register num.
      Inum : Natural := 0;
      Fnum : Natural := 0;
   end record;
end Ortho_Code.X86.Abi;
