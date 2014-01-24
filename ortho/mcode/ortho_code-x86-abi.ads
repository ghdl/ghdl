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
with Ortho_Code.Types; use Ortho_Code.Types;

package Ortho_Code.X86.Abi is
   type O_Abi_Subprg is private;

   procedure Init;
   procedure Finish;

   Mode_Align : Mode_Align_Array :=
     (Mode_U8 | Mode_I8 => 0,
      Mode_U16 | Mode_I16 => 1,
      Mode_U32 | Mode_I32 | Mode_F32 | Mode_P32 => 2,
      Mode_U64 | Mode_I64 => 2,
      Mode_F64 => 2, -- 2 for SVR4-ABI and Darwin, 3 for Windows.
      Mode_Blk | Mode_X1 | Mode_Nil | Mode_P64 => 0,
      Mode_B2 => 0);

   Mode_Ptr : constant Mode_Type := Mode_P32;

   Flag_Type_Completer : constant Boolean := False;
   Flag_Lower_Stmt : constant Boolean := True;

   Flag_Sse2 : Boolean := False;

   --  Procedures to layout a subprogram declaration.
   procedure Start_Subprogram (Subprg : O_Dnode; Abi : out O_Abi_Subprg);
   procedure New_Interface (Inter : O_Dnode; Abi : in out O_Abi_Subprg);
   procedure Finish_Subprogram (Subprg : O_Dnode; Abi : in out O_Abi_Subprg);

   --  Only called for top-level subprograms.
   procedure Start_Body (Subprg : O_Dnode);
   --  Finish compilation of a body.
   procedure Finish_Body (Subprg : Subprogram_Data_Acc);

   procedure Expand_Const_Decl (Decl : O_Dnode);
   procedure Expand_Var_Decl (Decl : O_Dnode);
   procedure Expand_Const_Value (Decl : O_Dnode; Val : O_Cnode);

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
private
   type O_Abi_Subprg is record
      --  For x86: offset of the next argument.
      Offset : Int32 := 0;
   end record;
end Ortho_Code.X86.Abi;
