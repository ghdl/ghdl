--  Mcode back-end for ortho - common definitions.
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
with Ada.Unchecked_Conversion;

package Ortho_Code is
   type Int32 is range -(2 ** 31) .. (2 ** 31) - 1;

   type Uns32 is mod 2 ** 32;

   type Uns64 is mod 2 ** 64;

   function Shift_Right (L : Uns64; R : Natural) return Uns64;
   function Shift_Right (L : Uns32; R : Natural) return Uns32;
   pragma Import (Intrinsic, Shift_Right);

   function Shift_Right_Arithmetic (L : Uns32; R : Natural) return Uns32;
   pragma Import (Intrinsic, Shift_Right_Arithmetic);

   function Shift_Left (L : Uns32; R : Natural) return Uns32;
   pragma Import (Intrinsic, Shift_Left);

   type O_Tnode is new Int32;
   for O_Tnode'Size use 32;
   O_Tnode_Null : constant O_Tnode := 0;
   O_Tnode_First : constant O_Tnode := 2;

   --  A generic pointer.
   --  This is used by static chains.
   O_Tnode_Ptr : constant O_Tnode := 2;

   type O_Cnode is new Int32;
   for O_Cnode'Size use 32;
   O_Cnode_Null : constant O_Cnode := 0;

   type O_Dnode is new Int32;
   for O_Dnode'Size use 32;
   O_Dnode_Null : constant O_Dnode := 0;
   O_Dnode_First : constant O_Dnode := 2;

   type O_Enode is new Int32;
   for O_Enode'Size use 32;
   O_Enode_Null : constant O_Enode := 0;
   O_Enode_Err : constant O_Enode := 1;

   type O_Fnode is new Int32;
   for O_Fnode'Size use 32;
   O_Fnode_Null : constant O_Fnode := 0;

   type O_Lnode is new Int32;
   for O_Lnode'Size use 32;
   O_Lnode_Null : constant O_Lnode := 0;

   type O_Gnode is new Int32;
   for O_Gnode'Size use 32;
   O_Gnode_Null : constant O_Gnode := 0;

   type O_Ident is new Int32;
   O_Ident_Nul : constant O_Ident := 0;

   function To_Int32 is new Ada.Unchecked_Conversion
     (Source => Uns32, Target => Int32);

   function To_Uns32 is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Uns32);


   --  Specifies the storage kind of a declaration.
   --  O_STORAGE_EXTERNAL:
   --    The declaration do not either reserve memory nor generate code, and
   --    is imported either from an other file or from a later place in the
   --    current file.
   --  O_STORAGE_PUBLIC, O_STORAGE_PRIVATE:
   --    The declaration reserves memory or generates code.
   --    With O_STORAGE_PUBLIC, the declaration is exported outside of the
   --    file while with O_STORAGE_PRIVATE, the declaration is local to the
   --    file.
   type O_Storage is (O_Storage_External,
                      O_Storage_Public,
                      O_Storage_Private,
                      O_Storage_Local);

   --  Depth of a declaration.
   --    0 for top-level,
   --    1 for declared in a top-level subprogram
   type O_Depth is range 0 .. (2 ** 16) - 1;
   O_Toplevel : constant O_Depth := 0;

   --  BE representation of a register.
   type O_Reg is mod 256;
   R_Nil : constant O_Reg := 0;

   type Mode_Type is (Mode_U8, Mode_U16, Mode_U32, Mode_U64,
                      Mode_I8, Mode_I16, Mode_I32, Mode_I64,
                      Mode_X1, Mode_Nil, Mode_F32, Mode_F64,
                      Mode_B2, Mode_Blk, Mode_P32, Mode_P64);

   subtype Mode_Uns is Mode_Type range Mode_U8 .. Mode_U64;
   subtype Mode_Int is Mode_Type range Mode_I8 .. Mode_I64;
   subtype Mode_Fp is Mode_Type range Mode_F32 .. Mode_F64;
   -- Mode_Ptr : constant Mode_Type := Mode_P32;

   type ON_Op_Kind is
     (
      --  Not an operation; invalid.
      ON_Nil,

      --  Dyadic operations.
      ON_Add_Ov,                --  ON_Dyadic_Op_Kind
      ON_Sub_Ov,                --  ON_Dyadic_Op_Kind
      ON_Mul_Ov,                --  ON_Dyadic_Op_Kind
      ON_Div_Ov,                --  ON_Dyadic_Op_Kind
      ON_Rem_Ov,                --  ON_Dyadic_Op_Kind
      ON_Mod_Ov,                --  ON_Dyadic_Op_Kind

      --  Binary operations.
      ON_And,                   --  ON_Dyadic_Op_Kind
      ON_Or,                    --  ON_Dyadic_Op_Kind
      ON_Xor,                   --  ON_Dyadic_Op_Kind

      --  Monadic operations.
      ON_Not,                   --  ON_Monadic_Op_Kind
      ON_Neg_Ov,                --  ON_Monadic_Op_Kind
      ON_Abs_Ov,                --  ON_Monadic_Op_Kind

      --  Comparaisons
      ON_Eq,                    --  ON_Compare_Op_Kind
      ON_Neq,                   --  ON_Compare_Op_Kind
      ON_Le,                    --  ON_Compare_Op_Kind
      ON_Lt,                    --  ON_Compare_Op_Kind
      ON_Ge,                    --  ON_Compare_Op_Kind
      ON_Gt                     --  ON_Compare_Op_Kind
      );

   subtype ON_Dyadic_Op_Kind is ON_Op_Kind range ON_Add_Ov .. ON_Xor;
   subtype ON_Monadic_Op_Kind is ON_Op_Kind range ON_Not .. ON_Abs_Ov;
   subtype ON_Compare_Op_Kind is ON_Op_Kind range ON_Eq .. ON_Gt;

   Syntax_Error : exception;
end Ortho_Code;
