--  Compute verilog expressions (with the verilog semantic).
--  Copyright (C) 2023 Tristan Gingold
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
with Types; use Types;
with Verilog.Types; use Verilog.Types;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Storages; use Verilog.Storages;

package Verilog.Bignums is
   type Digit_Index is new Natural;

   Digit_Width : constant := 32;
   subtype Bitvec_Digit is Uns32;

   type Bitvec_Array is array (Digit_Index range <>) of Bitvec_Digit;
   subtype Big_Bitvec_Array is Bitvec_Array (Digit_Index);
   type Bitvec_Ptr is access all Big_Bitvec_Array;

   type Logvec_Array is array (Digit_Index range <>) of Logic_32;
   subtype Big_Logvec_Array is Logvec_Array (Digit_Index);
   type Logvec_Ptr is access all Big_Logvec_Array;

   All_1 : constant Uns32 := not 0;

   Logic_32_Z : constant Logic_32 := (Val => 0, Zx => All_1);
   Logic_32_X : constant Logic_32 := (Val => All_1, Zx => All_1);

   function To_Last (W : Width_Type) return Digit_Index;

   type Bit_Type is (B_0, B_1);

   type Bit_Ptr is access all Bit_Type;

   --  Verilog 4-state logic type.
   type Logic_Type is (V_0, V_1, V_Z, V_X);

   type Logic_Ptr is access all Logic_Type;

   Log_To_Char : constant array (Logic_Type) of Character := "01zx";
   Bit_To_Char : constant array (Bit_Type) of Character := "01";

   function To_Logvec_Ptr is
      new Ada.Unchecked_Conversion (Data_Ptr, Logvec_Ptr);
   function To_Logic_Ptr is
      new Ada.Unchecked_Conversion (Data_Ptr, Logic_Ptr);
   function To_Bitvec_Ptr is
      new Ada.Unchecked_Conversion (Data_Ptr, Bitvec_Ptr);
   function To_Bit_Ptr is
      new Ada.Unchecked_Conversion (Data_Ptr, Bit_Ptr);

   function To_Uns32 is
      new Ada.Unchecked_Conversion (Int32, Uns32);
   function To_Int32 is
      new Ada.Unchecked_Conversion (Uns32, Int32);

   --  Debug routine: display LV.
   procedure Dlv (Lv : Logvec_Ptr; Width : Width_Type);

   --  Set dest to all-x.
   procedure Set_X (Res : Logvec_Ptr; Width : Width_Type);

   --  Set dest to all-0.
   procedure Set_0 (Res : Logvec_Ptr; Width : Width_Type);
   procedure Set_0 (Res : Bitvec_Ptr; Width : Width_Type);

   --  Return true iff BV has X or Z.
   function Has_Unknowns (Lv : Logvec_Ptr; Width : Width_Type) return Boolean;

   --  Utility: extract the HI/LO word of an Uns64 number.
   function Uns64_Hi (V : Uns64) return Uns32;
   function Uns64_Lo (V : Uns64) return Uns32;
   pragma Inline (Uns64_Hi);
   pragma Inline (Uns64_Lo);

   --  Return True iff LV fits in an Uns32 value.  False if LV has unknowns.
   function In_Uns32 (Lv : Logvec_Ptr; Width : Width_Type) return Boolean;
   function In_Uns32 (Bv : Bitvec_Ptr; Width : Width_Type) return Boolean;

   --  Convert Lv to an uns32.  Only valid if In_Uns32 is True.
   function To_Uns32 (Lv : Logvec_Ptr; Width : Width_Type) return Uns32;
   function To_Uns32 (Bv : Bitvec_Ptr; Width : Width_Type) return Uns32;

   --  Return True iff LV fits in an Int32 value.  False if LV has unknowns.
   function In_Int32 (Lv : Logvec_Ptr; Width : Width_Type) return Boolean;

   --  Convert Lv to an Int32.  Only valid if In_Int32 is True.
   function To_Int32 (Lv : Logvec_Ptr; Width : Width_Type) return Int32;

   --  Expand a logic scalar to a 1-logic vector.
   function To_Logic_32 (V : Logic_Type) return Logic_32;

   --  Assign SRC to DEST.
   procedure Assign (Dest : Logvec_Ptr; Src : Logvec_Ptr; Width : Width_Type);
   procedure Assign (Dest : Bitvec_Ptr; Src : Bitvec_Ptr; Width : Width_Type);

   procedure Compute_Conversion
     (Dest : Data_Ptr; Expr : Node; Src : Data_Ptr);

   --  Convert an N_Number node to DEST.
   procedure Compute_Number (Dest : Logvec_Ptr; Num : Node);
   procedure Compute_Number (Dest : Bitvec_Ptr; Num : Node);

   --  Likewise for N_Unbased_Literal.
   procedure Compute_Unbased_Literal (Dest : Logvec_Ptr; Num : Node);
   procedure Compute_Unbased_Literal (Dest : Bitvec_Ptr; Num : Node);
   procedure Compute_Unbased_Literal (Dest : Logic_Ptr; Num : Node);

   --  Convert an N_Bignum node to DEST.
   procedure Compute_Bignum (Dest : Logvec_Ptr; Num : Node);

   --  Evaluate VAL as a conditional predicate.
   function Compute_Predicate (Val : Logvec_Ptr; Width : Width_Type)
                              return Tri_State_Type;
   function Compute_Predicate (Val : Bitvec_Ptr; Width : Width_Type)
                              return Tri_State_Type;

   --  Compute R ? T : F when R is x.
   procedure Compute_Conditional_Mixed_Lv (Dest : Logvec_Ptr;
                                           T : Logvec_Ptr;
                                           F : Logvec_Ptr;
                                           Width : Width_Type);
   procedure Compute_Conditional_Mixed_Log (Dest : Logic_Ptr;
                                            T : Logic_Type;
                                            F : Logic_Type);

   --  Truncate VAL to SIZE bits.
   procedure Compute_Trunc (Dest : Logvec_Ptr;
                            Dwidth : Width_Type;
                            Val : Logvec_Ptr;
                            Width : Width_Type);
   procedure Compute_Lv_Bv_Trunc (Dest : Bitvec_Ptr;
                                  Dwidth : Width_Type;
                                  Val : Logvec_Ptr;
                                  Width : Width_Type);
   procedure Compute_Lv_Bv (Dest : Bitvec_Ptr;
                            Val : Logvec_Ptr;
                            Width : Width_Type);

   procedure Compute_Bv_Lv (Dest : Logvec_Ptr;
                            Val : Bitvec_Ptr;
                            Width : Width_Type);

   procedure Compute_Bv_Lv_Trunc (Dest : Logvec_Ptr;
                                  Dwidth : Width_Type;
                                  Val : Bitvec_Ptr;
                                  Width : Width_Type);

   procedure Compute_Bv_Lv_Sext (Dest : Logvec_Ptr;
                                 Dwidth : Width_Type;
                                 Val : Bitvec_Ptr;
                                 Width : Width_Type);

   procedure Compute_Bv_Lv_Zext (Dest : Logvec_Ptr;
                                 Dwidth : Width_Type;
                                 Val : Bitvec_Ptr;
                                 Width : Width_Type);

   --  Extend VAL to SIZE bits.
   procedure Compute_Zext (Dest : Logvec_Ptr;
                           Dwidth : Width_Type;
                           Val : Logvec_Ptr;
                           Width : Width_Type);
   procedure Compute_Sext (Dest : Logvec_Ptr;
                           Dwidth : Width_Type;
                           Val : Logvec_Ptr;
                           Width : Width_Type);

   procedure Compute_And
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Or
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Or
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type);
   procedure Compute_Xor
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Xnor
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);

   procedure Compute_Add
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Add
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type);
   procedure Compute_Sub
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Sub
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type);

   procedure Compute_Umul
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Smul
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);

   procedure Compute_Sdiv
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Udiv
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Udiv
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type);
   procedure Compute_Smod
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Smod
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type);
   procedure Compute_Umod
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type);

   procedure Compute_Inc
     (Res : Logvec_Ptr; Expr : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Inc
     (Res : Bitvec_Ptr; Expr : Bitvec_Ptr; Width : Width_Type);

   procedure Compute_Shr (Res : Logvec_Ptr;
                          Left : Logvec_Ptr;
                          Width : Width_Type;
                          Right : Logvec_Ptr;
                          Right_Width : Width_Type);
   procedure Compute_Shl (Res : Logvec_Ptr;
                          Left : Logvec_Ptr;
                          Width : Width_Type;
                          Right : Logvec_Ptr;
                          Right_Width : Width_Type);
   procedure Compute_Shl (Res : Bitvec_Ptr;
                          Left : Bitvec_Ptr;
                          Width : Width_Type;
                          Right : Bitvec_Ptr;
                          Right_Width : Width_Type);
   --  Arithmetic right shift for signed operands.
   procedure Compute_Asr (Res : Logvec_Ptr;
                          Left : Logvec_Ptr;
                          Width : Width_Type;
                          Right : Logvec_Ptr;
                          Right_Width : Width_Type);

   procedure Compute_Not
     (Res : Logvec_Ptr; Val : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Neg
     (Res : Logvec_Ptr; Val : Logvec_Ptr; Width : Width_Type);
   procedure Compute_Neg
     (Res : Bitvec_Ptr; Val : Bitvec_Ptr; Width : Width_Type);
   function Compute_Log_Neg (Val : Logvec_Ptr; Width : Width_Type)
                            return Logic_Type;
   function Compute_Log_Red_Or (Val : Logvec_Ptr; Width : Width_Type)
                               return Logic_Type;
   function Compute_Log_Red_Nor (Val : Logvec_Ptr; Width : Width_Type)
                                return Logic_Type;
   function Compute_Log_Red_And (Val : Logvec_Ptr; Width : Width_Type)
                                return Logic_Type;
   function Compute_Log_Red_Nand (Val : Logvec_Ptr; Width : Width_Type)
                                return Logic_Type;
   function Compute_Log_Red_Xor (Val : Logvec_Ptr; Width : Width_Type)
                                return Logic_Type;
   function Compute_Log_Red_Xnor (Val : Logvec_Ptr; Width : Width_Type)
                                 return Logic_Type;

   --  Convert for condition.
   function Compute_To_Logic (Val : Logvec_Ptr; Width : Width_Type)
                             return Logic_Type;

   --  Extract SRC_WD bits from SRC at SRC_OFF, and put them into DEST at
   --  DEST_OFF.  Destination bits between 0 and DEST_WD -1 not written
   --  should be set to X.
   procedure Compute_Part_Extract (Dest : Logvec_Ptr;
                                   Dest_Off : Bit_Offset;
                                   Dest_Wd : Width_Type;
                                   Src : Logvec_Ptr;
                                   Src_Off : Bit_Offset;
                                   Src_Wd : Width_Type);
   procedure Compute_Bit_Part_Extract (Dest : Bitvec_Ptr;
                                       Dest_Off : Bit_Offset;
                                       Dest_Wd : Width_Type;
                                       Src : Bitvec_Ptr;
                                       Src_Off : Bit_Offset;
                                       Src_Wd : Width_Type);

   --  Extract part of VAL to RES.
   procedure Compute_Part_Select (Res : Logvec_Ptr;
                                  Val : Logvec_Ptr;
                                  Off : Bit_Offset;
                                  Width : Width_Type);
   function Compute_Bit_Select
     (Val : Logvec_Ptr; Off : Bit_Offset) return Logic_Type;
   function Compute_Bit_Select
     (Val : Bitvec_Ptr; Off : Bit_Offset) return Bit_Type;

   --  Insert WIDTH bits from SRC starting from SRC_OFF) into DST at
   --  offset DST_OFF.
   --  It assumes DST is large enough.
   procedure Compute_Part_Insert (Dst : Logvec_Ptr;
                                  Dst_Off : Bit_Offset;
                                  Src : Logvec_Ptr;
                                  Src_Off : Bit_Offset;
                                  Width : Width_Type;
                                  Change : out Boolean);

   --  Likewise, but mixed log/bit (for packed aggregate)
   --  No offset for SRC (always 0).
   procedure Compute_Log_Bit_Part_Insert (Dst : Logvec_Ptr;
                                          Dst_Off : Bit_Offset;
                                          Src : Bitvec_Ptr;
                                          Width : Width_Type);

   --  Insert VAL in RES at offset OFF.
   procedure Compute_Log_Insert (Res : Logvec_Ptr;
                                 Off : Bit_Offset;
                                 Val : Logic_Type;
                                 Change : out Boolean);
   procedure Compute_Bit_Insert (Res : Bitvec_Ptr;
                                 Off : Bit_Offset;
                                 Val : Bit_Type;
                                 Change : out Boolean);

   --  Unsigned division by a single digit.  The result is put to VAL.
   procedure Compute_Div_Clean (Val : Logvec_Ptr;
                                Width : Width_Type;
                                Div : Uns32;
                                Remain : out Uns32);

   procedure Compute_Mul_Add_Clean (Val : Logvec_Ptr;
                                    Width : Width_Type;
                                    Mul : Uns32;
                                    Carry : in out Uns32);

   --  Return True iff VAL is 0.
   function Is_Zero_Clean (Val : Logvec_Ptr; Width : Width_Type)
                          return Boolean;

   --  Return True if LEFT === RIGHT (exactly the same bits, no wildcard).
   function Is_Eq (Left, Right : Logvec_Ptr; Width : Width_Type)
                  return Boolean;
   function Is_Eq (Left, Right : Bitvec_Ptr; Width : Width_Type)
                  return Boolean;

   --  Return true iff LEFT is equal to right, any 'x' on any side is
   --  considered as a don't care.
   function Is_Eqx (Left, Right : Logvec_Ptr; Width : Width_Type)
                   return Boolean;

   --  Return true iff LEFT is equal to right, any 'x' or 'z' on any side is
   --  considered as a don't care.
   function Is_Eqz (Left, Right : Logvec_Ptr; Width : Width_Type)
                   return Boolean;

   --  If EQ is true, return LEFT === RIGHT.
   --  If EQ is false, return LEFT !=== RIGHT.
   --  The comparison is done on WIDTH bits.
   function Compute_Case
     (Left, Right : Logvec_Ptr; Width : Width_Type; Eq : Boolean)
     return Logic_Type;

   --  Return 1'hx if LEFT or RIGHT has an x or a z.
   --  If EQ is true, return LEFT == RIGHT.
   --  If EQ is false, return LEFT != RIGHT.
   function Compute_Log_Eq
     (Left, Right : Logvec_Ptr; Width : Width_Type; Eq : Boolean)
     return Logic_Type;

   --  Return LEFT < RIGHT.
   function Compute_Ult (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type;
   function Compute_Slt (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type;

   --  Return LEFT <= RIGHT.
   function Compute_Ule (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type;
   function Compute_Sle (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type;

   procedure Compute_Int32
     (Dest : Logvec_Ptr; Width : Width_Type; Val : Int32);

   type Logic_Table is array (Logic_Type, Logic_Type) of Logic_Type;
   pragma Pack (Logic_Table);

   --  Return the binary digit (0, 1, x or z) at position P in V.
   function Get_Bin_Digit (V : Logic_32; P : Natural) return Character;

   function Get_Hex_Digit (V : Logic_32; P : Natural) return Character;

   --  1800-2017 11.4.8 Bitwise operators
   --  Table 11.11 Bitwise binary AND operator
   Logic_And : constant Logic_Table :=
     (V_0 => (others => V_0),
      V_1 => (V_1 => V_1, V_0 => V_0, others => V_X),
      V_X | V_Z => (V_0 => V_0, others => V_X));

   --  1800-2017 11.4.8 Bitwise operators
   --  Table 11.12 Bitwise binary OR operator
   Logic_Or : constant Logic_Table :=
     (V_0 => (V_0 => V_0, V_1 => V_1, others => V_X),
      V_1 => (others => V_1),
      V_X | V_Z => (V_1 => V_1, others => V_X));

   --  1800-2017 11.4.8 Bitwise operators
   --  Table 11.13 Bitwise binary exclusive OR operator
   Logic_Xor : constant Logic_Table :=
     (V_0 => (V_0 => V_0, V_1 => V_1, others => V_X),
      V_1 => (V_0 => V_1, V_1 => V_0, others => V_X),
      others => (others => V_X));

end Verilog.Bignums;
