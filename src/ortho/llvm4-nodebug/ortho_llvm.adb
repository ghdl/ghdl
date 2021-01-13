--  LLVM back-end for ortho.
--  Copyright (C) 2014 Tristan Gingold
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
with Ada.Unchecked_Deallocation;
with LLVM.Target; use LLVM.Target;

package body Ortho_LLVM is
   --  The current function for LLVM (needed to add new basic blocks).
   Cur_Func : ValueRef;

   --  The current function node (needed for return type).
   Cur_Func_Decl : O_Dnode;

   --  Whether the code is currently unreachable.  LLVM doesn't accept basic
   --  blocks that cannot be reached (using trivial rules).  So we need to
   --  discard instructions after a return, a next or an exit statement.
   Unreach : Boolean;

   --  Builder for statements.
   Builder : BuilderRef;

   --  Builder for declarations (local variables).
   Decl_Builder : BuilderRef;

   --  Temporary builder.
   Extra_Builder : BuilderRef;

   --  Additional data for declare blocks.
   type Declare_Block_Type;
   type Declare_Block_Acc is access Declare_Block_Type;

   type Declare_Block_Type is record
      --  First basic block of the declare.
      Stmt_Bb : BasicBlockRef;

      --  Stack pointer at entry of the block.  This value has to be restore
      --  when leaving the block (either normally or via exit/next).  Set only
      --  if New_Alloca was used.
      --  FIXME: TODO: restore stack pointer on exit/next stmts.
      Stack_Value : ValueRef;

      --  Debug data for the scope of the declare block.
      Dbg_Scope : ValueRef;

      --  Previous element in the stack.
      Prev : Declare_Block_Acc;
   end record;

   --  Current declare block.
   Cur_Declare_Block : Declare_Block_Acc;

   --  Chain of unused blocks to be recycled.
   Old_Declare_Block : Declare_Block_Acc;

   Stacksave_Fun : ValueRef;
   Stacksave_Name : constant String := "llvm.stacksave" & ASCII.NUL;
   Stackrestore_Fun : ValueRef;
   Stackrestore_Name : constant String := "llvm.stackrestore" & ASCII.NUL;
   Copysign_Fun : ValueRef;
   Copysign_Name : constant String := "llvm.copysign.f64" & ASCII.NUL;
   Fp_0_5 : ValueRef;

   Nounwind_Attr : AttributeRef;
   Uwtable_Attr : AttributeRef;

   procedure Free is new Ada.Unchecked_Deallocation
     (ValueRefArray, ValueRefArray_Acc);

   procedure Create_Declare_Block
   is
      Res : Declare_Block_Acc;
   begin
      --  Try to recycle an unused record.
      if Old_Declare_Block /= null then
         Res := Old_Declare_Block;
         Old_Declare_Block := Res.Prev;
      else
         --  Create a new one if no unused records.
         Res := new Declare_Block_Type;
      end if;

      --  Chain.
      Res.all := (Stmt_Bb => Null_BasicBlockRef,
                  Stack_Value => Null_ValueRef,
                  Dbg_Scope => Null_ValueRef,
                  Prev => Cur_Declare_Block);
      Cur_Declare_Block := Res;

      if not Unreach then
         Res.Stmt_Bb := AppendBasicBlock (Cur_Func, Empty_Cstring);
      end if;
   end Create_Declare_Block;

   procedure Destroy_Declare_Block
   is
      Blk : constant Declare_Block_Acc := Cur_Declare_Block;
   begin
      --  Unchain.
      Cur_Declare_Block := Blk.Prev;

      --  Put on the recyle list.
      Blk.Prev := Old_Declare_Block;
      Old_Declare_Block := Blk;
   end Destroy_Declare_Block;

   -----------------------
   -- Start_Record_Type --
   -----------------------

   procedure Start_Record_Type (Elements : out O_Element_List) is
   begin
      Elements := (Kind => OF_Record,
                   Nbr_Elements => 0,
                   Rec_Type => O_Tnode_Null,
                   Size => 0,
                   Align => 0,
                   Align_Type => Null_TypeRef,
                   First_Elem => null,
                   Last_Elem => null);
   end Start_Record_Type;

   ----------------------
   -- New_Record_Field --
   ----------------------

   procedure Add_Field
     (Elements : in out O_Element_List; Ident : O_Ident; Etype : O_Tnode)
   is
      O_El : O_Element_Acc;
   begin
      Elements.Nbr_Elements := Elements.Nbr_Elements + 1;
      O_El := new O_Element'(Next => null,
                             Etype => Etype,
                             Ident => Ident);
      if Elements.First_Elem = null then
         Elements.First_Elem := O_El;
      else
         Elements.Last_Elem.Next := O_El;
      end if;
      Elements.Last_Elem := O_El;
   end Add_Field;

   procedure New_Record_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident;
      Etype : O_Tnode) is
   begin
      El := (Kind => OF_Record,
             Index => Elements.Nbr_Elements,
             Ftype => Etype);
      Add_Field (Elements, Ident, Etype);
   end New_Record_Field;

   ------------------------
   -- Finish_Record_Type --
   ------------------------

   procedure Free_Elements (Elements : in out O_Element_List)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (O_Element, O_Element_Acc);
      El : O_Element_Acc;
      Next_El : O_Element_Acc;
   begin
      --  Free elements
      El := Elements.First_Elem;
      while El /= null loop
         Next_El := El.Next;
         Free (El);
         El := Next_El;
      end loop;
      Elements.First_Elem := null;
      Elements.Last_Elem := null;
   end Free_Elements;

   procedure Finish_Record_Type
     (Elements : in out O_Element_List; Res : out O_Tnode)
   is
      Count : constant unsigned := unsigned (Elements.Nbr_Elements);
      El : O_Element_Acc;
      Types : TypeRefArray (1 .. Count);
   begin
      El := Elements.First_Elem;
      for I in Types'Range loop
         Types (I) := Get_LLVM_Type (El.Etype);
         El := El.Next;
      end loop;

      if Elements.Rec_Type /= null then
         --  Completion
         StructSetBody (Elements.Rec_Type.LLVM, Types, Count, 0);
         Res := Elements.Rec_Type;
      else
         Res := new O_Tnode_Type'(Kind => ON_Record_Type,
                                  LLVM => StructType (Types, Count, 0),
                                  Dbg => Null_ValueRef);
      end if;

      Free_Elements (Elements);
   end Finish_Record_Type;

   --------------------------------
   -- New_Uncomplete_Record_Type --
   --------------------------------

   procedure New_Uncomplete_Record_Type (Res : out O_Tnode) is
   begin
      --  LLVM type will be created when the type is declared, as the name
      --  is required (for unification).
      Res := new O_Tnode_Type'(Kind => ON_Incomplete_Record_Type,
                               LLVM => Null_TypeRef,
                               Dbg => Null_ValueRef);
   end New_Uncomplete_Record_Type;

   ----------------------------------
   -- Start_Uncomplete_Record_Type --
   ----------------------------------

   procedure Start_Uncomplete_Record_Type
     (Res : O_Tnode;
      Elements : out O_Element_List)
   is
   begin
      if Res.Kind /= ON_Incomplete_Record_Type then
         raise Program_Error;
      end if;
      Elements := (Kind => OF_Record,
                   Nbr_Elements => 0,
                   Rec_Type => Res,
                   Size => 0,
                   Align => 0,
                   Align_Type => Null_TypeRef,
                   First_Elem => null,
                   Last_Elem => null);
   end Start_Uncomplete_Record_Type;

   ----------------------
   -- Start_Union_Type --
   ----------------------

   procedure Start_Union_Type (Elements : out O_Element_List) is
   begin
      Elements := (Kind => OF_Union,
                   Nbr_Elements => 0,
                   Rec_Type => O_Tnode_Null,
                   Size => 0,
                   Align => 0,
                   Align_Type => Null_TypeRef,
                   First_Elem => null,
                   Last_Elem => null);
   end Start_Union_Type;

   ---------------------
   -- New_Union_Field --
   ---------------------

   procedure New_Union_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident;
      Etype : O_Tnode)
   is
      El_Type : constant TypeRef := Get_LLVM_Type (Etype);
      Size : constant unsigned :=
        unsigned (ABISizeOfType (Target_Data, El_Type));
      Align : constant Unsigned_32 :=
        ABIAlignmentOfType (Target_Data, El_Type);
   begin
      El := (Kind => OF_Union,
             Ftype => Etype,
             Utype => El_Type,
             Ptr_Type => PointerType (El_Type));
      if Size > Elements.Size then
         Elements.Size := Size;
      end if;
      if Elements.Align_Type = Null_TypeRef or else Align > Elements.Align then
         Elements.Align := Align;
         Elements.Align_Type := El_Type;
      end if;
      Add_Field (Elements, Ident, Etype);
   end New_Union_Field;

   -----------------------
   -- Finish_Union_Type --
   -----------------------

   procedure Finish_Union_Type
     (Elements : in out O_Element_List;
      Res : out O_Tnode)
   is
      Count : unsigned;
      Types : TypeRefArray (1 .. 2);
      Pad : unsigned;
   begin
      if Elements.Align_Type = Null_TypeRef then
         --  An empty union.  Is it allowed ?
         Count := 0;
      else
         --  The first element is the field with the biggest alignment
         Types (1) := Elements.Align_Type;
         --  Possibly complete with an array of bytes.
         Pad := Elements.Size
           - unsigned (ABISizeOfType (Target_Data, Elements.Align_Type));
         if Pad /= 0 then
            Types (2) := ArrayType (Int8Type, Pad);
            Count := 2;
         else
            Count := 1;
         end if;
      end if;
      Res := new O_Tnode_Type'(Kind => ON_Union_Type,
                               LLVM => StructType (Types, Count, 0),
                               Dbg => Null_ValueRef,
                               Un_Size => Elements.Size,
                               Un_Main_Field => Elements.Align_Type);

      Free_Elements (Elements);
   end Finish_Union_Type;

   ---------------------
   -- New_Access_Type --
   ---------------------

   function New_Access_Type (Dtype : O_Tnode) return O_Tnode is
   begin
      if Dtype = O_Tnode_Null then
         --  LLVM type will be built by New_Type_Decl, so that the name
         --  can be used for the structure.
         return new O_Tnode_Type'(Kind => ON_Incomplete_Access_Type,
                                  LLVM => Null_TypeRef,
                                  Dbg => Null_ValueRef,
                                  Acc_Type => O_Tnode_Null);
      else
         return new O_Tnode_Type'(Kind => ON_Access_Type,
                                  LLVM => PointerType (Get_LLVM_Type (Dtype)),
                                  Dbg => Null_ValueRef,
                                  Acc_Type => Dtype);
      end if;
   end New_Access_Type;

   ------------------------
   -- Finish_Access_Type --
   ------------------------

   procedure Finish_Access_Type (Atype : O_Tnode; Dtype : O_Tnode)
   is
      Types : TypeRefArray (1 .. 1);
   begin
      if Atype.Kind /= ON_Incomplete_Access_Type then
         --  Not an incomplete access type.
         raise Program_Error;
      end if;
      if Atype.Acc_Type /= O_Tnode_Null then
         --  Already completed.
         raise Program_Error;
      end if;
      --  Completion
      Types (1) := Get_LLVM_Type (Dtype);
      StructSetBody (GetElementType (Atype.LLVM), Types, Types'Length, 0);
      Atype.Acc_Type := Dtype;
   end Finish_Access_Type;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type (El_Type : O_Tnode; Index_Type : O_Tnode)
                           return O_Tnode
   is
      pragma Unreferenced (Index_Type);
      Res : O_Tnode;
   begin
      Res := new O_Tnode_Type'
        (Kind => ON_Array_Type,
         LLVM => ArrayType (Get_LLVM_Type (El_Type), 0),
         Dbg => Null_ValueRef,
         Arr_El_Type => El_Type);

      return Res;
   end New_Array_Type;

   --------------------------------
   -- New_Constrained_Array_Type --
   --------------------------------

   function New_Constrained_Array_Type
     (Atype : O_Tnode; Length : O_Cnode) return O_Tnode
   is
      Res : O_Tnode;
      Len : constant unsigned := unsigned (ConstIntGetZExtValue (Length.LLVM));
   begin
      Res := new O_Tnode_Type'
        (Kind => ON_Array_Sub_Type,
         LLVM => ArrayType (GetElementType (Get_LLVM_Type (Atype)), Len),
         Dbg => Null_ValueRef,
         Arr_El_Type => Atype.Arr_El_Type);

      return Res;
   end New_Constrained_Array_Type;

   -----------------------
   -- New_Unsigned_Type --
   -----------------------

   function Size_To_Llvm (Size : Natural) return TypeRef is
      Llvm : TypeRef;
   begin
      case Size is
         when 8 =>
            Llvm := Int8Type;
         when 32 =>
            Llvm := Int32Type;
         when 64 =>
            Llvm := Int64Type;
         when others =>
            raise Program_Error;
      end case;
      return Llvm;
   end Size_To_Llvm;

   function New_Unsigned_Type (Size : Natural) return O_Tnode is
   begin
      return new O_Tnode_Type'(Kind => ON_Unsigned_Type,
                               LLVM => Size_To_Llvm (Size),
                               Dbg => Null_ValueRef,
                               Scal_Size => Size);
   end New_Unsigned_Type;

   ---------------------
   -- New_Signed_Type --
   ---------------------

   function New_Signed_Type (Size : Natural) return O_Tnode is
   begin
      return new O_Tnode_Type'(Kind => ON_Signed_Type,
                               LLVM => Size_To_Llvm (Size),
                               Dbg => Null_ValueRef,
                               Scal_Size => Size);
   end New_Signed_Type;

   --------------------
   -- New_Float_Type --
   --------------------

   function New_Float_Type return O_Tnode is
   begin
      return new O_Tnode_Type'(Kind => ON_Float_Type,
                               LLVM => DoubleType,
                               Dbg => Null_ValueRef,
                               Scal_Size => 64);
   end New_Float_Type;

   ----------------------
   -- New_Boolean_Type --
   ----------------------

   procedure New_Boolean_Type
     (Res : out O_Tnode;
      False_Id : O_Ident; False_E : out O_Cnode;
      True_Id : O_Ident; True_E : out O_Cnode)
   is
      pragma Unreferenced (False_Id, True_Id);
   begin
      Res := new O_Tnode_Type'(Kind => ON_Boolean_Type,
                               LLVM => Int1Type,
                               Dbg => Null_ValueRef,
                               Scal_Size => 1);
      False_E := O_Cnode'(LLVM => ConstInt (Res.LLVM, 0, 0),
                          Ctype => Res);
      True_E := O_Cnode'(LLVM => ConstInt (Res.LLVM, 1, 0),
                         Ctype => Res);
   end New_Boolean_Type;

   ---------------------
   -- Start_Enum_Type --
   ---------------------

   procedure Start_Enum_Type (List : out O_Enum_List; Size : Natural)
   is
      LLVM : constant TypeRef := Size_To_Llvm (Size);
   begin
      List := (LLVM => LLVM,
               Num => 0,
               Etype => new O_Tnode_Type'(Kind => ON_Enum_Type,
                                          LLVM => LLVM,
                                          Scal_Size => Size,
                                          Dbg => Null_ValueRef));

   end Start_Enum_Type;

   ----------------------
   -- New_Enum_Literal --
   ----------------------

   procedure New_Enum_Literal
     (List : in out O_Enum_List; Ident : O_Ident; Res : out O_Cnode)
   is
      pragma Unreferenced (Ident);
   begin
      Res := O_Cnode'(LLVM => ConstInt (List.LLVM, Unsigned_64 (List.Num), 0),
                      Ctype => List.Etype);

      List.Num := List.Num + 1;
   end New_Enum_Literal;

   ----------------------
   -- Finish_Enum_Type --
   ----------------------

   procedure Finish_Enum_Type (List : in out O_Enum_List; Res : out O_Tnode) is
   begin
      Res := List.Etype;
   end Finish_Enum_Type;

   ------------------------
   -- New_Signed_Literal --
   ------------------------

   function New_Signed_Literal (Ltype : O_Tnode; Value : Integer_64)
                               return O_Cnode
   is
      function To_Unsigned_64 is new Ada.Unchecked_Conversion
        (Integer_64, Unsigned_64);
   begin
      return O_Cnode'(LLVM => ConstInt (Get_LLVM_Type (Ltype),
                                        To_Unsigned_64 (Value), 1),
                     Ctype => Ltype);
   end New_Signed_Literal;

   --------------------------
   -- New_Unsigned_Literal --
   --------------------------

   function New_Unsigned_Literal (Ltype : O_Tnode; Value : Unsigned_64)
                                 return O_Cnode is
   begin
      return O_Cnode'(LLVM => ConstInt (Get_LLVM_Type (Ltype), Value, 0),
                      Ctype => Ltype);
   end New_Unsigned_Literal;

   -----------------------
   -- New_Float_Literal --
   -----------------------

   function New_Float_Literal (Ltype : O_Tnode; Value : IEEE_Float_64)
                              return O_Cnode is
   begin
      return O_Cnode'(LLVM => ConstReal (Get_LLVM_Type (Ltype),
                                         Interfaces.C.double (Value)),
                      Ctype => Ltype);
   end New_Float_Literal;

   ---------------------
   -- New_Null_Access --
   ---------------------

   function New_Null_Access (Ltype : O_Tnode) return O_Cnode is
   begin
      return O_Cnode'(LLVM => ConstNull (Get_LLVM_Type (Ltype)),
                      Ctype => Ltype);
   end New_Null_Access;

   -----------------------
   -- Start_Record_Aggr --
   -----------------------

   procedure Start_Record_Aggr
     (List : out O_Record_Aggr_List;
      Atype : O_Tnode)
   is
      Llvm : constant TypeRef := Get_LLVM_Type (Atype);
   begin
      List :=
        (Len => 0,
         Vals => new ValueRefArray (1 .. CountStructElementTypes (Llvm)),
         Atype => Atype);
   end Start_Record_Aggr;

   ------------------------
   -- New_Record_Aggr_El --
   ------------------------

   procedure New_Record_Aggr_El
     (List : in out O_Record_Aggr_List; Value : O_Cnode)
   is
   begin
      List.Len := List.Len + 1;
      List.Vals (List.Len) := Value.LLVM;
   end New_Record_Aggr_El;

   ------------------------
   -- Finish_Record_Aggr --
   ------------------------

   procedure Finish_Record_Aggr
     (List : in out O_Record_Aggr_List;
      Res : out O_Cnode)
   is
      V : ValueRef;
   begin
      if List.Atype.Kind = ON_Incomplete_Record_Type then
         V := ConstNamedStruct (Get_LLVM_Type (List.Atype),
                                List.Vals.all, List.Len);
      else
         V := ConstStruct (List.Vals.all, List.Len, 0);
      end if;
      Res := (LLVM => V, Ctype => List.Atype);
      Free (List.Vals);
   end Finish_Record_Aggr;

   ----------------------
   -- Start_Array_Aggr --
   ----------------------

   procedure Start_Array_Aggr
     (List : out O_Array_Aggr_List; Atype : O_Tnode; Len : Unsigned_32)
   is
      Llvm : constant TypeRef := Get_LLVM_Type (Atype);
   begin
      List := (Len => 0,
               Vals => new ValueRefArray (1 .. unsigned (Len)),
               El_Type => GetElementType (Llvm),
               Atype => Atype);
   end Start_Array_Aggr;

   -----------------------
   -- New_Array_Aggr_El --
   -----------------------

   procedure New_Array_Aggr_El (List : in out O_Array_Aggr_List;
                                Value : O_Cnode)
   is
   begin
      List.Len := List.Len + 1;
      List.Vals (List.Len) := Value.LLVM;
   end New_Array_Aggr_El;

   -----------------------
   -- Finish_Array_Aggr --
   -----------------------

   procedure Finish_Array_Aggr (List : in out O_Array_Aggr_List;
                                Res : out O_Cnode)
   is
   begin
      Res := (LLVM => ConstArray (List.El_Type,
                                  List.Vals.all, List.Len),
             Ctype => List.Atype);
      Free (List.Vals);
   end Finish_Array_Aggr;

   --------------------
   -- New_Union_Aggr --
   --------------------

   function New_Union_Aggr (Atype : O_Tnode; Field : O_Fnode; Value : O_Cnode)
                           return O_Cnode
   is
      Values : ValueRefArray (1 .. 2);
      Count : unsigned;
      Size : constant unsigned :=
        unsigned (ABISizeOfType (Target_Data, Field.Utype));

   begin
      Values (1) := Value.LLVM;
      if Size < Atype.Un_Size then
         Values (2) := GetUndef (ArrayType (Int8Type, Atype.Un_Size - Size));
         Count := 2;
      else
         Count := 1;
      end if;

      --  If `FIELD` is the main field of the union, create a struct using
      --  the same type as the union (and possibly pad).
      if Field.Utype = Atype.Un_Main_Field then
         return O_Cnode'
           (LLVM => ConstNamedStruct (Atype.LLVM, Values, Count),
            Ctype => Atype);
      else
         --  Create an on-the-fly record.
         return O_Cnode'(LLVM => ConstStruct (Values, Count, 0),
                         Ctype => Atype);
      end if;
   end New_Union_Aggr;

   -----------------------
   -- New_Default_Value --
   -----------------------

   function New_Default_Value (Ltype : O_Tnode) return O_Cnode is
   begin
      return O_Cnode'(LLVM => ConstNull (Ltype.LLVM),
                      Ctype => Ltype);
   end New_Default_Value;

   ----------------
   -- New_Sizeof --
   ----------------

   --  Return VAL with type RTYPE (either unsigned or access)
   function Const_To_Cnode (Rtype : O_Tnode; Val : Unsigned_64) return O_Cnode
   is
      Tmp : ValueRef;
   begin
      case Rtype.Kind is
         when ON_Scalar_Types =>
            --  Well, unsigned in fact.
            return O_Cnode'(LLVM => ConstInt (Rtype.LLVM, Val, 0),
                            Ctype => Rtype);
         when ON_Access_Type =>
            Tmp := ConstInt (Int64Type, Val, 0);
            return O_Cnode'(LLVM => ConstIntToPtr (Tmp, Rtype.LLVM),
                            Ctype => Rtype);
         when others =>
            raise Program_Error;
      end case;
   end Const_To_Cnode;

   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode is
   begin
      return Const_To_Cnode
        (Rtype, ABISizeOfType (Target_Data, Get_LLVM_Type (Atype)));
   end New_Sizeof;

   -----------------
   -- New_Alignof --
   -----------------

   function New_Alignof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode is
   begin
      return Const_To_Cnode
        (Rtype,
         Unsigned_64
           (ABIAlignmentOfType (Target_Data, Get_LLVM_Type (Atype))));
   end New_Alignof;

   ------------------
   -- New_Offsetof --
   ------------------

   function New_Offsetof (Atype : O_Tnode; Field : O_Fnode; Rtype : O_Tnode)
                         return O_Cnode is
   begin
      return Const_To_Cnode
        (Rtype,
         OffsetOfElement (Target_Data,
                          Get_LLVM_Type (Atype),
                          Unsigned_32 (Field.Index)));
   end New_Offsetof;

   ----------------------------
   -- New_Subprogram_Address --
   ----------------------------

   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
                                   return O_Cnode is
   begin
      return O_Cnode'
        (LLVM => ConstBitCast (Subprg.LLVM, Get_LLVM_Type (Atype)),
         Ctype => Atype);
   end New_Subprogram_Address;

   ------------------------
   -- New_Global_Address --
   ------------------------

   function New_Global_Address (Lvalue : O_Gnode; Atype : O_Tnode)
                               return O_Cnode is
   begin
      return New_Global_Unchecked_Address (Lvalue, Atype);
   end New_Global_Address;

   ----------------------------------
   -- New_Global_Unchecked_Address --
   ----------------------------------

   function New_Global_Unchecked_Address (Lvalue : O_Gnode; Atype : O_Tnode)
                                         return O_Cnode is
   begin
      return O_Cnode'(LLVM => ConstBitCast (Lvalue.LLVM,
                                            Get_LLVM_Type (Atype)),
                      Ctype => Atype);
   end New_Global_Unchecked_Address;

   -------------
   -- New_Lit --
   -------------

   function New_Lit (Lit : O_Cnode) return O_Enode is
   begin
      return O_Enode'(LLVM => Lit.LLVM,
                      Etype => Lit.Ctype);
   end New_Lit;

   ----------------
   -- New_Global --
   ----------------

   function New_Global (Decl : O_Dnode) return O_Gnode is
   begin
      --  Can be used to build global objects, even when Unreach is set.
      --  As this doesn't generate code, this is ok.
      case Decl.Kind is
         when ON_Const_Decl
           | ON_Var_Decl =>
            return O_Gnode'(LLVM => Decl.LLVM,
                            Ltype => Decl.Dtype);
         when others =>
            raise Program_Error;
      end case;
   end New_Global;

   -------------------
   -- New_Dyadic_Op --
   -------------------

   function New_Smod (L, R : ValueRef; Res_Type : TypeRef)
                     return ValueRef
   is
      Cond : ValueRef;
      Br : ValueRef;
      pragma Unreferenced (Br);

      --  The result of 'L rem R'.
      Rm : ValueRef;

      --  Rm + R
      Rm_Plus_R : ValueRef;

      --  The result of 'L xor R'.
      R_Xor : ValueRef;

      Adj : ValueRef;
      Phi : ValueRef;

      --  Basic basic for the non-overflow branch
      Normal_Bb : constant BasicBlockRef :=
        AppendBasicBlock (Cur_Func, Empty_Cstring);

      Adjust_Bb : constant BasicBlockRef :=
        AppendBasicBlock (Cur_Func, Empty_Cstring);

      --  Basic block after the result
      Next_Bb : constant BasicBlockRef :=
        AppendBasicBlock (Cur_Func, Empty_Cstring);

      Vals : ValueRefArray (1 .. 3);
      BBs  : BasicBlockRefArray (1 .. 3);
   begin
      --  Avoid overflow with -1:
      --   if R = -1 then
      --     result := 0;
      --   else
      --     ...
      Cond := BuildICmp
        (Builder, IntEQ, R, ConstAllOnes (Res_Type), Empty_Cstring);
      Br := BuildCondBr (Builder, Cond, Next_Bb, Normal_Bb);
      Vals (1) := ConstNull (Res_Type);
      BBs (1) := GetInsertBlock (Builder);

      --  Rm := Left rem Right
      PositionBuilderAtEnd (Builder, Normal_Bb);
      Rm := BuildSRem (Builder, L, R, Empty_Cstring);

      --  if Rm = 0 then
      --    result := 0
      --  else
      Cond := BuildICmp
        (Builder, IntEQ, Rm, ConstNull (Res_Type), Empty_Cstring);
      Br := BuildCondBr (Builder, Cond, Next_Bb, Adjust_Bb);
      Vals (2) := ConstNull (Res_Type);
      BBs (2) := Normal_Bb;

      --  if (L xor R) < 0 then
      --    result := Rm + R
      --  else
      --    result := Rm;
      --  end if;
      PositionBuilderAtEnd (Builder, Adjust_Bb);
      R_Xor := BuildXor (Builder, L, R, Empty_Cstring);
      Cond := BuildICmp
        (Builder, IntSLT, R_Xor, ConstNull (Res_Type), Empty_Cstring);
      Rm_Plus_R := BuildAdd (Builder, Rm, R, Empty_Cstring);
      Adj := BuildSelect (Builder, Cond, Rm_Plus_R, Rm, Empty_Cstring);
      Br := BuildBr (Builder, Next_Bb);
      Vals (3) := Adj;
      BBs (3) := Adjust_Bb;

      --  The Phi node
      PositionBuilderAtEnd (Builder, Next_Bb);
      Phi := BuildPhi (Builder, Res_Type, Empty_Cstring);
      AddIncoming (Phi, Vals, BBs, Vals'Length);

      return Phi;
   end New_Smod;

   type Dyadic_Builder_Acc is access
     function (Builder : BuilderRef;
               LHS : ValueRef; RHS : ValueRef; Name : Cstring)
              return ValueRef;
   pragma Convention (C, Dyadic_Builder_Acc);

   function New_Dyadic_Op (Kind : ON_Dyadic_Op_Kind; Left, Right : O_Enode)
                          return O_Enode
   is
      Build : Dyadic_Builder_Acc := null;
      Res : ValueRef := Null_ValueRef;
   begin
      if Unreach then
         return O_Enode'(LLVM => Null_ValueRef, Etype => Left.Etype);
      end if;

      case Left.Etype.Kind is
         when ON_Integer_Types =>
            case Kind is
               when ON_And =>
                  Build := BuildAnd'Access;
               when ON_Or =>
                  Build := BuildOr'Access;
               when ON_Xor =>
                  Build := BuildXor'Access;

               when ON_Add_Ov =>
                  Build := BuildAdd'Access;
               when ON_Sub_Ov =>
                  Build := BuildSub'Access;
               when ON_Mul_Ov =>
                  Build := BuildMul'Access;

               when ON_Div_Ov =>
                  case Left.Etype.Kind is
                     when ON_Unsigned_Type =>
                        Build := BuildUDiv'Access;
                     when ON_Signed_Type =>
                        Build := BuildSDiv'Access;
                     when others =>
                        null;
                  end case;

               when ON_Mod_Ov
                 | ON_Rem_Ov =>
                  case Left.Etype.Kind is
                     when ON_Unsigned_Type =>
                        Build := BuildURem'Access;
                     when ON_Signed_Type =>
                        if Kind = ON_Rem_Ov then
                           Build := BuildSRem'Access;
                        else
                           Res := New_Smod
                             (Left.LLVM, Right.LLVM, Left.Etype.LLVM);
                        end if;
                     when others =>
                        null;
                  end case;
            end case;

         when ON_Float_Type =>
            case Kind is
               when ON_Add_Ov =>
                  Build := BuildFAdd'Access;
               when ON_Sub_Ov =>
                  Build := BuildFSub'Access;
               when ON_Mul_Ov =>
                  Build := BuildFMul'Access;
               when ON_Div_Ov =>
                  Build := BuildFDiv'Access;

               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;

      if Build /= null then
         pragma Assert (Res = Null_ValueRef);
         Res := Build.all (Builder, Left.LLVM, Right.LLVM, Empty_Cstring);
      end if;

      if Res = Null_ValueRef then
         raise Program_Error with "Unimplemented New_Dyadic_Op "
           & ON_Dyadic_Op_Kind'Image (Kind)
           & " for type "
           & ON_Type_Kind'Image (Left.Etype.Kind);
      end if;

      return O_Enode'(LLVM => Res, Etype => Left.Etype);
   end New_Dyadic_Op;

   --------------------
   -- New_Monadic_Op --
   --------------------

   function New_Monadic_Op (Kind : ON_Monadic_Op_Kind; Operand : O_Enode)
                           return O_Enode
   is
      Res : ValueRef;
   begin
      if Unreach then
         return O_Enode'(LLVM => Null_ValueRef, Etype => Operand.Etype);
      end if;

      case Operand.Etype.Kind is
         when ON_Integer_Types =>
            case Kind is
               when ON_Not =>
                  Res := BuildNot (Builder, Operand.LLVM, Empty_Cstring);
               when ON_Neg_Ov =>
                  Res := BuildNeg (Builder, Operand.LLVM, Empty_Cstring);
               when ON_Abs_Ov =>
                  Res := BuildSelect
                    (Builder,
                     BuildICmp (Builder, IntSLT,
                                Operand.LLVM,
                                ConstInt (Get_LLVM_Type (Operand.Etype), 0, 0),
                                Empty_Cstring),
                     BuildNeg (Builder, Operand.LLVM, Empty_Cstring),
                     Operand.LLVM,
                     Empty_Cstring);
            end case;
         when ON_Float_Type =>
            case Kind is
               when ON_Not =>
                  raise Program_Error;
               when ON_Neg_Ov =>
                  Res := BuildFNeg (Builder, Operand.LLVM, Empty_Cstring);
               when ON_Abs_Ov =>
                  Res := BuildSelect
                    (Builder,
                     BuildFCmp (Builder, RealOLT,
                                Operand.LLVM,
                                ConstReal (Get_LLVM_Type (Operand.Etype), 0.0),
                                Empty_Cstring),
                     BuildFNeg (Builder, Operand.LLVM, Empty_Cstring),
                     Operand.LLVM,
                     Empty_Cstring);
            end case;
         when others =>
            raise Program_Error;
      end case;

      return O_Enode'(LLVM => Res, Etype => Operand.Etype);
   end New_Monadic_Op;

   --------------------
   -- New_Compare_Op --
   --------------------

   type Compare_Op_Entry is record
      Signed_Pred   : IntPredicate;
      Unsigned_Pred : IntPredicate;
      Real_Pred     : RealPredicate;
   end record;

   type Compare_Op_Table_Type is array (ON_Compare_Op_Kind) of
     Compare_Op_Entry;

   Compare_Op_Table : constant Compare_Op_Table_Type :=
     (ON_Eq  => (IntEQ,  IntEQ,  RealOEQ),
      ON_Neq => (IntNE,  IntNE,  RealONE),
      ON_Le  => (IntSLE, IntULE, RealOLE),
      ON_Lt  => (IntSLT, IntULT, RealOLT),
      ON_Ge  => (IntSGE, IntUGE, RealOGE),
      ON_Gt  => (IntSGT, IntUGT, RealOGT));

   function New_Compare_Op
     (Kind : ON_Compare_Op_Kind;
      Left, Right : O_Enode;
      Ntype : O_Tnode)
      return O_Enode
   is
      Res : ValueRef;
   begin
      if Unreach then
         return O_Enode'(LLVM => Null_ValueRef, Etype => Ntype);
      end if;

      case Left.Etype.Kind is
         when ON_Unsigned_Type
           | ON_Boolean_Type
           | ON_Enum_Type
           | ON_Access_Type
           | ON_Incomplete_Access_Type =>
            Res := BuildICmp (Builder, Compare_Op_Table (Kind).Unsigned_Pred,
                              Left.LLVM, Right.LLVM, Empty_Cstring);
         when ON_Signed_Type =>
            Res := BuildICmp (Builder, Compare_Op_Table (Kind).Signed_Pred,
                              Left.LLVM, Right.LLVM, Empty_Cstring);
         when ON_Float_Type =>
            Res := BuildFCmp (Builder, Compare_Op_Table (Kind).Real_Pred,
                              Left.LLVM, Right.LLVM, Empty_Cstring);
         when ON_Array_Type
           | ON_Array_Sub_Type
           | ON_Record_Type
           | ON_Incomplete_Record_Type
           | ON_Union_Type
           | ON_No_Type =>
            raise Program_Error;
      end case;
      return O_Enode'(LLVM => Res, Etype => Ntype);
   end New_Compare_Op;

   -------------------------
   -- New_Indexed_Element --
   -------------------------

   function New_Indexed_Element (Arr : O_Lnode; Index : O_Enode) return O_Lnode
   is
      Idx : constant ValueRefArray (1 .. 2) :=
        (ConstInt (Int32Type, 0, 0),
         Index.LLVM);
      Tmp : ValueRef;
   begin
      if Unreach then
         Tmp := Null_ValueRef;
      else
         Tmp := BuildGEP (Builder, Arr.LLVM, Idx, Idx'Length, Empty_Cstring);
      end if;
      return O_Lnode'(Direct => False,
                      LLVM => Tmp,
                      Ltype => Arr.Ltype.Arr_El_Type);
   end New_Indexed_Element;

   ---------------
   -- New_Slice --
   ---------------

   function New_Slice (Arr : O_Lnode; Res_Type : O_Tnode; Index : O_Enode)
      return O_Lnode
   is
      Idx : constant ValueRefArray (1 .. 2) :=
        (ConstInt (Int32Type, 0, 0),
         Index.LLVM);
      Tmp : ValueRef;
   begin
      if Unreach then
         Tmp := Null_ValueRef;
      else
         Tmp := BuildGEP (Builder, Arr.LLVM, Idx, Idx'Length, Empty_Cstring);
         Tmp := BuildBitCast
           (Builder, Tmp, PointerType (Get_LLVM_Type (Res_Type)),
            Empty_Cstring);
      end if;
      return O_Lnode'(Direct => False, LLVM => Tmp, Ltype => Res_Type);
   end New_Slice;

   --------------------------
   -- New_Selected_Element --
   --------------------------

   function New_Selected_Element (Rec : O_Lnode; El : O_Fnode)
                                 return O_Lnode
   is
      Res : ValueRef;
   begin
      if Unreach then
         Res := Null_ValueRef;
      else
         case El.Kind is
            when OF_Record =>
               declare
                  Idx : constant ValueRefArray (1 .. 2) :=
                    (ConstInt (Int32Type, 0, 0),
                     ConstInt (Int32Type, Unsigned_64 (El.Index), 0));
               begin
                  Res := BuildGEP (Builder, Rec.LLVM, Idx, 2, Empty_Cstring);
               end;
            when OF_Union =>
               Res := BuildBitCast (Builder,
                                    Rec.LLVM, El.Ptr_Type, Empty_Cstring);
            when OF_None =>
               raise Program_Error;
         end case;
      end if;
      return O_Lnode'(Direct => False, LLVM => Res, Ltype => El.Ftype);
   end New_Selected_Element;

   function New_Global_Selected_Element (Rec : O_Gnode; El : O_Fnode)
                                        return O_Gnode
   is
      Res : ValueRef;
   begin
      case El.Kind is
         when OF_Record =>
            declare
               Idx : constant ValueRefArray (1 .. 2) :=
                 (ConstInt (Int32Type, 0, 0),
                  ConstInt (Int32Type, Unsigned_64 (El.Index), 0));
            begin
               Res := ConstGEP (Rec.LLVM, Idx, 2);
            end;
         when OF_Union =>
            Res := ConstBitCast (Rec.LLVM, El.Ptr_Type);
         when OF_None =>
            raise Program_Error;
      end case;
      return O_Gnode'(LLVM => Res, Ltype => El.Ftype);
   end New_Global_Selected_Element;

   ------------------------
   -- New_Access_Element --
   ------------------------

   function New_Access_Element (Acc : O_Enode) return O_Lnode
   is
      Res : ValueRef;
   begin
      case Acc.Etype.Kind is
         when ON_Access_Type =>
            Res := Acc.LLVM;
         when ON_Incomplete_Access_Type =>
            --  Unwrap the structure
            declare
               Idx : constant ValueRefArray (1 .. 2) :=
                 (ConstInt (Int32Type, 0, 0), ConstInt (Int32Type, 0, 0));
            begin
               Res := BuildGEP (Builder, Acc.LLVM, Idx, 2, Empty_Cstring);
            end;
         when others =>
            raise Program_Error;
      end case;
      return O_Lnode'(Direct => False,
                      LLVM => Res,
                      Ltype => Acc.Etype.Acc_Type);
   end New_Access_Element;

   --------------------
   -- New_Convert_Ov --
   --------------------

   function New_Convert (Val : O_Enode; Rtype : O_Tnode) return O_Enode
   is
      Res : ValueRef := Null_ValueRef;
   begin
      if Rtype = Val.Etype then
         --  Convertion to itself: nothing to do.
         return Val;
      end if;
      if Rtype.LLVM = Val.Etype.LLVM then
         --  Same underlying LLVM type: no conversion but keep new type in
         --  case of change of sign.
         return O_Enode'(LLVM => Val.LLVM, Etype => Rtype);
      end if;
      if Unreach then
         return O_Enode'(LLVM => Val.LLVM, Etype => Rtype);
      end if;

      case Rtype.Kind is
         when ON_Integer_Types =>
            case Val.Etype.Kind is
               when ON_Integer_Types =>
                  --  Int to Int
                  if Val.Etype.Scal_Size > Rtype.Scal_Size then
                     --  Truncate
                     Res := BuildTrunc
                       (Builder, Val.LLVM, Get_LLVM_Type (Rtype),
                        Empty_Cstring);
                  elsif Val.Etype.Scal_Size < Rtype.Scal_Size then
                     if Val.Etype.Kind = ON_Signed_Type then
                        Res := BuildSExt
                          (Builder, Val.LLVM, Get_LLVM_Type (Rtype),
                           Empty_Cstring);
                     else
                        --  Unsigned, enum
                        Res := BuildZExt
                          (Builder, Val.LLVM, Get_LLVM_Type (Rtype),
                           Empty_Cstring);
                     end if;
                  else
                     Res := BuildBitCast
                       (Builder, Val.LLVM, Get_LLVM_Type (Rtype),
                        Empty_Cstring);
                  end if;

               when ON_Float_Type =>
                  --  Float to Int
                  if Rtype.Kind = ON_Signed_Type then
                     --  FPtoSI rounds toward zero, so we need to add
                     --  copysign (0.5, x).
                     declare
                        V : ValueRef;
                     begin
                        V := BuildCall (Builder, Copysign_Fun,
                                        (Fp_0_5, Val.LLVM), 2, Empty_Cstring);
                        V := BuildFAdd (Builder, Val.LLVM, V, Empty_Cstring);
                        Res := BuildFPToSI
                          (Builder, V, Get_LLVM_Type (Rtype), Empty_Cstring);
                     end;
                  end if;

               when others =>
                  null;
            end case;

         when ON_Float_Type =>
            if Val.Etype.Kind = ON_Signed_Type then
               Res := BuildSIToFP
                 (Builder, Val.LLVM, Get_LLVM_Type (Rtype),
                  Empty_Cstring);
            elsif Val.Etype.Kind = ON_Unsigned_Type then
               Res := BuildUIToFP
                 (Builder, Val.LLVM, Get_LLVM_Type (Rtype),
                  Empty_Cstring);
            end if;

         when ON_Access_Type
           | ON_Incomplete_Access_Type =>
            if GetTypeKind (TypeOf (Val.LLVM)) /= PointerTypeKind then
               raise Program_Error;
            end if;
            Res := BuildBitCast (Builder, Val.LLVM, Get_LLVM_Type (Rtype),
                                 Empty_Cstring);

         when others =>
            null;
      end case;
      if Res /= Null_ValueRef then
         --  FIXME: only if insn was generated
         --  Set_Insn_Dbg (Res);
         return O_Enode'(LLVM => Res, Etype => Rtype);
      else
         raise Program_Error with "New_Convert: not implemented for "
           & ON_Type_Kind'Image (Val.Etype.Kind)
           & " -> "
           & ON_Type_Kind'Image (Rtype.Kind);
      end if;
   end New_Convert;

   function New_Convert_Ov (Val : O_Enode; Rtype : O_Tnode) return O_Enode is
   begin
      return New_Convert (Val, Rtype);
   end New_Convert_Ov;

   -----------------
   -- New_Address --
   -----------------

   function New_Address (Lvalue : O_Lnode; Atype : O_Tnode) return O_Enode is
   begin
      return New_Unchecked_Address (Lvalue, Atype);
   end New_Address;

   ---------------------------
   -- New_Unchecked_Address --
   ---------------------------

   function New_Unchecked_Address  (Lvalue : O_Lnode; Atype : O_Tnode)
                                   return O_Enode
   is
      Res : ValueRef;
   begin
      if Unreach then
         Res := Null_ValueRef;
      else
         Res := BuildBitCast (Builder, Lvalue.LLVM, Get_LLVM_Type (Atype),
                              Empty_Cstring);
      end if;
      return O_Enode'(LLVM => Res, Etype => Atype);
   end New_Unchecked_Address;

   ---------------
   -- New_Value --
   ---------------

   function New_Value (Lvalue : O_Lnode) return O_Enode
   is
      Res : ValueRef;
   begin
      if Unreach then
         Res := Null_ValueRef;
      else
         Res := Lvalue.LLVM;
         if not Lvalue.Direct then
            Res := BuildLoad (Builder, Res, Empty_Cstring);
         end if;
      end if;
      return O_Enode'(LLVM => Res, Etype => Lvalue.Ltype);
   end New_Value;

   -------------------
   -- New_Obj_Value --
   -------------------

   function New_Obj_Value (Obj : O_Dnode) return O_Enode is
   begin
      return New_Value (New_Obj (Obj));
   end New_Obj_Value;

   -------------
   -- New_Obj --
   -------------

   function New_Obj (Obj : O_Dnode) return O_Lnode is
   begin
      --  Can be used to build global objects, even when Unreach is set.
      --  As this doesn't generate code, this is ok.
      case Obj.Kind is
         when ON_Const_Decl
           | ON_Var_Decl
           | ON_Local_Decl =>
            return O_Lnode'(Direct => False,
                            LLVM => Obj.LLVM,
                            Ltype => Obj.Dtype);

         when ON_Interface_Decl =>
            return O_Lnode'(Direct => True,
                            LLVM => Obj.Inter.Ival,
                            Ltype => Obj.Dtype);

         when ON_Type_Decl
           | ON_Completed_Type_Decl
           | ON_Subprg_Decl
           | ON_No_Decl =>
            raise Program_Error;
      end case;
   end New_Obj;

   ----------------
   -- New_Alloca --
   ----------------

   function New_Alloca (Rtype : O_Tnode; Size : O_Enode) return O_Enode
   is
      Res : ValueRef;
   begin
      if Unreach then
         Res := Null_ValueRef;
      else
         if Cur_Declare_Block.Stack_Value = Null_ValueRef
           and then Cur_Declare_Block.Prev /= null
         then
            --  Save stack pointer at entry of block
            declare
               First_Insn : ValueRef;
               Bld : BuilderRef;
            begin
               First_Insn := GetFirstInstruction (Cur_Declare_Block.Stmt_Bb);
               if First_Insn = Null_ValueRef then
                  --  Alloca is the first instruction, save the stack now.
                  Bld := Builder;
               else
                  --  There are instructions before alloca, insert the save
                  --  at the beginning.
                  PositionBuilderBefore (Extra_Builder, First_Insn);
                  Bld := Extra_Builder;
               end if;

               Cur_Declare_Block.Stack_Value :=
                 BuildCall (Bld, Stacksave_Fun,
                            (1 .. 0 => Null_ValueRef), 0, Empty_Cstring);
            end;
         end if;

         Res := BuildArrayAlloca
           (Builder, Int8Type, Size.LLVM, Empty_Cstring);

         Res := BuildBitCast
           (Builder, Res, Get_LLVM_Type (Rtype), Empty_Cstring);
      end if;

      return O_Enode'(LLVM => Res, Etype => Rtype);
   end New_Alloca;

   -------------------
   -- New_Type_Decl --
   -------------------

   procedure New_Type_Decl (Ident : O_Ident; Atype : O_Tnode) is
   begin
      --  Create the incomplete structure.  This is the only way in LLVM to
      --  build recursive types.
      case Atype.Kind is
         when ON_Incomplete_Record_Type =>
            Atype.LLVM :=
              StructCreateNamed (GetGlobalContext, Get_Cstring (Ident));
         when ON_Incomplete_Access_Type =>
            Atype.LLVM := PointerType
              (StructCreateNamed (GetGlobalContext, Get_Cstring (Ident)));
         when others =>
            null;
      end case;
   end New_Type_Decl;

   -----------------------------
   -- New_Debug_Filename_Decl --
   -----------------------------

   procedure New_Debug_Filename_Decl (Filename : String) is
   begin
      null;
   end New_Debug_Filename_Decl;

   -------------------------
   -- New_Debug_Line_Decl --
   -------------------------

   procedure New_Debug_Line_Decl (Line : Natural) is
   begin
      null;
   end New_Debug_Line_Decl;

   ----------------------------
   -- New_Debug_Comment_Decl --
   ----------------------------

   procedure New_Debug_Comment_Decl (Comment : String) is
   begin
      null;
   end New_Debug_Comment_Decl;

   --------------------
   -- New_Const_Decl --
   --------------------

   procedure New_Const_Decl
     (Res : out O_Dnode; Ident : O_Ident; Storage : O_Storage; Atype : O_Tnode)
   is
      Decl : ValueRef;
   begin
      if Storage = O_Storage_External then
         Decl := GetNamedGlobal (Module, Get_Cstring (Ident));
      else
         Decl := Null_ValueRef;
      end if;
      if Decl = Null_ValueRef then
         Decl := AddGlobal
           (Module, Get_LLVM_Type (Atype), Get_Cstring (Ident));
      end if;

      Res := (Kind => ON_Const_Decl, LLVM => Decl, Dtype => Atype);
      SetGlobalConstant (Res.LLVM, 1);
      if Storage = O_Storage_Private then
         SetLinkage (Res.LLVM, InternalLinkage);
      end if;
   end New_Const_Decl;

   -----------------------
   -- Start_Init_Value --
   -----------------------

   procedure Start_Init_Value (Decl : in out O_Dnode) is
   begin
      null;
   end Start_Init_Value;

   ------------------------
   -- Finish_Init_Value --
   ------------------------

   procedure Finish_Init_Value (Decl : in out O_Dnode; Val : O_Cnode) is
   begin
      SetInitializer (Decl.LLVM, Val.LLVM);
   end Finish_Init_Value;

   ------------------
   -- New_Var_Decl --
   ------------------

   procedure New_Var_Decl
     (Res : out O_Dnode; Ident : O_Ident; Storage : O_Storage; Atype : O_Tnode)
   is
      Decl : ValueRef;
   begin
      if Storage = O_Storage_Local then
         if Unreach then
            Decl := Null_ValueRef;
         else
            Decl := BuildAlloca
              (Decl_Builder, Get_LLVM_Type (Atype), Get_Cstring (Ident));
         end if;
         Res := (Kind => ON_Local_Decl, LLVM => Decl, Dtype => Atype);
      else
         if Storage = O_Storage_External then
            Decl := GetNamedGlobal (Module, Get_Cstring (Ident));
         else
            Decl := Null_ValueRef;
         end if;
         if Decl = Null_ValueRef then
            Decl := AddGlobal
              (Module, Get_LLVM_Type (Atype), Get_Cstring (Ident));
         end if;

         Res := (Kind => ON_Var_Decl, LLVM => Decl, Dtype => Atype);

         --  Set linkage.
         case Storage is
            when O_Storage_Private =>
               SetLinkage (Res.LLVM, InternalLinkage);
            when O_Storage_Public
              | O_Storage_External =>
               null;
            when O_Storage_Local =>
               raise Program_Error;
         end case;

         --  Set initializer.
         case Storage is
            when O_Storage_Private
              | O_Storage_Public =>
               SetInitializer (Res.LLVM, ConstNull (Get_LLVM_Type (Atype)));
            when O_Storage_External =>
               null;
            when O_Storage_Local =>
               raise Program_Error;
         end case;
      end if;
   end New_Var_Decl;

   -------------------------
   -- Start_Function_Decl --
   -------------------------

   procedure Start_Function_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage;
      Rtype : O_Tnode)
   is
   begin
      Interfaces := (Ident => Ident,
                     Storage => Storage,
                     Res_Type => Rtype,
                     Nbr_Inter => 0,
                     First_Inter => null,
                     Last_Inter => null);
   end Start_Function_Decl;

   --------------------------
   -- Start_Procedure_Decl --
   --------------------------

   procedure Start_Procedure_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage)
   is
   begin
      Interfaces := (Ident => Ident,
                     Storage => Storage,
                     Res_Type => O_Tnode_Null,
                     Nbr_Inter => 0,
                     First_Inter => null,
                     Last_Inter => null);
   end Start_Procedure_Decl;

   ------------------------
   -- New_Interface_Decl --
   ------------------------

   procedure New_Interface_Decl
     (Interfaces : in out O_Inter_List;
      Res : out O_Dnode;
      Ident : O_Ident;
      Atype : O_Tnode)
   is
      Inter : constant O_Inter_Acc := new O_Inter'(Itype => Atype,
                                                   Ival => Null_ValueRef,
                                                   Ident => Ident,
                                                   Next => null);
   begin
      Res := (Kind => ON_Interface_Decl,
              Dtype => Atype,
              LLVM => Null_ValueRef,
              Inter => Inter);
      Interfaces.Nbr_Inter := Interfaces.Nbr_Inter + 1;
      if Interfaces.First_Inter = null then
         Interfaces.First_Inter := Inter;
      else
         Interfaces.Last_Inter.Next := Inter;
      end if;
      Interfaces.Last_Inter := Inter;
   end New_Interface_Decl;

   ----------------------------
   -- Finish_Subprogram_Decl --
   ----------------------------

   procedure Finish_Subprogram_Decl
     (Interfaces : in out O_Inter_List;
      Res : out O_Dnode)
   is
      Count : constant unsigned := unsigned (Interfaces.Nbr_Inter);
      Inter : O_Inter_Acc;
      Types : TypeRefArray (1 .. Count);
      Ftype : TypeRef;
      Rtype : TypeRef;
      Decl : ValueRef;
      Id : constant Cstring := Get_Cstring (Interfaces.Ident);
   begin
      --  Fill Types (from interfaces list)
      Inter := Interfaces.First_Inter;
      for I in 1 .. Count loop
         Types (I) := Inter.Itype.LLVM;
         Inter := Inter.Next;
      end loop;

      --  Build function type.
      if Interfaces.Res_Type = O_Tnode_Null then
         Rtype := VoidType;
      else
         Rtype := Interfaces.Res_Type.LLVM;
      end if;
      Ftype := FunctionType (Rtype, Types, Count, 0);

      if Interfaces.Storage = O_Storage_External then
         Decl := GetNamedFunction (Module, Id);
      else
         Decl := Null_ValueRef;
      end if;
      if Decl = Null_ValueRef then
         Decl := AddFunction (Module, Id, Ftype);
         AddAttributeAtIndex (Decl, AttributeFunctionIndex, Nounwind_Attr);
         AddAttributeAtIndex (Decl, AttributeFunctionIndex, Uwtable_Attr);
      end if;

      Res := (Kind => ON_Subprg_Decl,
              Dtype => Interfaces.Res_Type,
              Subprg_Id => Interfaces.Ident,
              Nbr_Args => Count,
              Subprg_Inters => Interfaces.First_Inter,
              LLVM => Decl);
      SetFunctionCallConv (Res.LLVM, CCallConv);

      --  Translate interfaces.
      Inter := Interfaces.First_Inter;
      for I in 1 .. Count loop
         Inter.Ival := GetParam (Res.LLVM, I - 1);
         SetValueName (Inter.Ival, Get_Cstring (Inter.Ident));
         Inter := Inter.Next;
      end loop;
   end Finish_Subprogram_Decl;

   ---------------------------
   -- Start_Subprogram_Body --
   ---------------------------

   procedure Start_Subprogram_Body (Func : O_Dnode)
   is
      --  Basic block at function entry that contains all the declarations.
      Decl_BB : BasicBlockRef;
   begin
      if Cur_Func /= Null_ValueRef then
         --  No support for nested subprograms.
         raise Program_Error;
      end if;

      Cur_Func := Func.LLVM;
      Cur_Func_Decl := Func;

      pragma Assert (not Unreach);

      Decl_BB := AppendBasicBlock (Cur_Func, Empty_Cstring);
      PositionBuilderAtEnd (Decl_Builder, Decl_BB);

      Create_Declare_Block;

      PositionBuilderAtEnd (Builder, Cur_Declare_Block.Stmt_Bb);
   end Start_Subprogram_Body;

   ----------------------------
   -- Finish_Subprogram_Body --
   ----------------------------

   procedure Finish_Subprogram_Body is
      Ret : ValueRef;
      pragma Unreferenced (Ret);
   begin
      --  Add a jump from the declare basic block to the first statement BB.
      Ret := BuildBr (Decl_Builder, Cur_Declare_Block.Stmt_Bb);

      --  Terminate the statement BB.
      if not Unreach then
         if Cur_Func_Decl.Dtype = O_Tnode_Null then
            Ret := BuildRetVoid (Builder);
         else
            Ret := BuildUnreachable (Builder);
         end if;
      end if;

      Destroy_Declare_Block;

      Cur_Func := Null_ValueRef;

      Unreach := False;
   end Finish_Subprogram_Body;

   -------------------------
   -- New_Debug_Line_Stmt --
   -------------------------

   procedure New_Debug_Line_Stmt (Line : Natural) is
   begin
      null;
   end New_Debug_Line_Stmt;

   ----------------------------
   -- New_Debug_Comment_Stmt --
   ----------------------------

   procedure New_Debug_Comment_Stmt (Comment : String) is
   begin
      null;
   end New_Debug_Comment_Stmt;

   ------------------------
   -- Start_Declare_Stmt --
   ------------------------

   procedure Start_Declare_Stmt
   is
      Br : ValueRef;
      pragma Unreferenced (Br);
   begin
      Create_Declare_Block;

      if Unreach then
         return;
      end if;

      --  Add a jump to the new BB.
      Br := BuildBr (Builder, Cur_Declare_Block.Stmt_Bb);

      PositionBuilderAtEnd (Builder, Cur_Declare_Block.Stmt_Bb);
   end Start_Declare_Stmt;

   -------------------------
   -- Finish_Declare_Stmt --
   -------------------------

   procedure Finish_Declare_Stmt
   is
      Bb : BasicBlockRef;
      Br : ValueRef;
      Tmp : ValueRef;
      pragma Unreferenced (Br, Tmp);
   begin
      if not Unreach then
         --  Create a basic block for the statements after the declare.
         Bb := AppendBasicBlock (Cur_Func, Empty_Cstring);

         if Cur_Declare_Block.Stack_Value /= Null_ValueRef then
            --  Restore stack pointer.
            Tmp := BuildCall (Builder, Stackrestore_Fun,
                              (1 .. 1 => Cur_Declare_Block.Stack_Value), 1,
                              Empty_Cstring);
         end if;

         --  Execution will continue on the next statement
         Br := BuildBr (Builder, Bb);

         PositionBuilderAtEnd (Builder, Bb);
      end if;

      --  Do not reset Unread.

      Destroy_Declare_Block;
   end Finish_Declare_Stmt;

   -----------------------
   -- Start_Association --
   -----------------------

   procedure Start_Association (Assocs : out O_Assoc_List; Subprg : O_Dnode)
   is
   begin
      Assocs := (Subprg => Subprg,
                 Idx => 0,
                 Vals => new ValueRefArray (1 .. Subprg.Nbr_Args));
   end Start_Association;

   ---------------------
   -- New_Association --
   ---------------------

   procedure New_Association (Assocs : in out O_Assoc_List; Val : O_Enode) is
   begin
      Assocs.Idx := Assocs.Idx + 1;
      Assocs.Vals (Assocs.Idx) := Val.LLVM;
   end New_Association;

   -----------------------
   -- New_Function_Call --
   -----------------------

   function New_Function_Call (Assocs : O_Assoc_List) return O_Enode
   is
      Res : ValueRef;
      Old_Vals : ValueRefArray_Acc;
   begin
      if not Unreach then
         Res := BuildCall (Builder, Assocs.Subprg.LLVM,
                           Assocs.Vals.all, Assocs.Vals'Last, Empty_Cstring);
         Old_Vals := Assocs.Vals;
         Free (Old_Vals);
      else
         Res := Null_ValueRef;
      end if;
      return O_Enode'(LLVM => Res, Etype => Assocs.Subprg.Dtype);
   end New_Function_Call;

   ------------------------
   -- New_Procedure_Call --
   ------------------------

   procedure New_Procedure_Call (Assocs : in out O_Assoc_List)
   is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if not Unreach then
         Res := BuildCall (Builder, Assocs.Subprg.LLVM,
                           Assocs.Vals.all, Assocs.Vals'Last, Empty_Cstring);
      end if;
      Free (Assocs.Vals);
   end New_Procedure_Call;

   ---------------------
   -- New_Assign_Stmt --
   ---------------------

   procedure New_Assign_Stmt (Target : O_Lnode; Value : O_Enode)
   is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if Target.Direct then
         raise Program_Error;
      end if;
      if not Unreach then
         Res := BuildStore (Builder, Value.LLVM, Target.LLVM);
      end if;
   end New_Assign_Stmt;

   ---------------------
   -- New_Return_Stmt --
   ---------------------

   procedure New_Return_Stmt (Value : O_Enode)
   is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if Unreach then
         return;
      end if;
      Res := BuildRet (Builder, Value.LLVM);
      Unreach := True;
   end New_Return_Stmt;

   ---------------------
   -- New_Return_Stmt --
   ---------------------

   procedure New_Return_Stmt
   is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if Unreach then
         return;
      end if;
      Res := BuildRetVoid (Builder);
      Unreach := True;
   end New_Return_Stmt;

   -------------------
   -- Start_If_Stmt --
   -------------------

   procedure Start_If_Stmt (Block : in out O_If_Block; Cond : O_Enode)
   is
      Res : ValueRef;
      pragma Unreferenced (Res);
      Bb_Then : BasicBlockRef;
   begin
      if Unreach then
         Block := (Bb => Null_BasicBlockRef);
         return;
      end if;

      Bb_Then := AppendBasicBlock (Cur_Func, Empty_Cstring);
      Block := (Bb => AppendBasicBlock (Cur_Func, Empty_Cstring));
      Res := BuildCondBr (Builder, Cond.LLVM, Bb_Then, Block.Bb);

      PositionBuilderAtEnd (Builder, Bb_Then);
   end Start_If_Stmt;

   -------------------
   -- New_Else_Stmt --
   -------------------

   procedure New_Else_Stmt (Block : in out O_If_Block) is
      Res : ValueRef;
      pragma Unreferenced (Res);
      Bb_Next : BasicBlockRef;
   begin
      if not Unreach then
         Bb_Next := AppendBasicBlock (Cur_Func, Empty_Cstring);
         Res := BuildBr (Builder, Bb_Next);
      else
         if Block.Bb = Null_BasicBlockRef then
            --  The IF statement was unreachable.  Else part is also
            --  unreachable.
            return;
         end if;
         Bb_Next := Null_BasicBlockRef;
      end if;

      PositionBuilderAtEnd (Builder, Block.Bb);

      Block := (Bb => Bb_Next);
      Unreach := False;
   end New_Else_Stmt;

   --------------------
   -- Finish_If_Stmt --
   --------------------

   procedure Finish_If_Stmt (Block : in out O_If_Block) is
      Res : ValueRef;
      pragma Unreferenced (Res);
      Bb_Next : BasicBlockRef;
   begin
      if not Unreach then
         --  The branch can continue.
         if Block.Bb = Null_BasicBlockRef then
            Bb_Next := AppendBasicBlock (Cur_Func, Empty_Cstring);
         else
            Bb_Next := Block.Bb;
         end if;
         Res := BuildBr (Builder, Bb_Next);
         PositionBuilderAtEnd (Builder, Bb_Next);
      else
         --  The branch doesn't continue.
         if Block.Bb /= Null_BasicBlockRef then
            --  There is a fall-through (either from the then branch, or
            --  there is no else).
            Unreach := False;
            PositionBuilderAtEnd (Builder, Block.Bb);
         else
            Unreach := True;
         end if;
      end if;
   end Finish_If_Stmt;

   ---------------------
   -- Start_Loop_Stmt --
   ---------------------

   procedure Start_Loop_Stmt (Label : out O_Snode)
   is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if Unreach then
         Label := (Null_BasicBlockRef, Null_BasicBlockRef);
      else
         Label := (Bb_Entry => AppendBasicBlock (Cur_Func, Empty_Cstring),
                   Bb_Exit => AppendBasicBlock (Cur_Func, Empty_Cstring));
         Res := BuildBr (Builder, Label.Bb_Entry);
         PositionBuilderAtEnd (Builder, Label.Bb_Entry);
      end if;
   end Start_Loop_Stmt;

   ----------------------
   -- Finish_Loop_Stmt --
   ----------------------

   procedure Finish_Loop_Stmt (Label : in out O_Snode) is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if not Unreach then
         Res := BuildBr (Builder, Label.Bb_Entry);
      end if;
      if Label.Bb_Exit /= Null_BasicBlockRef then
         --  FIXME: always true...
         PositionBuilderAtEnd (Builder, Label.Bb_Exit);
         Unreach := False;
      else
         Unreach := True;
      end if;
   end Finish_Loop_Stmt;

   -------------------
   -- New_Exit_Stmt --
   -------------------

   procedure New_Exit_Stmt (L : O_Snode) is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if not Unreach then
         Res := BuildBr (Builder, L.Bb_Exit);
         Unreach := True;
      end if;
   end New_Exit_Stmt;

   -------------------
   -- New_Next_Stmt --
   -------------------

   procedure New_Next_Stmt (L : O_Snode) is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if not Unreach then
         Res := BuildBr (Builder, L.Bb_Entry);
         Unreach := True;
      end if;
   end New_Next_Stmt;

   ---------------------
   -- Start_Case_Stmt --
   ---------------------

   procedure Start_Case_Stmt (Block : in out O_Case_Block; Value : O_Enode) is
   begin
      Block := (BB_Prev => GetInsertBlock (Builder),
                Value => Value.LLVM,
                Vtype => Value.Etype,
                BB_Next => Null_BasicBlockRef,
                BB_Others => Null_BasicBlockRef,
                BB_Choice => Null_BasicBlockRef,
                Nbr_Choices => 0,
                Choices => new O_Choice_Array (1 .. 8));
   end Start_Case_Stmt;

   ------------------
   -- Start_Choice --
   ------------------

   procedure Finish_Branch (Block : in out O_Case_Block) is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      --  Close previous branch.
      if not Unreach then
         if Block.BB_Next = Null_BasicBlockRef then
            Block.BB_Next := AppendBasicBlock (Cur_Func, Empty_Cstring);
         end if;
         Res := BuildBr (Builder, Block.BB_Next);
      end if;
   end Finish_Branch;

   procedure Start_Choice (Block : in out O_Case_Block) is
      Res : ValueRef;
      pragma Unreferenced (Res);
   begin
      if Block.BB_Choice /= Null_BasicBlockRef then
         --  Close previous branch.
         Finish_Branch (Block);
      end if;

      Unreach := False;
      Block.BB_Choice := AppendBasicBlock (Cur_Func, Empty_Cstring);
      PositionBuilderAtEnd (Builder, Block.BB_Choice);
   end Start_Choice;

   ---------------------
   -- New_Expr_Choice --
   ---------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (O_Choice_Array, O_Choice_Array_Acc);

   procedure New_Choice (Block : in out O_Case_Block;
                         Low, High : ValueRef)
   is
      Choices : O_Choice_Array_Acc;
   begin
      if Block.Nbr_Choices = Block.Choices'Last then
         Choices := new O_Choice_Array (1 .. Block.Choices'Last * 2);
         Choices (1 .. Block.Choices'Last) := Block.Choices.all;
         Free (Block.Choices);
         Block.Choices := Choices;
      end if;
      Block.Nbr_Choices := Block.Nbr_Choices + 1;
      Block.Choices (Block.Nbr_Choices) := (Low => Low,
                                            High => High,
                                            Bb => Block.BB_Choice);
   end New_Choice;

   procedure New_Expr_Choice (Block : in out O_Case_Block; Expr : O_Cnode) is
   begin
      New_Choice (Block, Expr.LLVM, Null_ValueRef);
   end New_Expr_Choice;

   ----------------------
   -- New_Range_Choice --
   ----------------------

   procedure New_Range_Choice
     (Block : in out O_Case_Block; Low, High : O_Cnode)
   is
   begin
      New_Choice (Block, Low.LLVM, High.LLVM);
   end New_Range_Choice;

   ------------------------
   -- New_Default_Choice --
   ------------------------

   procedure New_Default_Choice (Block : in out O_Case_Block) is
   begin
      Block.BB_Others := Block.BB_Choice;
   end New_Default_Choice;

   -------------------
   -- Finish_Choice --
   -------------------

   procedure Finish_Choice (Block : in out O_Case_Block) is
   begin
      null;
   end Finish_Choice;

   ----------------------
   -- Finish_Case_Stmt --
   ----------------------

   procedure Finish_Case_Stmt (Block : in out O_Case_Block)
   is
      Bb_Default : constant BasicBlockRef :=
        AppendBasicBlock (Cur_Func, Empty_Cstring);
      Bb_Default_Last : BasicBlockRef;
      Nbr_Cases : unsigned := 0;
      GE, LE : IntPredicate;
      Res : ValueRef;
   begin
      if Block.BB_Choice /= Null_BasicBlockRef then
         --  Close previous branch.
         Finish_Branch (Block);
      end if;

      --  Strategy: use a switch instruction for simple choices, put range
      --   choices in the default using if statements.
      case Block.Vtype.Kind is
         when ON_Unsigned_Type
           | ON_Enum_Type
           | ON_Boolean_Type =>
            GE := IntUGE;
            LE := IntULE;
         when ON_Signed_Type =>
            GE := IntSGE;
            LE := IntSLE;
         when others =>
            raise Program_Error;
      end case;

      --  BB for the default case of the LLVM switch.
      PositionBuilderAtEnd (Builder, Bb_Default);
      Bb_Default_Last := Bb_Default;

      for I in 1 .. Block.Nbr_Choices loop
         declare
            C : O_Choice_Type renames Block.Choices (I);
         begin
            if C.High /= Null_ValueRef then
               Bb_Default_Last := AppendBasicBlock (Cur_Func, Empty_Cstring);
               Res := BuildCondBr (Builder,
                                   BuildAnd (Builder,
                                             BuildICmp (Builder, GE,
                                                        Block.Value, C.Low,
                                                        Empty_Cstring),
                                             BuildICmp (Builder, LE,
                                                        Block.Value, C.High,
                                                        Empty_Cstring),
                                             Empty_Cstring),
                                   C.Bb, Bb_Default_Last);
               PositionBuilderAtEnd (Builder, Bb_Default_Last);
            else
               Nbr_Cases := Nbr_Cases + 1;
            end if;
         end;
      end loop;

      --  Insert the switch
      PositionBuilderAtEnd (Builder, Block.BB_Prev);
      Res := BuildSwitch (Builder, Block.Value, Bb_Default, Nbr_Cases);
      for I in 1 .. Block.Nbr_Choices loop
         declare
            C : O_Choice_Type renames Block.Choices (I);
         begin
            if C.High = Null_ValueRef then
               AddCase (Res, C.Low, C.Bb);
            end if;
         end;
      end loop;

      --  Insert the others.
      PositionBuilderAtEnd (Builder, Bb_Default_Last);
      if Block.BB_Others /= Null_BasicBlockRef then
         Res := BuildBr (Builder, Block.BB_Others);
      else
         Res := BuildUnreachable (Builder);
      end if;

      if Block.BB_Next /= Null_BasicBlockRef then
         Unreach := False;
         PositionBuilderAtEnd (Builder, Block.BB_Next);
      else
         Unreach := True;
      end if;

      Free (Block.Choices);
   end Finish_Case_Stmt;

   function Get_LLVM_Type (Atype : O_Tnode) return TypeRef is
   begin
      case Atype.Kind is
         when ON_Incomplete_Record_Type
           | ON_Incomplete_Access_Type =>
            if Atype.LLVM = Null_TypeRef then
               raise Program_Error with "early use of incomplete type";
            end if;
            return Atype.LLVM;
         when ON_Union_Type
           | ON_Scalar_Types
           | ON_Access_Type
           | ON_Array_Type
           | ON_Array_Sub_Type
           | ON_Record_Type =>
            return Atype.LLVM;
         when others =>
            raise Program_Error;
      end case;
   end Get_LLVM_Type;

   function CreateAttribute (Name : String; Val : Unsigned_64 := 0)
                            return AttributeRef
   is
      Id : unsigned;
   begin
      Id := GetEnumAttributeKindForName (Name'Address, Name'Length);
      if Id = 0 then
         --  Unknown attribute
         raise Program_Error;
      end if;
      return CreateEnumAttribute (GetGlobalContext, Id, Val);
   end CreateAttribute;

   procedure Init is
      --  Some predefined types and functions.
      I8_Ptr_Type : TypeRef;
   begin
      Builder := CreateBuilder;
      Decl_Builder := CreateBuilder;
      Extra_Builder := CreateBuilder;

      --  Create type i8 *.
      I8_Ptr_Type := PointerType (Int8Type);

      --  Create intrinsic 'i8 *stacksave (void)'.
      Stacksave_Fun := AddFunction
        (Module, Stacksave_Name'Address,
         FunctionType (I8_Ptr_Type, (1 .. 0 => Null_TypeRef), 0, 0));

      --  Create intrinsic 'void stackrestore (i8 *)'.
      Stackrestore_Fun := AddFunction
        (Module, Stackrestore_Name'Address,
         FunctionType (VoidType, (1 => I8_Ptr_Type), 1, 0));

      --  Create intrinsic 'double llvm.copysign.f64 (double, double)'.
      Copysign_Fun := AddFunction
        (Module, Copysign_Name'Address,
         FunctionType (DoubleType, (0 .. 1 => DoubleType), 2, 0));

      Fp_0_5 := ConstReal (DoubleType, 0.5);

      Nounwind_Attr := CreateAttribute ("nounwind");
      Uwtable_Attr := CreateAttribute ("uwtable");
   end Init;

end Ortho_LLVM;
