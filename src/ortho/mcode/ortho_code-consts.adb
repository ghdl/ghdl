--  Mcode back-end for ortho - Constants handling.
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
with Ada.Unchecked_Conversion;
with Tables;
with Ada.Text_IO;
with Ortho_Code.Types; use Ortho_Code.Types;
with Ortho_Code.Debug;

package body Ortho_Code.Consts is
   type Cnode_Common is record
      Kind : OC_Kind;
      Lit_Type : O_Tnode;
   end record;
   for Cnode_Common use record
      Kind at 0 range 0 .. 31;
      Lit_Type at 4 range 0 .. 31;
   end record;
   for Cnode_Common'Size use 64;

   type Cnode_Signed is record
      Val : Integer_64;
   end record;
   for Cnode_Signed'Size use 64;

   type Cnode_Unsigned is record
      Val : Unsigned_64;
   end record;
   for Cnode_Unsigned'Size use 64;

   type Cnode_Float is record
      Val : IEEE_Float_64;
   end record;
   for Cnode_Float'Size use 64;

   type Cnode_Enum is record
      Id : O_Ident;
      Val : Uns32;
   end record;
   for Cnode_Enum'Size use 64;

   type Cnode_Addr is record
      Decl : O_Dnode;
      Pad : Int32;
   end record;
   for Cnode_Addr'Size use 64;

   type Cnode_Aggr is record
      Els : Int32;
      Nbr : Int32;
   end record;
   for Cnode_Aggr'Size use 64;

   type Cnode_Sizeof is record
      Atype : O_Tnode;
      Pad : Int32;
   end record;
   for Cnode_Sizeof'Size use 64;

   type Cnode_Union is record
      El : O_Cnode;
      Field : O_Fnode;
   end record;
   for Cnode_Union'Size use 64;

   package Cnodes is new Tables
     (Table_Component_Type => Cnode_Common,
      Table_Index_Type => O_Cnode,
      Table_Low_Bound => 2,
      Table_Initial => 128);

   function Get_Const_Kind (Cst : O_Cnode) return OC_Kind is
   begin
      return Cnodes.Table (Cst).Kind;
   end Get_Const_Kind;

   function Get_Const_Type (Cst : O_Cnode) return O_Tnode is
   begin
      return Cnodes.Table (Cst).Lit_Type;
   end Get_Const_Type;

   function Get_Const_U64 (Cst : O_Cnode) return Unsigned_64
   is
      function To_Cnode_Unsigned is new Ada.Unchecked_Conversion
        (Cnode_Common, Cnode_Unsigned);
   begin
      return To_Cnode_Unsigned (Cnodes.Table (Cst + 1)).Val;
   end Get_Const_U64;

   function Get_Const_I64 (Cst : O_Cnode) return Integer_64
   is
      function To_Cnode_Signed is new Ada.Unchecked_Conversion
        (Cnode_Common, Cnode_Signed);
   begin
      return To_Cnode_Signed (Cnodes.Table (Cst + 1)).Val;
   end Get_Const_I64;

   function Get_Const_F64 (Cst : O_Cnode) return IEEE_Float_64
   is
      function To_Cnode_Float is new Ada.Unchecked_Conversion
        (Cnode_Common, Cnode_Float);
   begin
      return To_Cnode_Float (Cnodes.Table (Cst + 1)).Val;
   end Get_Const_F64;

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Cnode_Signed, Target => Cnode_Common);

   function New_Signed_Literal (Ltype : O_Tnode; Value : Integer_64)
                               return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Signed,
                                   Lit_Type => Ltype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Signed'(Val => Value)));
      return Res;
   end New_Signed_Literal;

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Unsigned_64, Target => Cnode_Common);

   function New_Unsigned_Literal (Ltype : O_Tnode; Value : Unsigned_64)
                                 return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Unsigned,
                                   Lit_Type => Ltype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Value));
      return Res;
   end New_Unsigned_Literal;

--    function Get_Const_Literal (Cst : O_Cnode) return Uns32 is
--    begin
--       return Cnodes.Table (Cst).Val;
--    end Get_Const_Literal;

   function To_Uns64 is new Ada.Unchecked_Conversion
     (Source => Cnode_Common, Target => Uns64);

   function Get_Const_U32 (Cst : O_Cnode) return Uns32 is
   begin
      return Uns32 (To_Uns64 (Cnodes.Table (Cst + 1)));
   end Get_Const_U32;

   function Get_Const_R64 (Cst : O_Cnode) return Uns64 is
   begin
      return To_Uns64 (Cnodes.Table (Cst + 1));
   end Get_Const_R64;

   function Get_Const_Low (Cst : O_Cnode) return Uns32
   is
      V : Uns64;
   begin
      V := Get_Const_R64 (Cst);
      return Uns32 (V and 16#Ffff_Ffff#);
   end Get_Const_Low;

   function Get_Const_High (Cst : O_Cnode) return Uns32
   is
      V : Uns64;
   begin
      V := Get_Const_R64 (Cst);
      return Uns32 (Shift_Right (V, 32) and 16#Ffff_Ffff#);
   end Get_Const_High;

   function Get_Const_Low (Cst : O_Cnode) return Int32
   is
      V : Uns64;
   begin
      V := Get_Const_R64 (Cst);
      return To_Int32 (Uns32 (V and 16#Ffff_Ffff#));
   end Get_Const_Low;

   function Get_Const_High (Cst : O_Cnode) return Int32
   is
      V : Uns64;
   begin
      V := Get_Const_R64 (Cst);
      return To_Int32 (Uns32 (Shift_Right (V, 32) and 16#Ffff_Ffff#));
   end Get_Const_High;

   function New_Float_Literal (Ltype : O_Tnode; Value : IEEE_Float_64)
                              return O_Cnode
   is
      Res : O_Cnode;

      function To_Cnode_Common is new Ada.Unchecked_Conversion
        (Source => Cnode_Float, Target => Cnode_Common);
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Float,
                                   Lit_Type => Ltype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Float'(Val => Value)));
      return Res;
   end New_Float_Literal;

   function New_Null_Access (Ltype : O_Tnode) return O_Cnode is
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Null,
                                   Lit_Type => Ltype));
      return Cnodes.Last;
   end New_Null_Access;

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Cnode_Addr, Target => Cnode_Common);

   function To_Cnode_Addr is new Ada.Unchecked_Conversion
     (Source => Cnode_Common, Target => Cnode_Addr);

   function New_Global_Unchecked_Address (Decl : O_Dnode; Atype : O_Tnode)
                                         return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Address,
                                   Lit_Type => Atype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Addr'(Decl => Decl,
                                                  Pad => 0)));
      return Res;
   end New_Global_Unchecked_Address;

   function New_Global_Address (Decl : O_Dnode; Atype : O_Tnode)
                               return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Address,
                                   Lit_Type => Atype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Addr'(Decl => Decl,
                                                  Pad => 0)));
      return Res;
   end New_Global_Address;

   function New_Subprogram_Address (Subprg : O_Dnode; Atype : O_Tnode)
                                   return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Subprg_Address,
                                   Lit_Type => Atype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Addr'(Decl => Subprg,
                                                  Pad => 0)));
      return Res;
   end New_Subprogram_Address;

   function Get_Const_Decl (Cst : O_Cnode) return O_Dnode is
   begin
      return To_Cnode_Addr (Cnodes.Table (Cst + 1)).Decl;
   end Get_Const_Decl;

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Cnode_Enum, Target => Cnode_Common);

   function To_Cnode_Enum is new Ada.Unchecked_Conversion
     (Source => Cnode_Common, Target => Cnode_Enum);

   --function Get_Named_Literal_Id (Lit : O_Cnode) return O_Ident is
   --begin
   --   return To_Cnode_Enum (Cnodes.Table (Lit + 1)).Id;
   --end Get_Named_Literal_Id;

   function New_Named_Literal
     (Atype : O_Tnode; Id : O_Ident; Val : Uns32; Prev : O_Cnode)
     return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Lit,
                                   Lit_Type => Atype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Enum'(Id => Id,
                                                  Val => Val)));
      if Prev /= O_Cnode_Null then
         if Prev + 2 /= Res then
            raise Syntax_Error;
         end if;
      end if;
      return Res;
   end New_Named_Literal;

   function Get_Lit_Ident (L : O_Cnode) return O_Ident is
   begin
      return To_Cnode_Enum (Cnodes.Table (L + 1)).Id;
   end Get_Lit_Ident;

   function Get_Lit_Value (L : O_Cnode) return Uns32 is
   begin
      return To_Cnode_Enum (Cnodes.Table (L + 1)).Val;
   end Get_Lit_Value;

   function Get_Lit_Chain (L : O_Cnode) return O_Cnode is
   begin
      return L + 2;
   end Get_Lit_Chain;

   package Els is new Tables
     (Table_Component_Type => O_Cnode,
      Table_Index_Type => Int32,
      Table_Low_Bound => 2,
      Table_Initial => 128);

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Cnode_Aggr, Target => Cnode_Common);

   function To_Cnode_Aggr is new Ada.Unchecked_Conversion
     (Source => Cnode_Common, Target => Cnode_Aggr);


   procedure Start_Record_Aggr (List : out O_Record_Aggr_List;
                                Atype : O_Tnode)
   is
      Val : Int32;
      Num : Uns32;
   begin
      Num := Get_Type_Record_Nbr_Fields (Atype);
      Val := Els.Allocate (Integer (Num));

      Cnodes.Append (Cnode_Common'(Kind => OC_Record,
                                   Lit_Type => Atype));
      List := (Res => Cnodes.Last,
               Rec_Field => Get_Type_Record_Fields (Atype),
               El => Val);
      Cnodes.Append (To_Cnode_Common (Cnode_Aggr'(Els => Val,
                                                  Nbr => Int32 (Num))));
   end Start_Record_Aggr;


   procedure New_Record_Aggr_El (List : in out O_Record_Aggr_List;
                                 Value : O_Cnode)
   is
   begin
      Els.Table (List.El) := Value;
      List.El := List.El + 1;
   end New_Record_Aggr_El;

   procedure Finish_Record_Aggr (List : in out O_Record_Aggr_List;
                                 Res : out O_Cnode) is
   begin
      Res := List.Res;
   end Finish_Record_Aggr;


   procedure Start_Array_Aggr (List : out O_Array_Aggr_List; Atype : O_Tnode)
   is
      Val : Int32;
      Num : Uns32;
   begin
      Num := Get_Type_Subarray_Length (Atype);
      Val := Els.Allocate (Integer (Num));

      Cnodes.Append (Cnode_Common'(Kind => OC_Array,
                                   Lit_Type => Atype));
      List := (Res => Cnodes.Last,
               El => Val);
      Cnodes.Append (To_Cnode_Common (Cnode_Aggr'(Els => Val,
                                                  Nbr => Int32 (Num))));
   end Start_Array_Aggr;

   procedure New_Array_Aggr_El (List : in out O_Array_Aggr_List;
                                Value : O_Cnode)
   is
   begin
      Els.Table (List.El) := Value;
      List.El := List.El + 1;
   end New_Array_Aggr_El;

   procedure Finish_Array_Aggr (List : in out O_Array_Aggr_List;
                                Res : out O_Cnode)
   is
   begin
      Res := List.Res;
   end Finish_Array_Aggr;

   function Get_Const_Aggr_Length (Cst : O_Cnode) return Int32 is
   begin
      return To_Cnode_Aggr (Cnodes.Table (Cst + 1)).Nbr;
   end Get_Const_Aggr_Length;

   function Get_Const_Aggr_Element (Cst : O_Cnode; N : Int32) return O_Cnode
   is
      El : Int32;
   begin
      El := To_Cnode_Aggr (Cnodes.Table (Cst + 1)).Els;
      return Els.Table (El + N);
   end Get_Const_Aggr_Element;

   function New_Union_Aggr (Atype : O_Tnode; Field : O_Fnode; Value : O_Cnode)
                           return O_Cnode
   is
      function To_Cnode_Common is new Ada.Unchecked_Conversion
        (Source => Cnode_Union, Target => Cnode_Common);

      Res : O_Cnode;
   begin
      if Debug.Flag_Debug_Hli then
         Cnodes.Append (Cnode_Common'(Kind => OC_Union,
                                      Lit_Type => Atype));
         Res := Cnodes.Last;
         Cnodes.Append (To_Cnode_Common (Cnode_Union'(El => Value,
                                                      Field => Field)));
         return Res;
      else
         return Value;
      end if;
   end New_Union_Aggr;

   function To_Cnode_Union is new Ada.Unchecked_Conversion
        (Source => Cnode_Common, Target => Cnode_Union);

   function Get_Const_Union_Field (Cst : O_Cnode) return O_Fnode is
   begin
      return To_Cnode_Union (Cnodes.Table (Cst + 1)).Field;
   end Get_Const_Union_Field;

   function Get_Const_Union_Value (Cst : O_Cnode) return O_Cnode is
   begin
      return To_Cnode_Union (Cnodes.Table (Cst + 1)).El;
   end Get_Const_Union_Value;

   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode
   is
      function To_Cnode_Common is new Ada.Unchecked_Conversion
        (Source => Cnode_Sizeof, Target => Cnode_Common);

      Res : O_Cnode;
   begin
      if Debug.Flag_Debug_Hli then
         Cnodes.Append (Cnode_Common'(Kind => OC_Sizeof,
                                      Lit_Type => Rtype));
         Res := Cnodes.Last;
         Cnodes.Append (To_Cnode_Common (Cnode_Sizeof'(Atype => Atype,
                                                       Pad => 0)));
         return Res;
      else
         return New_Unsigned_Literal
           (Rtype, Unsigned_64 (Get_Type_Size (Atype)));
      end if;
   end New_Sizeof;

   function Get_Sizeof_Type (Cst : O_Cnode) return O_Tnode
   is
      function To_Cnode_Sizeof is new Ada.Unchecked_Conversion
        (Cnode_Common, Cnode_Sizeof);
   begin
      return To_Cnode_Sizeof (Cnodes.Table (Cst + 1)).Atype;
   end Get_Sizeof_Type;

   function New_Alignof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode
   is
      function To_Cnode_Common is new Ada.Unchecked_Conversion
        (Source => Cnode_Sizeof, Target => Cnode_Common);

      Res : O_Cnode;
   begin
      if Debug.Flag_Debug_Hli then
         Cnodes.Append (Cnode_Common'(Kind => OC_Alignof,
                                      Lit_Type => Rtype));
         Res := Cnodes.Last;
         Cnodes.Append (To_Cnode_Common (Cnode_Sizeof'(Atype => Atype,
                                                       Pad => 0)));
         return Res;
      else
         return New_Unsigned_Literal
           (Rtype, Unsigned_64 (Get_Type_Align_Bytes (Atype)));
      end if;
   end New_Alignof;

   function Get_Alignof_Type (Cst : O_Cnode) return O_Tnode
   is
      function To_Cnode_Sizeof is new Ada.Unchecked_Conversion
        (Cnode_Common, Cnode_Sizeof);
   begin
      return To_Cnode_Sizeof (Cnodes.Table (Cst + 1)).Atype;
   end Get_Alignof_Type;

   function New_Offsetof (Rec_Type : O_Tnode; Field : O_Fnode; Rtype : O_Tnode)
                         return O_Cnode is
   begin
      if Get_Field_Parent (Field) /= Rec_Type then
         raise Syntax_Error;
      end if;
      return New_Unsigned_Literal
        (Rtype, Unsigned_64 (Get_Field_Offset (Field)));
   end New_Offsetof;

   procedure Get_Const_Bytes (Cst : O_Cnode; H, L : out Uns32) is
   begin
      case Get_Const_Kind (Cst) is
         when OC_Signed
           | OC_Unsigned
           | OC_Float =>
            H := Get_Const_High (Cst);
            L := Get_Const_Low (Cst);
         when OC_Null =>
            H := 0;
            L := 0;
         when OC_Lit =>
            H := 0;
            L := To_Cnode_Enum (Cnodes.Table (Cst + 1)).Val;
         when OC_Array
           | OC_Record
           | OC_Union
           | OC_Sizeof
           | OC_Alignof
           | OC_Address
           | OC_Subprg_Address =>
            raise Syntax_Error;
      end case;
   end Get_Const_Bytes;

   procedure Mark (M : out Mark_Type) is
   begin
      M.Cnode := Cnodes.Last;
      M.Els := Els.Last;
   end Mark;

   procedure Release (M : Mark_Type) is
   begin
      Cnodes.Set_Last (M.Cnode);
      Els.Set_Last (M.Els);
   end Release;

   procedure Disp_Stats
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Number of Cnodes: " & O_Cnode'Image (Cnodes.Last));
      Put_Line ("Number of Cnodes-Els: " & Int32'Image (Els.Last));
   end Disp_Stats;

   procedure Finish is
   begin
      Cnodes.Free;
      Els.Free;
   end Finish;
end Ortho_Code.Consts;
