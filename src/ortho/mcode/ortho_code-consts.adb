--  Mcode back-end for ortho - Constants handling.
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
with Tables;
with Ada.Text_IO;
with Ortho_Code.Types; use Ortho_Code.Types;
with Ortho_Code.Decls;
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

   type Cnode_Global is record
      Obj : O_Gnode;
      Pad : Int32;
   end record;
   for Cnode_Global'Size use 64;

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

   type Gnode_Common is record
      Kind : OG_Kind;
      Ref : Int32;
   end record;
   for Gnode_Common use record
      Kind at 0 range 0 .. 31;
      Ref at 4 range 0 .. 31;
   end record;
   for Gnode_Common'Size use 64;

   type Gnode_Record_Ref is record
      Field : O_Fnode;
      Off : Uns32;
   end record;
   for Gnode_Record_Ref'Size use 64;

   function To_Gnode_Common is new Ada.Unchecked_Conversion
     (Gnode_Record_Ref, Gnode_Common);
   function To_Gnode_Record_Ref is new Ada.Unchecked_Conversion
     (Gnode_Common, Gnode_Record_Ref);

   package Gnodes is new Tables
     (Table_Component_Type => Gnode_Common,
      Table_Index_Type => O_Gnode,
      Table_Low_Bound => 2,
      Table_Initial => 64);

   function Get_Const_Kind (Cst : O_Cnode) return OC_Kind is
   begin
      return Cnodes.Table (Cst).Kind;
   end Get_Const_Kind;

   function Get_Global_Kind (Cst : O_Gnode) return OG_Kind is
   begin
      return Gnodes.Table (Cst).Kind;
   end Get_Global_Kind;

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

   function New_Default_Value (Ltype : O_Tnode) return O_Cnode is
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Zero,
                                   Lit_Type => Ltype));
      return Cnodes.Last;
   end New_Default_Value;

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Cnode_Global, Target => Cnode_Common);

   function To_Cnode_Global is new Ada.Unchecked_Conversion
     (Source => Cnode_Common, Target => Cnode_Global);

   function New_Global_Unchecked_Address (Lvalue : O_Gnode; Atype : O_Tnode)
                                         return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Address,
                                   Lit_Type => Atype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Global'(Obj => Lvalue,
                                                    Pad => 0)));
      return Res;
   end New_Global_Unchecked_Address;

   function New_Global_Address (Lvalue : O_Gnode; Atype : O_Tnode)
                               return O_Cnode
   is
      Res : O_Cnode;
   begin
      Cnodes.Append (Cnode_Common'(Kind => OC_Address,
                                   Lit_Type => Atype));
      Res := Cnodes.Last;
      Cnodes.Append (To_Cnode_Common (Cnode_Global'(Obj => Lvalue,
                                                    Pad => 0)));
      return Res;
   end New_Global_Address;

   function Get_Const_Global (Cst : O_Cnode) return O_Gnode is
   begin
      pragma Assert (Get_Const_Kind (Cst) = OC_Address);
      return To_Cnode_Global (Cnodes.Table (Cst + 1)).Obj;
   end Get_Const_Global;

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Cnode_Addr, Target => Cnode_Common);

   function To_Cnode_Addr is new Ada.Unchecked_Conversion
     (Source => Cnode_Common, Target => Cnode_Addr);

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
      pragma Assert (Get_Const_Kind (Cst) = OC_Subprg_Address);
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


   procedure Start_Array_Aggr
     (List : out O_Array_Aggr_List; Arr_Type : O_Tnode; Len : Unsigned_32)
   is
      Val : Int32;
   begin
      case Get_Type_Kind (Arr_Type) is
         when OT_Subarray =>
            pragma Assert (Uns32 (Len) = Get_Type_Subarray_Length (Arr_Type));
         when OT_Ucarray =>
            null;
         when others =>
            --  The type of an array aggregate must be an array type.
            raise Syntax_Error;
      end case;
      Val := Els.Allocate (Integer (Len));

      Cnodes.Append (Cnode_Common'(Kind => OC_Array,
                                   Lit_Type => Arr_Type));
      List := (Res => Cnodes.Last,
               El => Val,
               Len => Uns32 (Len));
      Cnodes.Append (To_Cnode_Common (Cnode_Aggr'(Els => Val,
                                                  Nbr => Int32 (Len))));
   end Start_Array_Aggr;

   procedure New_Array_Aggr_El (List : in out O_Array_Aggr_List;
                                Value : O_Cnode) is
   begin
      pragma Assert (List.Len > 0);
      List.Len := List.Len - 1;
      Els.Table (List.El) := Value;
      List.El := List.El + 1;
   end New_Array_Aggr_El;

   procedure Finish_Array_Aggr (List : in out O_Array_Aggr_List;
                                Res : out O_Cnode) is
   begin
      pragma Assert (List.Len = 0);
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

   function To_Cnode_Common is new Ada.Unchecked_Conversion
     (Source => Cnode_Sizeof, Target => Cnode_Common);

   function New_Sizeof (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode
   is
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

   function New_Record_Sizeof
     (Atype : O_Tnode; Rtype : O_Tnode) return O_Cnode
   is
      Res : O_Cnode;
   begin
      if Debug.Flag_Debug_Hli then
         Cnodes.Append (Cnode_Common'(Kind => OC_Record_Sizeof,
                                      Lit_Type => Rtype));
         Res := Cnodes.Last;
         Cnodes.Append (To_Cnode_Common (Cnode_Sizeof'(Atype => Atype,
                                                       Pad => 0)));
         return Res;
      else
         return New_Unsigned_Literal
           (Rtype, Unsigned_64 (Get_Type_Record_Size (Atype)));
      end if;
   end New_Record_Sizeof;

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

   function Get_Global_Decl (Global : O_Gnode) return O_Dnode is
   begin
      pragma Assert (Get_Global_Kind (Global) = OG_Decl);
      return O_Dnode (Gnodes.Table (Global).Ref);
   end Get_Global_Decl;

   function Get_Global_Field (Global : O_Gnode) return O_Fnode is
   begin
      pragma Assert (Get_Global_Kind (Global) = OG_Record_Ref);
      return To_Gnode_Record_Ref (Gnodes.Table (Global + 1)).Field;
   end Get_Global_Field;

   function Get_Global_Ref (Global : O_Gnode) return O_Gnode is
   begin
      pragma Assert (Get_Global_Kind (Global) = OG_Record_Ref);
      return O_Gnode (Gnodes.Table (Global).Ref);
   end Get_Global_Ref;

   function Get_Global_Type (Global : O_Gnode) return O_Tnode is
   begin
      case Get_Global_Kind (Global) is
         when OG_Decl =>
            return Decls.Get_Decl_Type (Get_Global_Decl (Global));
         when OG_Record_Ref =>
            return Get_Field_Type (Get_Global_Field (Global));
      end case;
   end Get_Global_Type;

   function New_Global (Decl : O_Dnode) return O_Gnode is
   begin
      Gnodes.Append (Gnode_Common'(Kind => OG_Decl,
                                   Ref => Int32 (Decl)));
      return Gnodes.Last;
   end New_Global;

   function New_Global_Selected_Element (Rec : O_Gnode; El : O_Fnode)
                                        return O_Gnode
   is
      Res : O_Gnode;
   begin
      --  TODO: Check Ref.

      --  Check type.
      pragma Assert
        (Get_Type_Kind (Get_Global_Type (Rec)) in OT_Kinds_Record_Union);

      Gnodes.Append (Gnode_Common'(Kind => OG_Record_Ref,
                                   Ref => Int32 (Rec)));
      Res := Gnodes.Last;
      Gnodes.Append (To_Gnode_Common
                       (Gnode_Record_Ref'(Field => El,
                                          Off => Get_Field_Offset (El))));
      return Res;
   end New_Global_Selected_Element;

   procedure Get_Global_Decl_Offset (Global : O_Gnode;
                                     Decl : out O_Dnode; Off : out Uns32) is
   begin
      case Get_Global_Kind (Global) is
         when OG_Decl =>
            Decl := Get_Global_Decl (Global);
            Off := 0;
         when OG_Record_Ref =>
            Get_Global_Decl_Offset (Get_Global_Ref (Global), Decl, Off);
            Off := Off + Get_Field_Offset (Get_Global_Field (Global));
      end case;
   end Get_Global_Decl_Offset;

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
            | OC_Record_Sizeof
            | OC_Alignof
            | OC_Address
            | OC_Subprg_Address
            | OC_Zero =>
            raise Syntax_Error;
      end case;
   end Get_Const_Bytes;

   function Get_Const_Size (Cst : O_Cnode) return Uns32
   is
      T : constant O_Tnode := Get_Const_Type (Cst);
   begin
      case Get_Type_Kind (T) is
         when OT_Ucarray =>
            declare
               Len : constant Int32 := Get_Const_Aggr_Length (Cst);
               El_Sz : Uns32;
            begin
               if Len = 0 then
                  return 0;
               end if;
               El_Sz := Get_Const_Size (Get_Const_Aggr_Element (Cst, 0));
               return Uns32 (Len) * El_Sz;
            end;
         when others =>
            return Get_Type_Size (T);
      end case;
   end Get_Const_Size;

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
