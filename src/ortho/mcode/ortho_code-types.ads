--  Mcode back-end for ortho - type handling.
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
package Ortho_Code.Types is
   type OT_Kind is (OT_Unsigned, OT_Signed, OT_Boolean, OT_Enum, OT_Float,
                    OT_Ucarray, OT_Subarray,
                    OT_Access,
                    OT_Record, OT_Subrecord,
                    OT_Union,

                    --  Type completion.  Mark the completion of a type.
                    --  Optionnal.
                    OT_Complete);

   subtype OT_Kinds_Record_Union is OT_Kind range OT_Record .. OT_Union;

   --  Kind of ATYPE.
   function Get_Type_Kind (Atype : O_Tnode) return OT_Kind;

   --  Number of bytes of type ATYPE.
   function Get_Type_Size (Atype : O_Tnode) return Uns32;

   --  True if ATYPE is bounded (and therefore its size is valid).
   function Get_Type_Sized (Atype : O_Tnode) return Boolean;

   --  Same as Get_Type_Size but for modes.
   --  Returns 0 in case of error.
   function Get_Mode_Size (Mode : Mode_Type) return Uns32;

   --  Alignment for ATYPE, in power of 2.
   subtype Small_Natural is Natural range 0 .. 3;
   type Mode_Align_Array is array (Mode_Type) of Small_Natural;
   function Get_Type_Align (Atype : O_Tnode) return Small_Natural;

   --  Alignment for ATYPE in bytes.
   function Get_Type_Align_Bytes (Atype : O_Tnode) return Uns32;

   --  Return true is the type was incomplete at creation.
   --  (it may - or not - have been completed later).
   function Get_Type_Deferred (Atype : O_Tnode) return Boolean;

   --  A back-end reserved flag.
   --  Initialized to False.
   function Get_Type_Flag1 (Atype : O_Tnode) return Boolean;
   procedure Set_Type_Flag1 (Atype : O_Tnode; Flag : Boolean);

   --  Align OFF on ATYPE.
   function Do_Align (Off : Uns32; Atype : O_Tnode) return Uns32;
   function Do_Align (Off : Uns32; Mode : Mode_Type) return Uns32;

   --  Get the mode for ATYPE.
   function Get_Type_Mode (Atype : O_Tnode) return Mode_Type;

   --  Get the type designated by access type ATYPE.
   function Get_Type_Access_Type (Atype : O_Tnode) return O_Tnode;

   --  Get the index type of array type ATYPE.
   function Get_Type_Ucarray_Index (Atype : O_Tnode) return O_Tnode;

   --  Get the element type of array type ATYPE.
   function Get_Type_Ucarray_Element (Atype : O_Tnode) return O_Tnode;

   --  Get the base type of array type ATYPE.
   function Get_Type_Subarray_Base (Atype : O_Tnode) return O_Tnode;

   --  Get number of element for array type ATYPE.
   function Get_Type_Subarray_Length (Atype : O_Tnode) return Uns32;

   --  Get the element type of subarray type ATYPE.
   function Get_Type_Subarray_Element (Atype : O_Tnode) return O_Tnode;

   --  Get the size of the bounded part of ATYPE.
   function Get_Type_Record_Size (Atype : O_Tnode) return Uns32;

   --  Get the first field of record/union ATYPE.
   function Get_Type_Record_Fields (Atype : O_Tnode) return O_Fnode;

   --  Get the number of fields of record/union ATYPE.
   function Get_Type_Record_Nbr_Fields (Atype : O_Tnode) return Uns32;

   --  Get the base type of subrecord ATYPE.
   function Get_Type_Subrecord_Base (Atype : O_Tnode) return O_Tnode;

   --  Get the first literal of enum type ATYPE.
   function Get_Type_Enum_Lits (Atype : O_Tnode) return O_Cnode;

   --  Get the POS th literal of enum type ATYPE.
   --  The first is when POS = 0.
   function Get_Type_Enum_Lit (Atype : O_Tnode; Pos : Uns32) return O_Cnode;

   --  Get the number of literals of enum type ATYPE.
   function Get_Type_Enum_Nbr_Lits (Atype : O_Tnode) return Uns32;

   --  Get the false/true literal of boolean type ATYPE.
   function Get_Type_Bool_False (Atype : O_Tnode) return O_Cnode;
   function Get_Type_Bool_True (Atype : O_Tnode) return O_Cnode;

   --  Return the union/record type which contains FIELD.
   function Get_Field_Parent (Field : O_Fnode) return O_Tnode;

   --  Get the offset of FIELD in its record/union.
   function Get_Field_Offset (Field : O_Fnode) return Uns32;
   procedure Set_Field_Offset (Field : O_Fnode; Offset : Uns32);

   --  Get the type of FIELD.
   function Get_Field_Type (Field : O_Fnode) return O_Tnode;

   --  Get the name of FIELD.
   function Get_Field_Ident (Field : O_Fnode) return O_Ident;

   --  Get the next field.
   function Get_Field_Chain (Field : O_Fnode) return O_Fnode;

   --  Get the type that was completed.
   function Get_Type_Complete_Type (Atype : O_Tnode) return O_Tnode;

   --  Build a scalar type; size may be 8, 16, 32 or 64.
   function New_Unsigned_Type (Size : Natural) return O_Tnode;
   function New_Signed_Type (Size : Natural) return O_Tnode;

   --  Build a float type.
   function New_Float_Type return O_Tnode;

   --  Build a boolean type.
   procedure New_Boolean_Type (Res : out O_Tnode;
                               False_Id : O_Ident;
                               False_E : out O_Cnode;
                               True_Id : O_Ident;
                               True_E : out O_Cnode);

   --  Create an enumeration
   type O_Enum_List is limited private;

   --  Elements are declared in order, the first is ordered from 0.
   procedure Start_Enum_Type (List : out O_Enum_List; Size : Natural);
   procedure New_Enum_Literal (List : in out O_Enum_List;
                               Ident : O_Ident; Res : out O_Cnode);
   procedure Finish_Enum_Type (List : in out O_Enum_List; Res : out O_Tnode);


   --  Build an access type.
   --  DTYPE may be O_tnode_null in order to build an incomplete access type.
   --  It is completed with finish_access_type.
   function New_Access_Type (Dtype : O_Tnode) return O_Tnode;
   procedure Finish_Access_Type (Atype : O_Tnode; Dtype : O_Tnode);


   --  Build an array type.
   --  The array is not constrained and unidimensional.
   function New_Array_Type (El_Type : O_Tnode; Index_Type : O_Tnode)
                            return O_Tnode;

   --  Build a constrained array type.
   function New_Array_Subtype
     (Atype : O_Tnode; El_Type : O_Tnode; Length : Uns32) return O_Tnode;

   --  Return the base type of ATYPE: for a subarray this is the uc array,
   --  otherwise this is the type.
   function Get_Base_Type (Atype : O_Tnode) return O_Tnode;

   type O_Element_List is limited private;

   --  Build a record type.
   procedure Start_Record_Type (Elements : out O_Element_List);
   --  Add a field in the record; not constrained array are prohibited, since
   --  its size is unlimited.
   procedure New_Record_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident; Etype : O_Tnode);
   --  Finish the record type.
   procedure Finish_Record_Type
     (Elements : in out O_Element_List; Res : out O_Tnode);

   --  Record subtype.
   procedure Start_Record_Subtype
     (Rtype : O_Tnode; Elements : out O_Element_List);
   procedure New_Subrecord_Field
     (Elements : in out O_Element_List; El : out O_Fnode; Etype : O_Tnode);
   procedure Finish_Record_Subtype
     (Elements : in out O_Element_List; Res : out O_Tnode);

   -- Build an uncomplete record type:
   -- First call NEW_UNCOMPLETE_RECORD_TYPE, which returns a record type.
   -- This type can be declared or used to define access types on it.
   -- Then, complete (if necessary) the record type, by calling
   -- START_UNCOMPLETE_RECORD_TYPE, NEW_RECORD_FIELD and FINISH_RECORD_TYPE.
   procedure New_Uncomplete_Record_Type (Res : out O_Tnode);
   procedure Start_Uncomplete_Record_Type (Res : O_Tnode;
                                           Elements : out O_Element_List);

   --  Build an union type.
   procedure Start_Union_Type (Elements : out O_Element_List);
   procedure New_Union_Field
     (Elements : in out O_Element_List;
      El : out O_Fnode;
      Ident : O_Ident;
      Etype : O_Tnode);
   procedure Finish_Union_Type
     (Elements : in out O_Element_List; Res : out O_Tnode);

   --  Non-primitives.

   --  Type of an element of a ucarray or constrained array.
   function Get_Type_Array_Element (Atype : O_Tnode) return O_Tnode;

   --  Get a type number limit (an O_Tnode is a number).
   --  There is no type whose number is beyond this limit.
   --  Note: the limit may not be a type!
   function Get_Type_Limit return O_Tnode;

   --  Get the type which follows ATYPE.
   --  User has to check that the result is valid (ie not beyond limit).
   function Get_Type_Next (Atype : O_Tnode) return O_Tnode;

   procedure Disp_Stats;

   --  Free all the memory used.
   procedure Finish;

   type Mark_Type is limited private;
   procedure Mark (M : out Mark_Type);
   procedure Release (M : Mark_Type);

   procedure Dump_Tnode (Atype : O_Tnode);
   procedure Dump_Fnode (Field : O_Fnode);
private
   type O_Enum_List is record
      Res   : O_Tnode;
      First : O_Cnode;
      Last  : O_Cnode;
      Nbr   : Uns32;
   end record;

   type O_Element_List is record
      Res         : O_Tnode;
      Nbr         : Uns32;
      Off         : Uns32;
      Align       : Small_Natural;
      First_Field : O_Fnode;
      Last_Field  : O_Fnode;
      --  For subrecords
      Base_Field  : O_Fnode;
   end record;

   type Mark_Type is record
      Tnode : O_Tnode;
      Fnode : O_Fnode;
   end record;

end Ortho_Code.Types;
