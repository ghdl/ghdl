--  GHDL Run Time (GRT) -  Run Time Informations.
--  Copyright (C) 2002 - 2020 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with System; use System;
with Grt.Types; use Grt.Types;
with Ada.Unchecked_Conversion;

package Grt.Rtis is
   pragma Preelaborate (Grt.Rtis);

   --  To keep in sync with:
   --   * trans-rtis.ads
   --   * grt.disp_rti.Disp_Kind
   type Ghdl_Rtik is
     (Ghdl_Rtik_Top,
      Ghdl_Rtik_Library,        -- use scalar
      Ghdl_Rtik_Package,
      Ghdl_Rtik_Package_Body,
      Ghdl_Rtik_Entity,

      Ghdl_Rtik_Architecture,
      Ghdl_Rtik_Process,
      Ghdl_Rtik_Block,
      Ghdl_Rtik_If_Generate,
      Ghdl_Rtik_Case_Generate,

      Ghdl_Rtik_For_Generate,   -- 10
      Ghdl_Rtik_Generate_Body,
      Ghdl_Rtik_Instance,
      Ghdl_Rtik_Constant,
      Ghdl_Rtik_Iterator,

      Ghdl_Rtik_Variable,
      Ghdl_Rtik_Signal,
      Ghdl_Rtik_File,
      Ghdl_Rtik_Port,
      Ghdl_Rtik_Generic,

      Ghdl_Rtik_Alias,          -- 20
      Ghdl_Rtik_Guard,
      Ghdl_Rtik_Component,
      Ghdl_Rtik_Attribute,
      Ghdl_Rtik_Type_B1,                      --  Rtin_Type_Enum

      Ghdl_Rtik_Type_E8,                      --  Rtin_Type_Enum
      Ghdl_Rtik_Type_E32,
      Ghdl_Rtik_Type_I32,                     --  Rtin_Type_Scalar
      Ghdl_Rtik_Type_I64,                     --  Rtin_Type_Scalar
      Ghdl_Rtik_Type_F64,                     --  Rtin_Type_Scalar

      Ghdl_Rtik_Type_P32,       -- 30         --  Rtin_Type_Physical
      Ghdl_Rtik_Type_P64,                     --  Rtin_Type_Physical
      Ghdl_Rtik_Type_Access,                  --  Rtin_Type_Fileacc
      Ghdl_Rtik_Type_Array,                   --  Rtin_Type_Array
      Ghdl_Rtik_Type_Record,                  --  Rtin_Type_Record

      Ghdl_Rtik_Type_Unbounded_Record,        --  Rtin_Type_Record
      Ghdl_Rtik_Type_File,                    --  Rtin_Type_Fileacc
      Ghdl_Rtik_Subtype_Scalar,               --  Rtin_Subtype_Scalar
      Ghdl_Rtik_Subtype_Array,                --  Rtin_Subtype_Composite
      Ghdl_Rtik_Subtype_Unbounded_Array,

      Ghdl_Rtik_Subtype_Record, -- 40         --  Rtin_Subtype_Composite
      Ghdl_Rtik_Subtype_Unbounded_Record,
      Ghdl_Rtik_Subtype_Access,               --  Rtin_Type_Fileacc
      Ghdl_Rtik_Type_Protected,
      Ghdl_Rtik_Element,

      Ghdl_Rtik_Unit64,
      Ghdl_Rtik_Unitptr,
      Ghdl_Rtik_Attribute_Transaction,
      Ghdl_Rtik_Attribute_Quiet,
      Ghdl_Rtik_Attribute_Stable,

      Ghdl_Rtik_Psl_Assert,
      Ghdl_Rtik_Psl_Assume,
      Ghdl_Rtik_Psl_Cover,
      Ghdl_Rtik_Psl_Endpoint,

      Ghdl_Rtik_Error);
   for Ghdl_Rtik'Size use 8;

   subtype Ghdl_Rtiks_Psl is
     Ghdl_Rtik range Ghdl_Rtik_Psl_Assert .. Ghdl_Rtik_Psl_Cover;

   type Ghdl_Rti_Depth is range 0 .. 255;
   for Ghdl_Rti_Depth'Size use 8;

   type Ghdl_Rti_U8 is mod 2 ** 8;
   for Ghdl_Rti_U8'Size use 8;

   --  This structure is common to all RTI nodes.
   type Ghdl_Rti_Common is record
      --  Kind of the RTI, list is above.
      Kind : Ghdl_Rtik;

      Depth : Ghdl_Rti_Depth;

      --  * array types and subtypes, record types, protected types:
      --    bit 0: set for complex type
      --    bit 1: set for anonymous type definition
      --    bit 2: set only for physical type with non-static units (time)
      --  * record elements:
      --    bit 0: set for complex type (copy of the type complex bit).
      --  * signals:
      --    bit 0-3: mode (1: linkage, 2: buffer, 3 : out, 4 : inout, 5: in)
      --    bit 4-5: kind (0 : none, 1 : register, 2 : bus)
      --    bit 6: set if has 'active attributes
      Mode : Ghdl_Rti_U8;

      --  * Types and subtypes definition:
      --    maximum depth of all RTIs referenced.
      --  * Others:
      --    0
      Max_Depth : Ghdl_Rti_Depth;
   end record;
   pragma Convention (C, Ghdl_Rti_Common);

   type Ghdl_Rti_Access is access all Ghdl_Rti_Common;

   --  Fat array of rti accesses.
   type Ghdl_Rti_Array is array (Ghdl_Index_Type) of Ghdl_Rti_Access;
   type Ghdl_Rti_Arr_Acc is access Ghdl_Rti_Array;

   subtype Ghdl_Rti_Loc is Integer_Address;
   Null_Rti_Loc : constant Ghdl_Rti_Loc := 0;

   type Ghdl_C_String_Array is array (Ghdl_Index_Type) of Ghdl_C_String;
   type Ghdl_C_String_Array_Ptr is access Ghdl_C_String_Array;

   type Ghdl_Rtin_Block is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      --  Address of the instantiation data relative to the parent's
      --  instantation data address.
      Loc : Ghdl_Rti_Loc;
      --  Line and column of the declaration.
      Linecol : Ghdl_Index_Type;
      Parent : Ghdl_Rti_Access;
      Nbr_Child : Ghdl_Index_Type;
      Children : Ghdl_Rti_Arr_Acc;
   end record;
   pragma Convention (C, Ghdl_Rtin_Block);
   type Ghdl_Rtin_Block_Acc is access Ghdl_Rtin_Block;
   function To_Ghdl_Rtin_Block_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Block_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Block_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Generate is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      --  Address of the instantiation data relative to the parent's
      --  instantation data address.
      Loc : Ghdl_Rti_Loc;
      --  Line and column of the declaration.
      Linecol : Ghdl_Index_Type;
      Parent : Ghdl_Rti_Access;
      --  The total size of the instantiation data for the contents of
      --  the generate statement. Useful to find the instantation data
      --  for a particular index of the loop. Only used for
      --  for_generate_statement.
      Size : Ghdl_Index_Type;
      Child : Ghdl_Rti_Access;
   end record;
   pragma Convention (C, Ghdl_Rtin_Generate);
   type Ghdl_Rtin_Generate_Acc is access Ghdl_Rtin_Generate;
   function To_Ghdl_Rtin_Generate_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Generate_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Generate_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Block_Filename is record
      Block : Ghdl_Rtin_Block;
      Filename : Ghdl_C_String;
   end record;
   pragma Convention (C, Ghdl_Rtin_Block_Filename);
   type Ghdl_Rtin_Block_Filename_Acc is access Ghdl_Rtin_Block_Filename;
   function To_Ghdl_Rtin_Block_Filename_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Block_Filename_Acc);

   type Ghdl_Rtin_Object is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      --  Address of the instantiation data relative to the parent's
      --  instantation data address. For a signal/port, this location
      --  contains the address of the signal. For a constant/generic
      --  this is the location of the value.
      Loc : Ghdl_Rti_Loc;
      --  Type of the object.
      Obj_Type : Ghdl_Rti_Access;
      --  Line and column of the declaration.
      Linecol : Ghdl_Index_Type;
   end record;
   pragma Convention (C, Ghdl_Rtin_Object);
   type Ghdl_Rtin_Object_Acc is access Ghdl_Rtin_Object;
   function To_Ghdl_Rtin_Object_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Object_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Object_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Psl_Directive is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      -- Location of the RTI data (count, state vector, state)
      Loc : Ghdl_Rti_Loc;
      Linecol : Ghdl_Index_Type;
      -- Parent architecture containing the PSL directive
      Parent : Ghdl_Rti_Access;
   end record;
   pragma Convention (C, Ghdl_Rtin_Psl_Directive);
   type Ghdl_Rtin_Psl_Directive_Acc is access Ghdl_Rtin_Psl_Directive;
   function To_Ghdl_Rtin_Psl_Directive_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Psl_Directive_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Psl_Directive_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Instance is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Linecol : Ghdl_Index_Type;
      Loc : Ghdl_Rti_Loc;
      Parent : Ghdl_Rti_Access;
      Instance : Ghdl_Rti_Access; --  Component or entity.
   end record;
   pragma Convention (C, Ghdl_Rtin_Instance);
   type Ghdl_Rtin_Instance_Acc is access Ghdl_Rtin_Instance;
   function To_Ghdl_Rtin_Instance_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Instance_Acc);

   --  Must be kept in sync with grt.types.mode_signal_type.
   Ghdl_Rti_Signal_Mode_Mask    : constant Ghdl_Rti_U8 := 15;
   Ghdl_Rti_Signal_Mode_None    : constant Ghdl_Rti_U8 := 0;
   Ghdl_Rti_Signal_Mode_Linkage : constant Ghdl_Rti_U8 := 1;
   Ghdl_Rti_Signal_Mode_Buffer  : constant Ghdl_Rti_U8 := 2;
   Ghdl_Rti_Signal_Mode_Out     : constant Ghdl_Rti_U8 := 3;
   Ghdl_Rti_Signal_Mode_Inout   : constant Ghdl_Rti_U8 := 4;
   Ghdl_Rti_Signal_Mode_In      : constant Ghdl_Rti_U8 := 5;

   Ghdl_Rti_Signal_Kind_Mask     : constant Ghdl_Rti_U8 := 3 * 16;
   Ghdl_Rti_Signal_Kind_Offset   : constant Ghdl_Rti_U8 := 1 * 16;
   Ghdl_Rti_Signal_Kind_No       : constant Ghdl_Rti_U8 := 0 * 16;
   Ghdl_Rti_Signal_Kind_Register : constant Ghdl_Rti_U8 := 1 * 16;
   Ghdl_Rti_Signal_Kind_Bus      : constant Ghdl_Rti_U8 := 2 * 16;

   Ghdl_Rti_Signal_Has_Active    : constant Ghdl_Rti_U8 := 64;

   type Ghdl_Rtin_Component is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Nbr_Child : Ghdl_Index_Type;
      Children : Ghdl_Rti_Arr_Acc;
   end record;
   pragma Convention (C, Ghdl_Rtin_Component);
   type Ghdl_Rtin_Component_Acc is access Ghdl_Rtin_Component;
   function To_Ghdl_Rtin_Component_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Component_Acc);

   type Ghdl_Rtin_Type_Enum is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Nbr : Ghdl_Index_Type;
      --  Characters are represented as 'X', identifiers are represented as is,
      --  extended identifiers are represented as is too.
      Names : Ghdl_C_String_Array_Ptr;
   end record;
   pragma Convention (C, Ghdl_Rtin_Type_Enum);
   type Ghdl_Rtin_Type_Enum_Acc is access Ghdl_Rtin_Type_Enum;
   function To_Ghdl_Rtin_Type_Enum_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Enum_Acc);

   type Ghdl_Rtin_Type_Scalar is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
   end record;
   pragma Convention (C, Ghdl_Rtin_Type_Scalar);
   type Ghdl_Rtin_Type_Scalar_Acc is access Ghdl_Rtin_Type_Scalar;
   function To_Ghdl_Rtin_Type_Scalar_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Scalar_Acc);

   type Ghdl_Rtin_Subtype_Scalar is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Basetype : Ghdl_Rti_Access;
      --  Address of the instantiation data relative to the parent's
      --  instantation data address.  The instantiation data in this
      --  case is the range of the scalar.
      Range_Loc : Ghdl_Rti_Loc;
   end record;
   pragma Convention (C, Ghdl_Rtin_Subtype_Scalar);
   type Ghdl_Rtin_Subtype_Scalar_Acc is access Ghdl_Rtin_Subtype_Scalar;
   function To_Ghdl_Rtin_Subtype_Scalar_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Subtype_Scalar_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Subtype_Scalar_Acc, Target => Ghdl_Rti_Access);

   --  True if the type is complex, set in Mode field.
   Ghdl_Rti_Type_Complex_Mask : constant Ghdl_Rti_U8 := 1;
   Ghdl_Rti_Type_Complex      : constant Ghdl_Rti_U8 := 1;

   --  True if the type is anonymous
   Ghdl_Rti_Type_Anonymous_Mask : constant Ghdl_Rti_U8 := 2;
   Ghdl_Rti_Type_Anonymous      : constant Ghdl_Rti_U8 := 2;

   type Ghdl_Rtin_Type_Array is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Element : Ghdl_Rti_Access;
      --  The number of dimension of the array.
      --  (e.g. array(1 downto 0, 2 downto 0, 3 downto 0) has 3 dimensions)
      Nbr_Dim : Ghdl_Index_Type;
      --  An array of types, one for each range of the dimensions.
      Indexes : Ghdl_Rti_Arr_Acc;
      --  Note: An array has no `Loc` field so it has no instantiation
      --  data.  A consequence of this is that `Element` must be
      --  fully constrained, since there is nowhere to store the
      --  constraints applied by this type.
   end record;
   pragma Convention (C, Ghdl_Rtin_Type_Array);
   type Ghdl_Rtin_Type_Array_Acc is access Ghdl_Rtin_Type_Array;
   function To_Ghdl_Rtin_Type_Array_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Array_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Type_Array_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Subtype_Composite is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Basetype : Ghdl_Rti_Access;
      --  Address of the instantiation data relative to the parent's
      --  instantation data address. For an array the instantiation
      --  data would constain the element sizes, and the bounds. For a
      --  record the instantiation data consists of the element
      --  instantiation data.
      Layout : Ghdl_Rti_Loc;
   end record;
   pragma Convention (C, Ghdl_Rtin_Subtype_Composite);
   type Ghdl_Rtin_Subtype_Composite_Acc is access Ghdl_Rtin_Subtype_Composite;
   function To_Ghdl_Rtin_Subtype_Composite_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Subtype_Composite_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Subtype_Composite_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Type_Fileacc is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Base : Ghdl_Rti_Access;
   end record;
   pragma Convention (C, Ghdl_Rtin_Type_Fileacc);
   type Ghdl_Rtin_Type_Fileacc_Acc is access Ghdl_Rtin_Type_Fileacc;
   function To_Ghdl_Rtin_Type_Fileacc_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Fileacc_Acc);

   --  A static element is one in which the data structure does not depend
   --  on the context.
   Ghdl_Rti_Element_Static : constant Ghdl_Rti_U8 := 0;
   --  A complex element is one in which the data structure can depend
   --  on the context. For example:
   --  * An array with a size given by a generic parameter.
   --  * A record containing an array with a size given by a for-generate
   --    loop variable.
   Ghdl_Rti_Element_Complex : constant Ghdl_Rti_U8 := 1;
   --  The element is unbounded. (i.e. The bounds must be specified by
   --  a subtype of the record).
   Ghdl_Rti_Element_Unbounded : constant Ghdl_Rti_U8 := 2;

   --  Whether an element is static, complex or unbounded is set in the
   --  `Common.Mode` field.
   --  This could also be extracted from the element type.

   type Ghdl_Rtin_Element is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Eltype : Ghdl_Rti_Access;
      --  `Val_Off` and `Sig_Off` are used to find the object data
      --  corresponding to this element. If the object is a
      --  constant/generic use the `Val_Off` offset. If the object is
      --  a signal/port use the `Sig_Off` offset.
      --  For a static element the offset is the position of
      --  the element instantiation data within the record's instantiation
      --  data.
      --  For a complex element the size of the instantiation data was
      --  unknown at compile time, so the location given my the offset
      --  instead contains the address of the element's instantiation
      --  data.
      Val_Off : Ghdl_Index_Type;
      Sig_Off : Ghdl_Index_Type;
      --  If the element has an unbounded type, then we need to store
      --  information about the bounds/layout of that type somewhere.
      --  `Layout_Off` gives the location of these bounds/layout within
      --  the instantiation data.
      Layout_Off : Ghdl_Index_Type;
   end record;
   pragma Convention (C, Ghdl_Rtin_Element);
   type Ghdl_Rtin_Element_Acc is access Ghdl_Rtin_Element;
   function To_Ghdl_Rtin_Element_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Element_Acc);

   type Ghdl_Rtin_Type_Record is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Nbrel : Ghdl_Index_Type;
      Elements : Ghdl_Rti_Arr_Acc;
      --  Address of the instantiation data relative to the parent's
      --  instantation data address. A record only needs instantiation
      --  data if it is complex.  Otherwise all the layout information
      --  is stored directly in the element RTI structures.
      Layout : Ghdl_Rti_Loc;
   end record;
   pragma Convention (C, Ghdl_Rtin_Type_Record);
   type Ghdl_Rtin_Type_Record_Acc is access Ghdl_Rtin_Type_Record;
   function To_Ghdl_Rtin_Type_Record_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Record_Acc);

   type Ghdl_Rtin_Unit64 is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Value : Ghdl_I64;
   end record;
   pragma Convention (C, Ghdl_Rtin_Unit64);
   type Ghdl_Rtin_Unit64_Acc is access Ghdl_Rtin_Unit64;
   function To_Ghdl_Rtin_Unit64_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Unit64_Acc);

   type Ghdl_Rtin_Unitptr is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Addr : Ghdl_Value_Ptr;
   end record;
   pragma Convention (C, Ghdl_Rtin_Unitptr);
   type Ghdl_Rtin_Unitptr_Acc is access Ghdl_Rtin_Unitptr;
   function To_Ghdl_Rtin_Unitptr_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Unitptr_Acc);

   --  Mode field is set to 4 if units value is per address.  Otherwise,
   --  mode is 0.
   type Ghdl_Rtin_Type_Physical is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Nbr : Ghdl_Index_Type;
      Units : Ghdl_Rti_Arr_Acc;
   end record;
   pragma Convention (C, Ghdl_Rtin_Type_Physical);
   type Ghdl_Rtin_Type_Physical_Acc is access Ghdl_Rtin_Type_Physical;
   function To_Ghdl_Rtin_Type_Physical_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Physical_Acc);

   --  Instance linkage.

   --  At the beginning of a component structure (or the object for a direct
   --  instantiation), there is a Ghdl_Component_Link_Type record.
   --  These record contains a pointer to the instance (down link),
   --  and RTIS to the statement and its parent (up link).
   type Ghdl_Component_Link_Type;
   type Ghdl_Component_Link_Acc is access Ghdl_Component_Link_Type;

   --  At the beginning of an entity structure, there is a Ghdl_Link_Type,
   --  which contains the RTI for the architecture (down-link) and a pointer
   --  to the instantiation object (up-link).
   type Ghdl_Entity_Link_Type is record
      Rti : Ghdl_Rti_Access;
      Parent : Ghdl_Component_Link_Acc;
   end record;
   pragma Convention (C, Ghdl_Entity_Link_Type);

   type Ghdl_Entity_Link_Acc is access Ghdl_Entity_Link_Type;

   function To_Ghdl_Entity_Link_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Entity_Link_Acc);

   type Ghdl_Component_Link_Type is record
      Instance : Ghdl_Entity_Link_Acc;
      Stmt : Ghdl_Rti_Access;
   end record;
   pragma Convention (C, Ghdl_Component_Link_Type);

   function To_Ghdl_Component_Link_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Component_Link_Acc);

   --  TOP rti.
   Ghdl_Rti_Top : Ghdl_Rtin_Block :=
     (Common => (Ghdl_Rtik_Top, 0, 0, 0),
      Name => null,
      Loc => Null_Rti_Loc,
      Linecol => 0,
      Parent => null,
      Nbr_Child => 0,
      Children => null);

   --  Address of the top instance.
   Ghdl_Rti_Top_Instance : Address;
   pragma Export (C, Ghdl_Rti_Top_Instance, "__ghdl_rti_top_instance");

   --  Instances have a pointer to their RTI at offset 0.
   type Ghdl_Rti_Acc_Acc is access Ghdl_Rti_Access;
   function To_Ghdl_Rti_Acc_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Rti_Acc_Acc);

   function To_Address is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Address);

   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Rti_Access);

   procedure Ghdl_Rti_Add_Top (Max_Pkg : Ghdl_Index_Type;
                               Pkgs : Ghdl_Rti_Arr_Acc;
                               Top : Ghdl_Rti_Access;
                               Instance : Address);
   pragma Export (C, Ghdl_Rti_Add_Top, "__ghdl_rti_add_top");

   --  Register a package
   procedure Ghdl_Rti_Add_Package (Pkg : Ghdl_Rti_Access);
   pragma Export (C, Ghdl_Rti_Add_Package, "__ghdl_rti_add_package");
end Grt.Rtis;
