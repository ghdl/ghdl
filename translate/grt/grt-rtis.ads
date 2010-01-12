--  GHDL Run Time (GRT) -  Run Time Informations.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with System; use System;
with Grt.Types; use Grt.Types;
with Ada.Unchecked_Conversion;

package Grt.Rtis is
   pragma Preelaborate (Grt.Rtis);

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
      Ghdl_Rtik_For_Generate,
      Ghdl_Rtik_Instance, --10
      Ghdl_Rtik_Constant,
      Ghdl_Rtik_Iterator,
      Ghdl_Rtik_Variable,
      Ghdl_Rtik_Signal,
      Ghdl_Rtik_File, -- 15
      Ghdl_Rtik_Port,
      Ghdl_Rtik_Generic,
      Ghdl_Rtik_Alias,
      Ghdl_Rtik_Guard,
      Ghdl_Rtik_Component, -- 20
      Ghdl_Rtik_Attribute,
      Ghdl_Rtik_Type_B2,        --  Enum
      Ghdl_Rtik_Type_E8,
      Ghdl_Rtik_Type_E32,
      Ghdl_Rtik_Type_I32,       --  25 Scalar
      Ghdl_Rtik_Type_I64,
      Ghdl_Rtik_Type_F64,
      Ghdl_Rtik_Type_P32,
      Ghdl_Rtik_Type_P64,
      Ghdl_Rtik_Type_Access,
      Ghdl_Rtik_Type_Array,
      Ghdl_Rtik_Type_Record,
      Ghdl_Rtik_Type_File,
      Ghdl_Rtik_Subtype_Scalar,
      Ghdl_Rtik_Subtype_Array,
      Ghdl_Rtik_Subtype_Array_Ptr,
      Ghdl_Rtik_Subtype_Unconstrained_Array,
      Ghdl_Rtik_Subtype_Record,
      Ghdl_Rtik_Subtype_Access,
      Ghdl_Rtik_Type_Protected,
      Ghdl_Rtik_Element,
      Ghdl_Rtik_Unit,
      Ghdl_Rtik_Attribute_Transaction,
      Ghdl_Rtik_Attribute_Quiet,
      Ghdl_Rtik_Attribute_Stable,
      Ghdl_Rtik_Error);
   for Ghdl_Rtik'Size use 8;

   type Ghdl_Rti_Depth is range 0 .. 255;
   for Ghdl_Rti_Depth'Size use 8;

   type Ghdl_Rti_U8 is mod 2 ** 8;
   for Ghdl_Rti_U8'Size use 8;

   type Ghdl_Rti_Common is record
      Kind : Ghdl_Rtik;
      Depth : Ghdl_Rti_Depth;
      Mode : Ghdl_Rti_U8;
      Max_Depth : Ghdl_Rti_Depth;
   end record;

   type Ghdl_Rti_Access is access all Ghdl_Rti_Common;

   --  Fat array of rti accesses.
   type Ghdl_Rti_Array is array (Ghdl_Index_Type) of Ghdl_Rti_Access;
   type Ghdl_Rti_Arr_Acc is access Ghdl_Rti_Array;

   type Ghdl_Rti_Loc (Rel : Boolean := False) is record
      case Rel is
         when True =>
            Off : Ghdl_Index_Type;
         when False =>
            Addr : Address;
      end case;
   end record;
   pragma Unchecked_Union (Ghdl_Rti_Loc);

   type Ghdl_C_String_Array is array (Ghdl_Index_Type) of Ghdl_C_String;
   type Ghdl_C_String_Array_Ptr is access Ghdl_C_String_Array;

   type Ghdl_Rtin_Block is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Loc : Ghdl_Rti_Loc;
      Parent : Ghdl_Rti_Access;
      Size : Ghdl_Index_Type;
      Nbr_Child : Ghdl_Index_Type;
      Children : Ghdl_Rti_Arr_Acc;
   end record;
   type Ghdl_Rtin_Block_Acc is access Ghdl_Rtin_Block;
   function To_Ghdl_Rtin_Block_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Block_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Block_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Object is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Loc : Ghdl_Rti_Loc;
      Obj_Type : Ghdl_Rti_Access;
   end record;
   type Ghdl_Rtin_Object_Acc is access Ghdl_Rtin_Object;
   function To_Ghdl_Rtin_Object_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Object_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Object_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Instance is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Loc : Ghdl_Rti_Loc;
      Parent : Ghdl_Rti_Access;
      Instance : Ghdl_Rti_Access;
   end record;
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
   type Ghdl_Rtin_Component_Acc is access Ghdl_Rtin_Component;
   function To_Ghdl_Rtin_Component_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Component_Acc);

   type Ghdl_Rtin_Type_Enum is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Nbr : Ghdl_Index_Type;
      Names : Ghdl_C_String_Array_Ptr;
   end record;
   type Ghdl_Rtin_Type_Enum_Acc is access Ghdl_Rtin_Type_Enum;
   function To_Ghdl_Rtin_Type_Enum_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Enum_Acc);

   type Ghdl_Rtin_Type_Scalar is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
   end record;
   type Ghdl_Rtin_Type_Scalar_Acc is access Ghdl_Rtin_Type_Scalar;
   function To_Ghdl_Rtin_Type_Scalar_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Scalar_Acc);

   type Ghdl_Rtin_Subtype_Scalar is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Basetype : Ghdl_Rti_Access;
      Range_Loc : Ghdl_Rti_Loc;
   end record;
   type Ghdl_Rtin_Subtype_Scalar_Acc is access Ghdl_Rtin_Subtype_Scalar;
   function To_Ghdl_Rtin_Subtype_Scalar_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Subtype_Scalar_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Subtype_Scalar_Acc, Target => Ghdl_Rti_Access);

   --  True if the type is complex, set in Mode field.
   Ghdl_Rti_Type_Complex_Mask : constant Ghdl_Rti_U8 := 1;
   Ghdl_Rti_Type_Complex      : constant Ghdl_Rti_U8 := 1;

   type Ghdl_Rtin_Type_Array is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Element : Ghdl_Rti_Access;
      Nbr_Dim : Ghdl_Index_Type;
      Indexes : Ghdl_Rti_Arr_Acc;
   end record;
   type Ghdl_Rtin_Type_Array_Acc is access Ghdl_Rtin_Type_Array;
   function To_Ghdl_Rtin_Type_Array_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Array_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Type_Array_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Subtype_Array is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Basetype : Ghdl_Rtin_Type_Array_Acc;
      Bounds : Ghdl_Rti_Loc;
      Valsize : Ghdl_Rti_Loc;
      Sigsize : Ghdl_Rti_Loc;
   end record;
   type Ghdl_Rtin_Subtype_Array_Acc is access Ghdl_Rtin_Subtype_Array;
   function To_Ghdl_Rtin_Subtype_Array_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Subtype_Array_Acc);
   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rtin_Subtype_Array_Acc, Target => Ghdl_Rti_Access);

   type Ghdl_Rtin_Type_Fileacc is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Base : Ghdl_Rti_Access;
   end record;
   type Ghdl_Rtin_Type_Fileacc_Acc is access Ghdl_Rtin_Type_Fileacc;
   function To_Ghdl_Rtin_Type_Fileacc_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Fileacc_Acc);

   type Ghdl_Rtin_Element is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Eltype : Ghdl_Rti_Access;
      Val_Off : Ghdl_Index_Type;
      Sig_Off : Ghdl_Index_Type;
   end record;
   type Ghdl_Rtin_Element_Acc is access Ghdl_Rtin_Element;
   function To_Ghdl_Rtin_Element_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Element_Acc);

   type Ghdl_Rtin_Type_Record is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Nbrel : Ghdl_Index_Type;
      Elements : Ghdl_Rti_Arr_Acc;
   end record;
   type Ghdl_Rtin_Type_Record_Acc is access Ghdl_Rtin_Type_Record;
   function To_Ghdl_Rtin_Type_Record_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Type_Record_Acc);

   --  MODE is never used.  Refer to mode field of physical type.
   type Ghdl_Rti_Unit_Mode is (Unit_Mode_32, Unit_Mode_64, Unit_Mode_Addr);
   type Ghdl_Rti_Unit_Val (Mode : Ghdl_Rti_Unit_Mode := Unit_Mode_64) is record
      case Mode is
         when Unit_Mode_32 =>
            Unit_32 : Ghdl_I32;
         when Unit_Mode_64 =>
            Unit_64 : Ghdl_I64;
         when Unit_Mode_Addr =>
            Unit_Addr : Ghdl_Value_Ptr;
      end case;
   end record;
   pragma Unchecked_Union (Ghdl_Rti_Unit_Val);

   type Ghdl_Rtin_Unit is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Value : Ghdl_Rti_Unit_Val;
   end record;
   type Ghdl_Rtin_Unit_Acc is access Ghdl_Rtin_Unit;
   function To_Ghdl_Rtin_Unit_Acc is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Ghdl_Rtin_Unit_Acc);

   --  Mode field is set to 1 if units value is per address.  Otherwise,
   --  mode is 0.
   type Ghdl_Rtin_Type_Physical is record
      Common : Ghdl_Rti_Common;
      Name : Ghdl_C_String;
      Nbr : Ghdl_Index_Type;
      Units : Ghdl_Rti_Arr_Acc;
   end record;
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

   type Ghdl_Entity_Link_Acc is access Ghdl_Entity_Link_Type;

   function To_Ghdl_Entity_Link_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Entity_Link_Acc);

   type Ghdl_Component_Link_Type is record
      Instance : Ghdl_Entity_Link_Acc;
      Stmt : Ghdl_Rti_Access;
   end record;

   function To_Ghdl_Component_Link_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Component_Link_Acc);

   --  TOP rti.
   Ghdl_Rti_Top_Ptr : Ghdl_Rtin_Block_Acc;

   --  Address of the top instance.
   Ghdl_Rti_Top_Instance : Ghdl_Rti_Access;

   --  Instances have a pointer to their RTI at offset 0.
   type Ghdl_Rti_Acc_Acc is access Ghdl_Rti_Access;
   function To_Ghdl_Rti_Acc_Acc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Rti_Acc_Acc);

   function To_Address is new Ada.Unchecked_Conversion
     (Source => Ghdl_Rti_Access, Target => Address);

   function To_Ghdl_Rti_Access is new Ada.Unchecked_Conversion
     (Source => Address, Target => Ghdl_Rti_Access);

private
   pragma Export (C, Ghdl_Rti_Top_Ptr, "__ghdl_rti_top_ptr");
   pragma Export (C, Ghdl_Rti_Top_Instance, "__ghdl_rti_top_instance");
end Grt.Rtis;
